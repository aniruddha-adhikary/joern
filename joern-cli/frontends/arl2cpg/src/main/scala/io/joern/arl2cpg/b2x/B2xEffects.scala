package io.joern.arl2cpg.b2x

import io.joern.arl2cpg.parser.{ARLLexer, ARLParser}
import org.antlr.v4.runtime.atn.PredictionMode
import org.antlr.v4.runtime.tree.{ParseTree, TerminalNode}
import org.antlr.v4.runtime.misc.ParseCancellationException
import org.antlr.v4.runtime.{
  BailErrorStrategy,
  BaseErrorListener,
  CharStreams,
  CommonTokenStream,
  RecognitionException,
  Recognizer,
  Token
}

import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.Try

/** What a B2X body does to the object it is invoked on. Ported from arlgraph `B2xEffects.java` / `ExprFacts`.
  *
  * Every path is relative to `this`; the call site substitutes its receiver, so `this.rejected` written by `rejectWith`
  * becomes `outcome.rejected` at `outcome.rejectWith(...)`.
  *
  * `opaque` is the honest remainder: methods called on `this` whose own body is nowhere in the mapping, and
  * non-accessor methods called on a field of `this` (`child.modify`), which may mutate that field's object. Resolving
  * one hop and then claiming the call is understood would be worse than not resolving it at all, so the caller keeps an
  * unresolved edge naming exactly what is still unknown.
  *
  * Accessor-shaped calls (`setX`/`getX`/`isX`) count as an inferred write/read of `x`; if the mapping also holds a body
  * for the accessor, that body is followed too, so a setter that touches more than its property is not concealed.
  */
final class B2xEffects private {
  val writes: mutable.LinkedHashSet[String]         = mutable.LinkedHashSet.empty
  val inferredWrites: mutable.LinkedHashSet[String] = mutable.LinkedHashSet.empty
  val reads: mutable.LinkedHashSet[String]          = mutable.LinkedHashSet.empty
  val inferredReads: mutable.LinkedHashSet[String]  = mutable.LinkedHashSet.empty
  val opaque: mutable.LinkedHashSet[String]         = mutable.LinkedHashSet.empty

  /** Bodies that went into this result, outermost first. */
  val resolvedThrough: mutable.ListBuffer[B2xMember] = mutable.ListBuffer.empty

  private val inProgress: mutable.Set[B2xMember] = mutable.Set.empty

  private def absorb(member: B2xMember, b2x: B2xModel, depth: Int): Boolean = {
    if (depth >= B2xEffects.MaxDepth || inProgress.contains(member)) {
      opaque += member.name
      return true
    }
    B2xEffects.parseBody(member.body) match {
      case None        => false
      case Some(block) =>
        inProgress += member
        if (!resolvedThrough.contains(member)) resolvedThrough += member
        val facts = new B2xEffects.BodyFacts
        facts.walk(block, lhs = false)
        facts.reads --= facts.calls
        writes ++= facts.writes.flatMap(B2xEffects.onThis)
        inferredWrites ++= facts.inferredWrites.flatMap(B2xEffects.onThis)
        reads ++= facts.reads.flatMap(B2xEffects.onThis)
        inferredReads ++= facts.inferredReads.flatMap(B2xEffects.onThis)
        facts.callsOnThis.foreach {
          case ("this", name, arity) =>
            b2x.candidates(member.businessClass, name, arity) match {
              case single :: Nil if absorb(single, b2x, depth + 1) =>
              case Nil if B2xEffects.isAccessorShaped(name)        =>
              case _                                               => opaque += name
            }
          case (receiverPath, name, _) if !B2xEffects.isAccessorShaped(name) =>
            B2xEffects.onThis(receiverPath).foreach(path => opaque += s"$path.$name")
          case _ =>
        }
        inProgress -= member
        true
    }
  }
}

object B2xEffects {

  /** Depth cap for B2X bodies calling other B2X bodies; also breaks accidental cycles. */
  val MaxDepth = 4

  /** Analyses `member`, following calls into other B2X bodies. None if the body is not ARL or does not parse — the
    * caller must then leave the call unresolved rather than assume it is inert.
    */
  def of(member: B2xMember, b2x: B2xModel): Option[B2xEffects] = {
    if (!member.isArl) None
    else {
      val effects = new B2xEffects
      if (effects.absorb(member, b2x, 0)) Some(effects) else None
    }
  }

  private def onThis(path: String): Option[String] =
    if (path.startsWith("this.")) Some(path.stripPrefix("this.")) else None

  private[b2x] def isAccessorShaped(name: String): Boolean =
    accessorProperty(name, "set").isDefined || accessorProperty(name, "get").isDefined ||
      accessorProperty(name, "is").isDefined

  /** `setFoo` -> `foo` for the given prefix; None when the name is not accessor-shaped. */
  private[b2x] def accessorProperty(name: String, prefix: String): Option[String] =
    if (name.startsWith(prefix) && name.length > prefix.length && name.charAt(prefix.length).isUpper)
      Some(name.charAt(prefix.length).toLower.toString + name.substring(prefix.length + 1))
    else None

  /** Bodies are bare statements: wrapped in braces and parsed as a block, SLL with bail — a body that does not parse is
    * "we cannot read this one", not something to retry.
    */
  private[b2x] def parseBody(body: String): Option[ARLParser.BlockContext] = Try {
    val lexer = new ARLLexer(CharStreams.fromString(s"{\n$body\n}"))
    lexer.removeErrorListeners()
    lexer.addErrorListener(BailLexerErrorListener)
    val parser = new ARLParser(new CommonTokenStream(lexer))
    parser.removeErrorListeners()
    parser.getInterpreter.setPredictionMode(PredictionMode.SLL)
    parser.setErrorHandler(new BailErrorStrategy())
    val block = parser.block()
    if (parser.getCurrentToken.getType != Token.EOF) throw new ParseCancellationException("trailing input")
    block
  }.toOption

  private object BailLexerErrorListener extends BaseErrorListener {
    override def syntaxError(
      recognizer: Recognizer[?, ?],
      offendingSymbol: Any,
      line: Int,
      charPositionInLine: Int,
      msg: String,
      e: RecognitionException
    ): Unit = throw new ParseCancellationException(msg)
  }

  /** Syntactic facts of one body: written paths, read paths, accessor-shaped inferred effects and calls on `this`. */
  private[b2x] class BodyFacts {
    val writes: mutable.LinkedHashSet[String]         = mutable.LinkedHashSet.empty
    val inferredWrites: mutable.LinkedHashSet[String] = mutable.LinkedHashSet.empty
    val reads: mutable.LinkedHashSet[String]          = mutable.LinkedHashSet.empty
    val inferredReads: mutable.LinkedHashSet[String]  = mutable.LinkedHashSet.empty
    val calls: mutable.LinkedHashSet[String]          = mutable.LinkedHashSet.empty

    /** (receiver path, name, arity) of every call whose receiver is `this` or a path hanging off it (`this.a.b`). */
    val callsOnThis: mutable.ListBuffer[(String, String, Int)] = mutable.ListBuffer.empty

    def walk(node: ParseTree, lhs: Boolean): Unit = {
      node match {
        case a: ARLParser.AssignmentContext if a.assignment() != null =>
          pathOf(a.ternary()).foreach(writes += _)
          walk(a.ternary(), lhs = true)
          walk(a.assignment(), lhs = false)
          return
        case p: ARLParser.PostfixContext =>
          val path = pathOf(p)
          if (!lhs) path.foreach(reads += _)
          recordCalls(p)
        case _ =>
      }
      (0 until node.getChildCount).foreach(i => walk(node.getChild(i), lhs && i == 0))
    }

    /** The dotted path a call-free postfix names (`this.a.b`), None if any segment is a call, subscript or value. */
    private def pathOf(node: ParseTree): Option[String] = node match {
      case p: ARLParser.PostfixContext =>
        val primary  = p.primary()
        val base     = primaryPath(primary)
        val suffixes = children(p).drop(1).map {
          case s: ARLParser.SelectorContext if s.arguments() == null => Some(selectorName(s))
          case _: TerminalNode                                       => Some("")
          case _                                                     => None
        }
        if (base.isEmpty || suffixes.exists(_.isEmpty)) None
        else Some((base.get :: suffixes.flatten.filter(_.nonEmpty)).mkString("."))
      case ctx if ctx.getChildCount == 1 => pathOf(ctx.getChild(0))
      case _                             => None
    }

    private def primaryPath(primary: ARLParser.PrimaryContext): Option[String] =
      if (primary == null || primary.arguments() != null || primary.creator() != null || primary.literal() != null)
        None
      else if (primary.qualifiedName() != null) Some(primary.qualifiedName().getText.replace("`", ""))
      else if (primary.getChildCount == 1 && primary.getChild(0).getText == "this") Some("this")
      else None

    private def children(p: ParseTree): List[ParseTree] =
      (0 until p.getChildCount).map(p.getChild).toList

    private def selectorName(s: ARLParser.SelectorContext): String = s.getChild(0).getText.replace("`", "")

    private def arityOf(args: ARLParser.ArgumentsContext): Int =
      Option(args.expressionList()).map(_.expression().size()).getOrElse(0)

    private def recordCalls(p: ARLParser.PostfixContext): Unit = {
      val primary = p.primary()
      if (primary == null) return
      var prefix: Option[String] = primaryPath(primary)
      if (primary.arguments() != null && primary.qualifiedName() != null) {
        calls += primary.qualifiedName().getText.replace("`", "")
        prefix = None
      }
      children(p).drop(1).foreach {
        case s: ARLParser.SelectorContext =>
          val name = selectorName(s)
          if (s.arguments() != null) {
            val arity = arityOf(s.arguments())
            prefix.foreach(receiver => calls += s"$receiver.$name")
            prefix.filter(r => r == "this" || r.startsWith("this.")).foreach { receiver =>
              callsOnThis += ((receiver, name, arity))
              accessorProperty(name, "set").foreach(prop => inferredWrites += s"$receiver.$prop")
              accessorProperty(name, "get").orElse(accessorProperty(name, "is")).foreach { prop =>
                inferredReads += s"$receiver.$prop"
              }
            }
            prefix = None
          } else {
            prefix = prefix.map(receiver => s"$receiver.$name")
          }
        case _: ARLParser.ArrayAccessContext => prefix = None
        case _                               =>
      }
    }
  }
}
