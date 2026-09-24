package io.joern.arl2cpg.astcreation

import io.joern.arl2cpg.ArlOperators
import io.joern.arl2cpg.parser.ARLParser
import io.joern.x2cpg.{Ast, Defines}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, DispatchTypes, Operators}
import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.tree.TerminalNode

import scala.jdk.CollectionConverters.*

/** Layer C — Java-forked expressions: operator chains, qualified names, calls, creators, intervals, literals. */
trait AstForExpressions {
  this: AstCreator =>

  protected def astForExpression(ctx: ParserRuleContext): Ast = ctx match {
    case e: ARLParser.ExpressionContext => astForExpression(e.assignment())
    case e: ARLParser.AssignmentContext => astForAssignment(e)
    case e: ARLParser.TernaryContext    => astForTernary(e)
    case e: ARLParser.LogicalOrContext  => astForBinaryChain(e, e.logicalAnd().asScala.toList, childrenOf(e))
    case e: ARLParser.LogicalAndContext => astForBinaryChain(e, e.equality().asScala.toList, childrenOf(e))
    case e: ARLParser.EqualityContext   => astForBinaryChain(e, e.relational().asScala.toList, childrenOf(e))
    case e: ARLParser.RelationalContext => astForRelational(e)
    case e: ARLParser.AdditiveContext   =>
      astForBinaryChain(e, e.multiplicative().asScala.toList, childrenOf(e))
    case e: ARLParser.MultiplicativeContext  => astForBinaryChain(e, e.unary().asScala.toList, childrenOf(e))
    case e: ARLParser.UnaryContext           => astForUnary(e)
    case e: ARLParser.CastExprContext        => astForCast(e)
    case e: ARLParser.PostfixContext         => astForPostfix(e)
    case e: ARLParser.PrimaryContext         => astForPrimary(e)
    case e: ARLParser.IntervalLiteralContext => astForInterval(e)
    case e: ARLParser.LiteralContext         => astForLiteral(e)
    case _                                   => unknownAst(ctx)
  }

  // ------------------------------------------------------------------
  // assignment / ternary
  // ------------------------------------------------------------------

  private def astForAssignment(ctx: ARLParser.AssignmentContext): Ast = {
    val lhs = astForExpression(ctx.ternary())
    Option(ctx.assignment()) match {
      case Some(rhsCtx) =>
        val opText = childrenOf(ctx).collectFirst { case t: TerminalNode => t.getText }.getOrElse("=")
        val op     = opText match {
          case "="  => Operators.assignment
          case "+=" => Operators.assignmentPlus
          case "-=" => Operators.assignmentMinus
          case "*=" => Operators.assignmentMultiplication
          case "/=" => Operators.assignmentDivision
          case _    => Operators.assignment
        }
        val rhs  = astForExpression(rhsCtx)
        val call = operatorCallNode(ctx, code(ctx), op, Option(Defines.Any))
        callAst(call, Seq(lhs, rhs))
      case None => lhs
    }
  }

  private def astForTernary(ctx: ARLParser.TernaryContext): Ast = {
    val cond  = astForExpression(ctx.logicalOr())
    val exprs = ctx.expression().asScala.toList
    if (exprs.size == 2) {
      val call = operatorCallNode(ctx, code(ctx), Operators.conditional, Option(Defines.Any))
      callAst(call, Seq(cond, astForExpression(exprs(0)), astForExpression(exprs(1))))
    } else cond
  }

  // ------------------------------------------------------------------
  // binary chains
  // ------------------------------------------------------------------

  private def binaryOp(opToken: String): String = opToken match {
    case "||"         => Operators.logicalOr
    case "&&"         => Operators.logicalAnd
    case "=="         => Operators.equals
    case "!="         => Operators.notEquals
    case "<"          => Operators.lessThan
    case ">"          => Operators.greaterThan
    case "<="         => Operators.lessEqualsThan
    case ">="         => Operators.greaterEqualsThan
    case "+"          => Operators.plus
    case "-"          => Operators.minus
    case "*"          => Operators.multiplication
    case "/"          => Operators.division
    case "%"          => Operators.modulo
    case "instanceof" => Operators.instanceOf
    case _            => opToken
  }

  private def astForBinaryChain(ctx: ParserRuleContext, operands: List[ParserRuleContext], children: List[Any]): Ast = {
    if (operands.size == 1) return astForExpression(operands.head)
    val opTokens = childrenOf(ctx).collect { case t: TerminalNode => t.getText }
    operands.tail.zip(opTokens).foldLeft(astForExpression(operands.head)) { (acc, pair) =>
      val (operandCtx, opTok) = pair
      val rhs                 = astForExpression(operandCtx)
      val call                =
        operatorCallNode(ctx, s"${rootCode(acc)} $opTok ${rootCode(rhs)}", binaryOp(opTok), Option(Defines.Any))
      callAst(call, Seq(acc, rhs))
    }
  }

  /** Relational tier additionally admits `instanceof` and `as` (cast). */
  private def astForRelational(ctx: ARLParser.RelationalContext): Ast = {
    val operands = ctx.additive().asScala.toList
    if (operands.size == 1) return astForExpression(operands.head)
    val opTokens = childrenOf(ctx).collect { case t: TerminalNode => t.getText }
    operands.tail.zip(opTokens).foldLeft(astForExpression(operands.head)) { (acc, pair) =>
      val (operandCtx, opTok) = pair
      opTok match {
        case "instanceof" =>
          // right side is a type written as an additive; keep its text as TYPE_REF
          val typeName = resolveTypeName(operandCtx.getText)
          val typeRef  = Ast(typeRefNode(operandCtx, operandCtx.getText, typeName))
          val call     = operatorCallNode(ctx, code(ctx), Operators.instanceOf, Option("boolean"))
          callAst(call, Seq(acc, typeRef))
        case "as" =>
          astForCastLike(ctx, acc, operandCtx.getText)
        case _ =>
          val rhs  = astForExpression(operandCtx)
          val call =
            operatorCallNode(ctx, s"${rootCode(acc)} $opTok ${rootCode(rhs)}", binaryOp(opTok), Option(Defines.Any))
          callAst(call, Seq(acc, rhs))
      }
    }
  }

  // ------------------------------------------------------------------
  // unary / cast / postfix
  // ------------------------------------------------------------------

  private def astForUnary(ctx: ARLParser.UnaryContext): Ast = {
    childrenOf(ctx).headOption match {
      case Some(t: TerminalNode) =>
        val op = t.getText match {
          case "!" => Operators.logicalNot
          case "-" => Operators.minus
          case "+" => Operators.plus
          case _   => t.getText
        }
        val operand = astForExpression(ctx.unary())
        val call    = operatorCallNode(ctx, code(ctx), op, Option(Defines.Any))
        callAst(call, Seq(operand))
      case Some(c: ARLParser.CastExprContext) => astForCast(c)
      case Some(p: ARLParser.PostfixContext)  => astForPostfix(p)
      case _                                  => unknownAst(ctx)
    }
  }

  private def astForCast(ctx: ARLParser.CastExprContext): Ast = {
    val operand = astForExpression(ctx.unary())
    astForCastLike(ctx, operand, ctx.`type`().getText)
  }

  /** `<operator>.cast(TYPE_REF T, e)`. */
  private def astForCastLike(ctx: ParserRuleContext, operand: Ast, rawTypeName: String): Ast = {
    val typeName = resolveTypeName(rawTypeName)
    val typeRef  = Ast(typeRefNode(ctx, rawTypeName, typeName))
    val call     = operatorCallNode(ctx, s"($rawTypeName) ${rootCode(operand)}", Operators.cast, Option(typeName))
    callAst(call, Seq(typeRef, operand))
  }

  /** postfix: primary ( '.' selector | '[' expr ']' )* */
  private def astForPostfix(ctx: ARLParser.PostfixContext): Ast = {
    val primaryCtx = ctx.primary()
    var acc        = astForPrimary(primaryCtx)
    // Suffixes after the primary: selector or arrayAccess children interleaved with '.' tokens.
    childrenOf(ctx).drop(1).foreach {
      case t: TerminalNode              => () // '.' separators
      case s: ARLParser.SelectorContext =>
        acc = astForSelector(s, acc)
      case a: ARLParser.ArrayAccessContext =>
        val index = astForExpression(a.expression())
        val call  = operatorCallNode(a, code(a), Operators.indexAccess, Option(Defines.Any))
        acc = callAst(call, Seq(acc, index))
      case _ => ()
    }
    acc
  }

  /** `.id` / `.id(args)` / `.`id`` / `.this` after a base expression. */
  private def astForSelector(ctx: ARLParser.SelectorContext, base: Ast): Ast = {
    val nameText = childrenOf(ctx)
      .collectFirst {
        case i: ARLParser.IdContext => i.getText
        case t: TerminalNode        => t.getText
      }
      .map(stripBackticks)
      .getOrElse(ctx.getText)
    Option(ctx.arguments()) match {
      case Some(argsCtx) =>
        val argAsts     = argsForArguments(argsCtx)
        val receiverTyp = rootType(base)
        val ns   = if (receiverTyp == Defines.Any || receiverTyp.isEmpty) Defines.UnresolvedNamespace else receiverTyp
        val call = callNode(
          ctx,
          s"${rootCode(base)}.$nameText(${argAsts.map(rootCode).mkString(", ")})",
          nameText,
          unresolvedMethodFullName(ns, nameText, argAsts.size + 1),
          DispatchTypes.DYNAMIC_DISPATCH,
          Option(s"${Defines.UnresolvedSignature}(${argAsts.size + 1})"),
          Option(Defines.Any)
        )
        callAst(call, argAsts, base = Option(base))
      case None =>
        fieldAccessAst(ctx, ctx, base, s"${rootCode(base)}.$nameText", nameText, Defines.Any)
    }
  }

  private def argsForArguments(ctx: ARLParser.ArgumentsContext): List[Ast] =
    Option(ctx.expressionList()).toList.flatMap(_.expression().asScala.toList.map(astForExpression))

  // ------------------------------------------------------------------
  // primary
  // ------------------------------------------------------------------

  private def astForPrimary(ctx: ARLParser.PrimaryContext): Ast = {
    childrenOf(ctx).headOption match {
      case Some(l: ARLParser.LiteralContext)         => astForLiteral(l)
      case Some(i: ARLParser.IntervalLiteralContext) => astForInterval(i)
      case Some(t: TerminalNode)                     =>
        t.getText match {
          case "new"                        => astForCreator(ctx.creator())
          case "this"                       => astForThis(ctx)
          case "("                          => astForExpression(ctx.expression())
          case text if text.startsWith("$") => boundIdentifierAst(ctx, text, text, Defines.Any)
          case _                            => unknownAst(ctx)
        }
      case Some(q: ARLParser.QualifiedNameContext) => astForQualifiedNameExpr(q, Option(ctx.arguments()))
      case _                                       => unknownAst(ctx)
    }
  }

  private def astForThis(ctx: ParserRuleContext): Ast = {
    implicitReceiver.top match {
      case Some((bindingName, bindingType)) =>
        boundIdentifierAst(ctx, bindingName, "this", bindingType)
      case None =>
        thisIdentifierAst(ctx, thisParamType)
    }
  }

  /** qualifiedName [arguments] — the core name-resolution entry point. */
  private def astForQualifiedNameExpr(
    qn: ARLParser.QualifiedNameContext,
    argsCtx: Option[ARLParser.ArgumentsContext]
  ): Ast = {
    val segments = qualifiedNameSegments(qn)
    astForNameSegments(
      qn,
      segments,
      argsCtx.map(argsForArguments).getOrElse(List.empty),
      argsCtx.isDefined,
      code(qn) + argsCtx.map(args => code(args)).getOrElse("")
    )
  }

  /** Resolves `seg(.seg)*` with optional trailing args following the brief's resolution order. */
  private def astForNameSegments(
    ctx: ParserRuleContext,
    segments: List[String],
    argAsts: List[Ast],
    hasArgs: Boolean,
    fullCode: String
  ): Ast = {
    val first = segments.headOption.getOrElse("")
    val rest  = segments.drop(1)

    def dynamicSuffix(base: Ast, suffixes: List[String], trailingArgs: List[Ast], hasTrailingArgs: Boolean): Ast = {
      var acc = base
      suffixes.zipWithIndex.foreach { case (seg, idx) =>
        val isLast = idx == suffixes.size - 1
        if (isLast && hasTrailingArgs) {
          val receiverTyp = rootType(acc)
          val ns   = if (receiverTyp == Defines.Any || receiverTyp.isEmpty) Defines.UnresolvedNamespace else receiverTyp
          val call = callNode(
            ctx,
            fullCode,
            seg,
            unresolvedMethodFullName(ns, seg, trailingArgs.size + 1),
            DispatchTypes.DYNAMIC_DISPATCH,
            Option(s"${Defines.UnresolvedSignature}(${trailingArgs.size + 1})"),
            Option(Defines.Any)
          )
          acc = callAst(call, trailingArgs, base = Option(acc))
        } else {
          acc = fieldAccessAst(ctx, ctx, acc, s"${rootCode(acc)}.$seg", seg, Defines.Any)
        }
      }
      acc
    }

    if (isBoundValue(first) || implicitReceiver.top.exists { case (n, _) => n == first }) {
      // binding / local / task param
      val tpe =
        if (isBoundValue(first)) boundValueType(first) else implicitReceiver.top.map(_._2).getOrElse(Defines.Any)
      val base = boundIdentifierAst(ctx, first, first, tpe)
      rest match {
        case Nil if hasArgs =>
          // calling a bound name as a function: e.g. `b(...)` — treat as dynamic call on it
          val call = callNode(
            ctx,
            fullCode,
            first,
            unresolvedMethodFullName(Defines.UnresolvedNamespace, first, argAsts.size),
            DispatchTypes.STATIC_DISPATCH,
            Option(s"${Defines.UnresolvedSignature}(${argAsts.size})"),
            Option(Defines.Any)
          )
          callAst(call, argAsts)
        case Nil => base
        case _   => dynamicSuffix(base, rest, argAsts, hasArgs)
      }
    } else if (signatureMembers.contains(first)) {
      val base = signatureParamFieldAccessAst(ctx, first)
      dynamicSuffix(base, rest, argAsts, hasArgs)
    } else if (rest.nonEmpty && isLikelyTypeName(first)) {
      // static context: T.m(args) or T.F
      val resolvedT = resolveTypeName(segments.dropRight(1).mkString(".") match {
        case ""    => first
        case other => other
      })
      val lastName = segments.last
      if (hasArgs) {
        val fullType = resolveTypeName(segments.dropRight(1).mkString("."))
        val call     = callNode(
          ctx,
          fullCode,
          lastName,
          unresolvedMethodFullName(fullType, lastName, argAsts.size),
          DispatchTypes.STATIC_DISPATCH,
          Option(s"${Defines.UnresolvedSignature}(${argAsts.size})"),
          Option(Defines.Any)
        )
        callAst(call, argAsts)
      } else {
        val baseIdent = Ast(identifierNode(ctx, first, first, resolveTypeName(first)))
        dynamicSuffix(baseIdent, rest, List.empty, hasTrailingArgs = false)
      }
    } else if (implicitReceiver.top.isDefined && !isLikelyTypeName(first)) {
      // unqualified name inside a pattern test → resolve against the pattern's own binding
      val (bindingName, bindingType) = implicitReceiver.top.get
      val base = boundIdentifierAst(ctx, bindingName, s"$bindingName.${(first +: rest).mkString(".")}", bindingType)
      dynamicSuffix(base, first +: rest, argAsts, hasArgs)
    } else if (rest.isEmpty && (hasArgs || isLikelyTypeName(first) == false && first.headOption.exists(_.isLower))) {
      // bare x(...) or unknown lowercase bare name
      if (hasArgs) {
        val call = callNode(
          ctx,
          fullCode,
          first,
          unresolvedMethodFullName(Defines.UnresolvedNamespace, first, argAsts.size),
          DispatchTypes.STATIC_DISPATCH,
          Option(s"${Defines.UnresolvedSignature}(${argAsts.size})"),
          Option(Defines.Any)
        )
        callAst(call, argAsts)
      } else {
        Ast(identifierNode(ctx, first, first, Defines.Any))
      }
    } else if (rest.isEmpty && hasArgs) {
      val call = callNode(
        ctx,
        fullCode,
        first,
        unresolvedMethodFullName(Defines.UnresolvedNamespace, first, argAsts.size),
        DispatchTypes.STATIC_DISPATCH,
        Option(s"${Defines.UnresolvedSignature}(${argAsts.size})"),
        Option(Defines.Any)
      )
      callAst(call, argAsts)
    } else {
      // fallback: chain of field accesses starting from an untyped identifier
      val base = Ast(identifierNode(ctx, first, first, Defines.Any))
      dynamicSuffix(base, rest, argAsts, hasArgs)
    }
  }

  // ------------------------------------------------------------------
  // creators / intervals / literals
  // ------------------------------------------------------------------

  /** `new T(args)` → alloc + `<init>` call; `new T[n]` / `new T[] {…}` → alloc/arrayInitializer. */
  private def astForCreator(ctx: ARLParser.CreatorContext): Ast = {
    val typeName = resolveTypeName(qualifiedNameText(ctx.qualifiedName()))
    Option(ctx.arguments()) match {
      case Some(argsCtx) =>
        val argAsts   = argsForArguments(argsCtx)
        val allocCall = operatorCallNode(ctx, s"new ${ctx.qualifiedName().getText}", Operators.alloc, Option(typeName))
        val allocAst  = callAst(allocCall, List.empty)
        val initCall  = callNode(
          ctx,
          code(ctx),
          Defines.ConstructorMethodName,
          unresolvedMethodFullName(typeName, Defines.ConstructorMethodName, argAsts.size),
          DispatchTypes.DYNAMIC_DISPATCH,
          Option(s"${Defines.UnresolvedSignature}(${argAsts.size})"),
          Option(typeName)
        )
        callAst(initCall, argAsts, base = Option(allocAst))
      case None =>
        Option(ctx.arrayCreatorRest()) match {
          case Some(rest) =>
            val dims = rest.expression().asScala.toList.map(astForExpression)
            Option(rest.arrayInit()) match {
              case Some(arrayInit) =>
                val elems = arrayInit.expression().asScala.toList.map(astForExpression)
                val call  = operatorCallNode(ctx, code(ctx), Operators.arrayInitializer, Option(s"$typeName[]"))
                callAst(call, elems)
              case None =>
                val call = operatorCallNode(ctx, code(ctx), Operators.alloc, Option(s"$typeName[]"))
                callAst(call, dims)
            }
          case None => unknownAst(ctx)
        }
    }
  }

  /** `[0,1]` / `]a,b[` → CALL `<operator>.interval(lo, hi)`, code = source text; trailing `.selector` chain applied. */
  private def astForInterval(ctx: ARLParser.IntervalLiteralContext): Ast = {
    val exprs = ctx.expression().asScala.toList.map(astForExpression)
    val call  = operatorCallNode(ctx, code(ctx).takeWhile(_ != '.'), ArlOperators.interval, Option(Defines.Any))
    var acc   = callAst(call, exprs)
    ctx.selector().asScala.toList.foreach { sel =>
      acc = astForSelector(sel, acc)
    }
    acc
  }

  private def astForLiteral(ctx: ARLParser.LiteralContext): Ast = {
    val text = ctx.getText
    val tpe  =
      if (Option(ctx.Integer()).isDefined) {
        if (text.endsWith("l") || text.endsWith("L")) "long" else "int"
      } else if (Option(ctx.FloatLit()).isDefined) {
        if (text.endsWith("f") || text.endsWith("F")) "float" else "double"
      } else if (Option(ctx.StringLit()).isDefined) {
        "java.lang.String"
      } else if (text == "true" || text == "false") {
        "boolean"
      } else {
        Defines.Any // null
      }
    Ast(literalNode(ctx, text, tpe))
  }
}
