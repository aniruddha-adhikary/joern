package io.joern.arl2cpg.passes

import io.joern.arl2cpg.ArlFindings.{Codes, Reasons}
import io.joern.arl2cpg.b2x.{B2xEffects, B2xMember, B2xModel}
import io.joern.arl2cpg.{ArlFindings, ArlTags}
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{
  Cpg,
  DiffGraphBuilder,
  DispatchTypes,
  EdgeTypes,
  EvaluationStrategies,
  NodeTypes,
  PropertyNames
}
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language.*

import scala.collection.mutable

/** Resolves the effects of ARL calls on business objects through the archive's B2X mapping. Ported from arlgraph
  * `Lowering.b2xEffects` / `candidates` / `emitB2xEffects`.
  *
  * ARL only *calls* B2X/XOM methods; their bodies live elsewhere. For every dynamic call on ruleset data:
  *   - if the mapping holds exactly one ARL body for (receiver class chain, name, arity), a METHOD node is created for
  *     that body (`filename` = the b2x file), the CALL gets a CALL edge to it and an `ARL_RESOLVES_TO` tag, and each
  *     field the body touches on `this` becomes an `ARL_WRITES` / `ARL_WRITES_INFERRED` / `ARL_READS` /
  *     `ARL_READS_INFERRED` tag on the CALL, path rewritten to the call's receiver;
  *   - whatever remains unresolved — no mapping at all, an unknown receiver type, a class or method the mapping lacks,
  *     an overload the call site cannot disambiguate, a body that does not parse, or a body that itself calls compiled
  *     Java — is an `ARL_MAY_AFFECT` tag on the CALL carrying the reason plus a FINDING (`unresolved-call-effects`).
  *
  * Nothing is ever dropped: a call that cannot be understood says so, in machine-readable form, on the call itself.
  */
class B2xEffectsPass(cpg: Cpg, b2x: Option[B2xModel]) extends CpgPass(cpg) {

  private val bodyMethods: mutable.Map[String, NewMethod] = mutable.Map.empty

  override def run(builder: DiffGraphBuilder): Unit = {
    b2x.foreach(model => reportUnhandled(builder, model))

    val javaTypeDecls = cpg.typeDecl.isExternal(false).filenameNot(".*\\.arl$").l
    val byFullName    = javaTypeDecls.map(td => td.fullName -> td).toMap

    def classChain(businessClass: String): List[String] = {
      val seen                        = mutable.LinkedHashSet.empty[String]
      def loop(current: String): Unit = if (seen.add(current)) {
        byFullName.get(current).toList.flatMap(_.inheritsFromTypeFullName).foreach(loop)
      }
      loop(businessClass)
      seen.toList
    }

    cpg.call
      .dispatchType(DispatchTypes.DYNAMIC_DISPATCH)
      .filter(_.file.name.exists(_.endsWith(".arl")))
      .filterNot(_.name == Defines.ConstructorMethodName)
      .foreach { call =>
        call.receiver.nextOption().foreach { receiver =>
          if (isRulesetData(receiver)) {
            val arity  = call.argument.size - 1
            val reason = effects(builder, call, receiver, arity, classChain)
            reason.foreach { r =>
              ArlTags.tag(builder, call, ArlTags.MayAffect, r)
              ArlFindings.finding(
                builder,
                Option(call),
                Codes.UnresolvedCallEffects,
                r,
                s"${receiver.code}.${call.name}/$arity may affect ${receiver.code}: $r",
                call.file.name.headOption.getOrElse(""),
                call.lineNumber
              )
            }
          }
        }
      }
  }

  /** A receiver that ultimately names ruleset data: `this`, a signature parameter, a binding or a local — as opposed to
    * a class named outright (`LoanUtil.audit(...)`), which is compiled Java and not data the graph tracks.
    */
  private def isRulesetData(receiver: Expression): Boolean = receiver match {
    case id: Identifier => id.name == "this" || id.refsTo.nonEmpty || id.typeFullName != Defines.Any
    case call: Call     => call.argument.headOption.exists(isRulesetData)
    case _              => false
  }

  private def effects(
    builder: DiffGraphBuilder,
    call: Call,
    receiver: Expression,
    arity: Int,
    classChain: String => List[String]
  ): Option[String] = {
    b2x match {
      case None        => Some(Reasons.CalleeBodyNotInArtifact)
      case Some(model) =>
        val receiverType = receiver.propertyOption(PropertyNames.TypeFullName).getOrElse("")
        if (receiverType.isEmpty || receiverType == Defines.Any || receiverType == Defines.UnresolvedNamespace) {
          return Some(Reasons.ReceiverTypeUnknown)
        }
        val chain      = classChain(receiverType)
        val candidates = chain.iterator.map(model.candidates(_, call.name, arity)).find(_.nonEmpty).getOrElse(Nil)
        candidates match {
          case Nil =>
            Some(if (chain.exists(model.hasClass)) Reasons.NoB2xBodyForMethod else Reasons.ClassNotInB2x)
          case _ :: _ :: _   => Some(Reasons.B2xOverloadAmbiguous)
          case member :: Nil =>
            B2xEffects.of(member, model) match {
              case None          => Some(Reasons.B2xBodyUnreadable)
              case Some(effects) =>
                emit(builder, call, receiver.code, member, effects, model)
                if (effects.opaque.isEmpty) None
                else Some(s"${Reasons.B2xBodyCallsMethodWithoutBody}:${effects.opaque.mkString(",")}")
            }
        }
    }
  }

  private def emit(
    builder: DiffGraphBuilder,
    call: Call,
    receiverPath: String,
    member: B2xMember,
    effects: B2xEffects,
    model: B2xModel
  ): Unit = {
    val method = bodyMethod(builder, member, model)
    builder.addEdge(call, method, EdgeTypes.CALL)
    builder.setNodeProperty(call, PropertyNames.MethodFullName, method.fullName)
    ArlTags.tag(builder, call, ArlTags.ResolvesTo, method.fullName)
    effects.writes.foreach(w => ArlTags.tag(builder, call, ArlTags.Writes, s"$receiverPath.$w"))
    effects.inferredWrites.foreach(w => ArlTags.tag(builder, call, ArlTags.WritesInferred, s"$receiverPath.$w"))
    effects.reads.foreach(r => ArlTags.tag(builder, call, ArlTags.Reads, s"$receiverPath.$r"))
    effects.inferredReads.foreach(r => ArlTags.tag(builder, call, ArlTags.ReadsInferred, s"$receiverPath.$r"))
  }

  /** One METHOD per B2X body, whatever the number of call sites that resolve to it. */
  private def bodyMethod(builder: DiffGraphBuilder, member: B2xMember, model: B2xModel): NewMethod =
    bodyMethods.getOrElseUpdate(
      member.key, {
        val signature = s"${member.paramTypes.mkString(",")}"
        val method    = NewMethod()
          .name(member.name)
          .fullName(B2xEffectsPass.bodyFullName(member))
          .signature(signature)
          .code(member.body)
          .filename(model.path)
          .astParentType(NodeTypes.TYPE_DECL)
          .astParentFullName(member.businessClass)
          .isExternal(false)
        val thisParam = NewMethodParameterIn()
          .name("this")
          .code("this")
          .index(0)
          .order(0)
          .typeFullName(member.businessClass)
          .evaluationStrategy(EvaluationStrategies.BY_REFERENCE)
        val params = member.paramTypes.zipWithIndex.map { case (tpe, i) =>
          NewMethodParameterIn()
            .name(s"p${i + 1}")
            .code(tpe)
            .index(i + 1)
            .order(i + 1)
            .typeFullName(tpe)
            .evaluationStrategy(EvaluationStrategies.BY_SHARING)
        }
        val block = NewBlock().code(member.body).typeFullName(Defines.Any).order(params.size + 1)
        val ret   = NewMethodReturn()
          .code("RET")
          .typeFullName(Defines.Any)
          .evaluationStrategy(EvaluationStrategies.BY_VALUE)
          .order(params.size + 2)
        builder.addNode(method)
        (thisParam :: params).foreach { p =>
          builder.addNode(p)
          builder.addEdge(method, p, EdgeTypes.AST)
        }
        builder.addNode(block)
        builder.addEdge(method, block, EdgeTypes.AST)
        builder.addNode(ret)
        builder.addEdge(method, ret, EdgeTypes.AST)
        method
      }
    )

  private def reportUnhandled(builder: DiffGraphBuilder, model: B2xModel): Unit =
    model.unhandled.foreach { u =>
      ArlFindings.finding(
        builder,
        None,
        Codes.B2xUnmodelledElement,
        u.path,
        s"b2x element not modelled: ${u.path} ${u.excerpt}",
        model.path,
        None
      )
    }
}

object B2xEffectsPass {

  /** `com.acme.Outcome.rejectWith:b2x(com.acme.Reason,int)` — the fullName of the METHOD standing for a B2X body. */
  def bodyFullName(member: B2xMember): String =
    s"${member.businessClass}.${member.name}:b2x(${member.paramTypes.mkString(",")})"
}
