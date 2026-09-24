package io.joern.arl2cpg.passes

import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{Cpg, DiffGraphBuilder, DispatchTypes, Operators, PropertyNames}
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory

import scala.jdk.CollectionConverters.*

/** Links ARL graph fragments against the Java XOM types imported by javasrc2cpg into the same CPG.
  *
  * Only touches nodes whose `filename` ends in `.arl`. Steps:
  *   1. simple-name type resolution (LOCAL / PARAMETER_IN / IDENTIFIER / MEMBER / TYPE_REF / METHOD_RETURN /
  *      fieldAccess CALL typeFullName + signature TYPE_DECL inheritsFromTypeFullName)
  *   2. dynamic and static call resolution against the Java type hierarchy
  *   3. field-access typing from Java MEMBERs / getters, with one propagation step into assigned identifiers
  *   4. STATIC dispatch flag on resolved static calls
  */
class XomLinkerPass(cpg: Cpg) extends CpgPass(cpg) {

  private val logger = LoggerFactory.getLogger(getClass)

  private case class JavaType(simpleName: String, fullName: String, typeDecl: TypeDecl)

  override def run(builder: DiffGraphBuilder): Unit = {
    val javaTypeDecls = cpg.typeDecl.isExternal(false).filenameNot(".*\\.arl$").l
    if (javaTypeDecls.isEmpty) return

    val byFullName   = javaTypeDecls.map(td => td.fullName -> td).toMap
    val bySimpleName = javaTypeDecls.groupBy(_.name)

    def resolveSimpleType(name: String): Option[String] =
      if (name.contains('.')) None
      else bySimpleName.get(name).filter(_.size == 1).map(_.head.fullName)

    // ------------------------------------------------------------------
    // 1. simple-name type resolution on ARL nodes
    // ------------------------------------------------------------------
    def setType(node: AstNode, currentType: String): Unit = {
      resolveSimpleType(currentType).foreach { resolved =>
        builder.setNodeProperty(node, PropertyNames.TypeFullName, resolved)
      }
    }

    def inArlFile(node: StoredNode): Boolean = node.file.name.exists(_.endsWith(".arl"))

    cpg.local.filter(inArlFile).foreach(local => setType(local, local.typeFullName))
    cpg.parameter.filter(inArlFile).foreach(param => setType(param, param.typeFullName))
    cpg.identifier.filter(inArlFile).foreach(identifier => setType(identifier, identifier.typeFullName))
    cpg.member.filter(inArlFile).foreach(member => setType(member, member.typeFullName))
    cpg.typeRef.filter(inArlFile).foreach(typeRef => setType(typeRef, typeRef.typeFullName))
    cpg.methodReturn.filter(inArlFile).foreach(methodReturn => setType(methodReturn, methodReturn.typeFullName))
    cpg.call
      .filter(inArlFile)
      .name(Operators.fieldAccess)
      .foreach(fieldAccess => setType(fieldAccess, fieldAccess.typeFullName))

    cpg.typeDecl.filename(".*\\.arl$").foreach { td =>
      val resolved = td.inheritsFromTypeFullName.toList.map { inh =>
        resolveSimpleType(inh).getOrElse(inh)
      }
      builder.setNodeProperty(td, PropertyNames.InheritsFromTypeFullName, resolved)
    }

    // helper: effective (post-step-1) type of an ARL node
    def effectiveType(currentType: String): String =
      resolveSimpleType(currentType).getOrElse(currentType)

    // ------------------------------------------------------------------
    // 2. call resolution against the Java type hierarchy
    // ------------------------------------------------------------------
    def superTypes(td: TypeDecl): List[TypeDecl] = {
      val seen                                    = scala.collection.mutable.Set.empty[String]
      def loop(current: TypeDecl): List[TypeDecl] = {
        if (!seen.add(current.fullName)) Nil
        else current +: current.inheritsFromTypeFullName.toList.flatMap(byFullName.get).flatMap(loop)
      }
      loop(td).tail
    }

    def findMethods(typeFullName: String, name: String): List[Method] = {
      byFullName.get(typeFullName).toList.flatMap { td =>
        (td +: superTypes(td)).flatMap(scope => scope.method.name(name).l)
      }
    }

    def expressionType(node: StoredNode): Option[String] = node match {
      case n: Call       => Option(n.typeFullName)
      case n: Identifier => Option(n.typeFullName)
      case n: Literal    => Option(n.typeFullName)
      case n: Block      => Option(n.typeFullName)
      case _             => None
    }

    def receiverTypeOf(call: Call): String = {
      // receiver = argument -1 (RECEIVER edge) or argument 0 (base)
      call.receiver.nextOption().flatMap(expressionType).map(effectiveType).getOrElse(Defines.UnresolvedNamespace)
    }

    cpg.call.filter(inArlFile).foreach { call =>
      val candidates = call.dispatchType match {
        case DispatchTypes.DYNAMIC_DISPATCH =>
          val tpe = receiverTypeOf(call)
          if (byFullName.contains(tpe)) findMethods(tpe, call.name) else List.empty
        case DispatchTypes.STATIC_DISPATCH =>
          // `T.m(...)` — T is the namespace prefix of methodFullName `<T>.m:<sig>`
          val owner = call.methodFullName.split(':').headOption.getOrElse("").stripSuffix(s".${call.name}")
          findMethods(owner, call.name)
        case _ => List.empty
      }
      // match arity: `parameter` includes the implicit `this` for instance methods, and `argument`
      // includes the receiver at index 0 — both counts line up directly.
      val arityMatches = candidates.filter(candidate => candidate.parameter.size == call.argument.size)
      arityMatches match {
        case single :: Nil =>
          builder.setNodeProperty(call, PropertyNames.MethodFullName, single.fullName)
          builder.setNodeProperty(call, PropertyNames.Signature, single.signature)
          builder.setNodeProperty(call, PropertyNames.TypeFullName, single.methodReturn.typeFullName)
          if (single.modifier.exists(_.modifierType == "STATIC")) {
            builder.setNodeProperty(call, PropertyNames.DispatchType, DispatchTypes.STATIC_DISPATCH)
          }
        case Nil  =>
        case many =>
          logger.debug(
            s"Ambiguous resolution for ARL call '${call.code}' (${many.size} candidates); leaving unresolved"
          )
      }
    }

    // ------------------------------------------------------------------
    // 3. field-access typing + one propagation step into assigned identifiers
    // ------------------------------------------------------------------
    def memberType(typeFullName: String, fieldName: String): Option[String] = {
      byFullName.get(typeFullName).flatMap { td =>
        (td +: superTypes(td)).toList.flatMap { scope =>
          val member      = scope.member.name(fieldName).headOption.map(_.typeFullName)
          lazy val getter = scope.method
            .name(s"get${fieldName.capitalize}", s"is${fieldName.capitalize}")
            .l
            .filter(_.parameter.isEmpty)
            .map(_.methodReturn.typeFullName)
            .headOption
          List(member, getter).flatten
        }.headOption
      }
    }

    cpg.call.filter(inArlFile).name(Operators.fieldAccess).foreach { call =>
      val args      = call.argument.l
      val baseType  = args.lift(0).flatMap(expressionType).map(effectiveType)
      val fieldName = args.lift(1).collect { case f: FieldIdentifier => f.canonicalName }
      (baseType, fieldName) match {
        case (Some(tpe), Some(field)) if byFullName.contains(tpe) =>
          memberType(tpe, field).foreach { mt =>
            builder.setNodeProperty(call, PropertyNames.TypeFullName, mt)
          }
        case _ =>
      }
    }

    // one propagation step: `x = a.b(); x.c()` — identifiers receiving a typed call get that type
    cpg.call.filter(inArlFile).name(Operators.assignment).foreach { call =>
      val args = call.argument.l
      (args.lift(0), args.lift(1)) match {
        case (Some(lhs: Identifier), Some(rhs: Expression)) =>
          val rhsType = expressionType(rhs).map(effectiveType).getOrElse(Defines.Any)
          if (rhsType != Defines.Any && byFullName.contains(rhsType)) {
            builder.setNodeProperty(lhs, PropertyNames.TypeFullName, rhsType)
          }
        case _ =>
      }
    }
  }
}
