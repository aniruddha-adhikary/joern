package io.joern.arl2cpg.passes

import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{Cpg, DiffGraphBuilder, DispatchTypes, Operators, PropertyNames}
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory

import scala.collection.mutable

/** Links ARL graph fragments against the Java XOM types imported by javasrc2cpg into the same CPG.
  *
  * Only touches nodes whose `filename` ends in `.arl`.
  *
  * Step 1 resolves simple type names on ARL nodes (LOCAL / PARAMETER_IN / IDENTIFIER / MEMBER / TYPE_REF /
  * METHOD_RETURN / fieldAccess CALL typeFullName + signature TYPE_DECL inheritsFromTypeFullName) and seeds an in-pass
  * type map. Steps 2-4 then iterate to a fixpoint over that map — never reading stored properties the pass itself is
  * about to write:
  *   - fieldAccess typing from Java MEMBERs / zero-arg getters
  *   - dynamic and static call resolution against the Java type hierarchy (unique arity match wins)
  *   - assignment propagation: a Java-typed RHS types the LHS identifier, the LOCAL it references, and every
  *     still-untyped Identifier of the same name in the same METHOD
  *
  * After the fixpoint, all changed types and resolved call targets are written back.
  */
class XomLinkerPass(cpg: Cpg) extends CpgPass(cpg) {

  private val logger = LoggerFactory.getLogger(getClass)

  private val MaxIterations = 10

  override def run(builder: DiffGraphBuilder): Unit = {
    val javaTypeDecls = cpg.typeDecl.isExternal(false).filenameNot(".*\\.arl$").l
    if (javaTypeDecls.isEmpty) return

    val byFullName   = javaTypeDecls.map(td => td.fullName -> td).toMap
    val bySimpleName = javaTypeDecls.groupBy(_.name)

    def resolveSimpleType(name: String): Option[String] =
      if (name.contains('.')) None
      else bySimpleName.get(name).filter(_.size == 1).map(_.head.fullName)

    def inArlFile(node: StoredNode): Boolean = node.file.name.exists(_.endsWith(".arl"))

    // ------------------------------------------------------------------
    // 1. simple-name type resolution on ARL nodes + seed the type map
    // ------------------------------------------------------------------
    val types: mutable.Map[Long, String]         = mutable.Map.empty
    val originalTypes: mutable.Map[Long, String] = mutable.Map.empty
    val nodesById: mutable.Map[Long, StoredNode] = mutable.Map.empty

    def seed(node: AstNode & StoredNode, currentType: String): Unit = {
      val resolved = resolveSimpleType(currentType).getOrElse(currentType)
      types(node.id()) = resolved
      originalTypes(node.id()) = currentType
      nodesById(node.id()) = node
    }

    cpg.local.filter(inArlFile).foreach(node => seed(node, node.typeFullName))
    cpg.parameter.filter(inArlFile).foreach(node => seed(node, node.typeFullName))
    cpg.identifier.filter(inArlFile).foreach(node => seed(node, node.typeFullName))
    cpg.member.filter(inArlFile).foreach(node => seed(node, node.typeFullName))
    cpg.typeRef.filter(inArlFile).foreach(node => seed(node, node.typeFullName))
    cpg.methodReturn.filter(inArlFile).foreach(node => seed(node, node.typeFullName))
    cpg.call.filter(inArlFile).foreach(node => seed(node, node.typeFullName))

    cpg.typeDecl.filename(".*\\.arl$").foreach { td =>
      val resolved = td.inheritsFromTypeFullName.toList.map { inh =>
        resolveSimpleType(inh).getOrElse(inh)
      }
      builder.setNodeProperty(td, PropertyNames.InheritsFromTypeFullName, resolved)
    }

    def typeOf(node: StoredNode): Option[String] = types.get(node.id()).filter(_.nonEmpty)

    def isKnownJavaType(tpe: String): Boolean = byFullName.contains(tpe)

    def expressionType(node: StoredNode): Option[String] = node match {
      case n: Call       => typeOf(n)
      case n: Identifier => typeOf(n)
      case n: Literal    => typeOf(n)
      case n: Block      => typeOf(n)
      case _             => None
    }

    // ------------------------------------------------------------------
    // helpers over the Java type hierarchy
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

    def memberType(typeFullName: String, fieldName: String): Option[String] = {
      byFullName.get(typeFullName).flatMap { td =>
        (td +: superTypes(td)).toList.flatMap { scope =>
          val member      = scope.member.name(fieldName).headOption.map(_.typeFullName)
          lazy val getter = scope.method
            .name(s"get${fieldName.capitalize}", s"is${fieldName.capitalize}")
            .l
            .filter(getter => getter.parameter.forall(_.index <= 0))
            .map(_.methodReturn.typeFullName)
            .headOption
          List(member, getter).flatten
        }.headOption
      }
    }

    def receiverTypeOf(call: Call): String = {
      // receiver = argument -1 (RECEIVER edge) or argument 0 (base)
      call.receiver.nextOption().flatMap(expressionType).getOrElse(Defines.UnresolvedNamespace)
    }

    case class ResolvedCall(methodFullName: String, signature: String, isStatic: Boolean)
    val resolvedCalls: mutable.Map[Long, ResolvedCall] = mutable.Map.empty

    def setType(node: StoredNode, tpe: String): Boolean =
      types.get(node.id()).forall(_ != tpe) && { types(node.id()) = tpe; true }

    // ------------------------------------------------------------------
    // 2-4. fixpoint: fieldAccess typing, call resolution, assignment propagation
    // ------------------------------------------------------------------
    val arlCalls        = cpg.call.filter(inArlFile).l
    val fieldAccesses   = arlCalls.filter(_.name == Operators.fieldAccess)
    val assignments     = arlCalls.filter(_.name == Operators.assignment)
    val resolvableCalls = arlCalls.filter(call =>
      call.dispatchType == DispatchTypes.DYNAMIC_DISPATCH || call.dispatchType == DispatchTypes.STATIC_DISPATCH
    )

    var changed   = true
    var iteration = 0
    while (changed && iteration < MaxIterations) {
      changed = false
      iteration += 1

      // (a) field-access typing: base type (from the map) -> Java MEMBER/getter type
      fieldAccesses.foreach { call =>
        val args      = call.argument.l
        val baseType  = args.lift(0).flatMap(expressionType)
        val fieldName = args.lift(1).collect { case f: FieldIdentifier => f.canonicalName }
        (baseType, fieldName) match {
          case (Some(tpe), Some(field)) if isKnownJavaType(tpe) =>
            memberType(tpe, field).foreach(mt => changed ||= setType(call, mt))
          case _ =>
        }
      }

      // (b) call resolution: dynamic via receiver type, static via owner prefix of methodFullName
      resolvableCalls.foreach { call =>
        val candidates = call.dispatchType match {
          case DispatchTypes.DYNAMIC_DISPATCH =>
            val tpe = receiverTypeOf(call)
            if (isKnownJavaType(tpe)) findMethods(tpe, call.name) else List.empty
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
            val returnType = single.methodReturn.typeFullName
            changed ||= setType(call, returnType)
            val isStatic = single.modifier.exists(_.modifierType == "STATIC")
            resolvedCalls(call.id()) = ResolvedCall(single.fullName, single.signature, isStatic)
          case Nil  =>
          case many =>
            logger.debug(
              s"Ambiguous resolution for ARL call '${call.code}' (${many.size} candidates); leaving unresolved"
            )
        }
      }

      // (c) assignment propagation: `x = e` — type the LHS identifier, its LOCAL, and
      // same-named untyped identifiers in the same method.
      assignments.foreach { call =>
        val args = call.argument.l
        (args.lift(0), args.lift(1)) match {
          case (Some(lhs: Identifier), Some(rhs: Expression)) =>
            val rhsType = expressionType(rhs).getOrElse(Defines.Any)
            if (rhsType != Defines.Any && isKnownJavaType(rhsType)) {
              changed ||= setType(lhs, rhsType)
              lhs.refsTo.l.foreach { decl =>
                decl match {
                  case local: Local             => changed ||= setType(local, rhsType)
                  case param: MethodParameterIn => changed ||= setType(param, rhsType)
                  case _                        =>
                }
                // every other read of the same decl in this method
                decl._refIn.l
                  .collect { case id: Identifier => id }
                  .filter(id => typeOf(id).forall(tpe => tpe == Defines.Any || tpe == Defines.UnresolvedNamespace))
                  .foreach(id => changed ||= setType(id, rhsType))
              }
            }
          case _ =>
        }
      }
    }

    // ------------------------------------------------------------------
    // write back: types that changed since seeding + resolved call targets
    // ------------------------------------------------------------------
    types.foreach { case (id, tpe) =>
      if (originalTypes.get(id).forall(_ != tpe)) {
        nodesById.get(id).foreach { node =>
          builder.setNodeProperty(node, PropertyNames.TypeFullName, tpe)
        }
      }
    }

    resolvedCalls.foreach { case (id, resolved) =>
      nodesById.get(id).foreach { node =>
        builder.setNodeProperty(node, PropertyNames.MethodFullName, resolved.methodFullName)
        builder.setNodeProperty(node, PropertyNames.Signature, resolved.signature)
        if (resolved.isStatic) {
          builder.setNodeProperty(node, PropertyNames.DispatchType, DispatchTypes.STATIC_DISPATCH)
        }
      }
    }
  }
}
