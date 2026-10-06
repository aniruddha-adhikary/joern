package io.joern.arl2cpg.passes

import io.joern.arl2cpg.ArlFindings
import io.joern.arl2cpg.ArlFindings.{Codes, Keys}
import io.joern.arl2cpg.b2x.B2xModel
import io.joern.arl2cpg.passes.resolution.{JavaMethodInfo, TypeModel}
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{Cpg, DiffGraphBuilder, DispatchTypes, Operators, PropertyNames}
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language.*

import scala.collection.mutable

/** Links ARL calls against Java XOM, classpath, and JDK types using statically inferred argument types. */
class XomLinkerPass(cpg: Cpg, xomClasspath: Seq[String] = Seq.empty, b2x: Option[B2xModel] = None)
    extends CpgPass(cpg) {

  private val MaxIterations   = 30
  private val NullType        = "null"
  private val EngineDataKinds = Set("ruleflow", "flowtask", "functiontask")

  private sealed trait CallResolution
  private final case class Resolved(method: JavaMethodInfo)                     extends CallResolution
  private final case class Unresolved(reason: String, candidates: List[String]) extends CallResolution

  private final case class ResolvedCall(
    methodFullName: String,
    signature: String,
    returnType: String,
    isStatic: Boolean
  )

  override def run(builder: DiffGraphBuilder): Unit = {
    val sourceDecls    = cpg.typeDecl.isExternal(false).filenameNot(".*\\.arl$").l
    val signatureDecls = cpg.typeDecl
      .filename(".*\\.arl$")
      .l
      .filter(_.code.startsWith("signature "))
      .sortBy(typeDecl => (typeDecl.fullName, typeDecl.filename))
    val typeModel      = new TypeModel(sourceDecls ++ signatureDecls, xomClasspath)
    val xomSimpleNames = sourceDecls
      .map(td => td.name -> td.fullName)
      .groupBy(_._1)
      .view
      .mapValues(_.map(_._2).distinct.sorted)
      .toMap

    def resolveSimpleType(name: String): Option[String] =
      if (name.contains('.')) None
      else xomSimpleNames.get(name).filter(_.size == 1).flatMap(_.headOption)

    def inArlFile(node: StoredNode): Boolean = node.file.name.exists(_.endsWith(".arl"))

    val types: mutable.Map[Long, String]         = mutable.Map.empty
    val originalTypes: mutable.Map[Long, String] = mutable.Map.empty
    val nodesById: mutable.Map[Long, StoredNode] = mutable.Map.empty

    def seed(node: AstNode & StoredNode, currentType: String): Unit = {
      val resolved = node match {
        case call: Call if call.name == Operators.cast => currentType
        case _                                         => resolveSimpleType(currentType).getOrElse(currentType)
      }
      types(node.id()) = resolved
      originalTypes(node.id()) = currentType
      nodesById(node.id()) = node
    }

    cpg.local.filter(inArlFile).foreach(node => seed(node, node.typeFullName))
    cpg.parameter.filter(inArlFile).foreach(node => seed(node, node.typeFullName))
    cpg.identifier.filter(inArlFile).foreach(node => seed(node, node.typeFullName))
    cpg.literal.filter(inArlFile).foreach(node => seed(node, node.typeFullName))
    cpg.member.filter(inArlFile).foreach(node => seed(node, node.typeFullName))
    cpg.typeRef.filter(inArlFile).foreach(node => seed(node, node.typeFullName))
    cpg.methodReturn.filter(inArlFile).foreach(node => seed(node, node.typeFullName))
    cpg.call.filter(inArlFile).foreach(node => seed(node, node.typeFullName))

    signatureDecls match {
      case signature :: Nil =>
        val engineDataParameters = cpg.method
          .filter(inArlFile)
          .filter(method => method.annotation.name("arlKind").parameterAssign.value.code.l.exists(EngineDataKinds))
          .flatMap(_.parameter.l)
          .filter(_.name.startsWith("$"))
          .toList
          .sortBy(_.id())
        val engineDataNames = engineDataParameters.map(_.name).toSet
        engineDataParameters.foreach(parameter => types(parameter.id()) = signature.fullName)
        cpg.identifier
          .filter(inArlFile)
          .filter(identifier => identifier.typeFullName == Defines.Any && engineDataNames.contains(identifier.name))
          .foreach(identifier => types(identifier.id()) = signature.fullName)
      case _ =>
    }

    cpg.typeDecl.filename(".*\\.arl$").l.sortBy(_.fullName).foreach { typeDecl =>
      val resolvedParents = typeDecl.inheritsFromTypeFullName.toList.map { parent =>
        resolveSimpleType(parent).getOrElse(parent)
      }
      builder.setNodeProperty(typeDecl, PropertyNames.InheritsFromTypeFullName, resolvedParents)
    }

    def typeOf(node: StoredNode): Option[String] = types.get(node.id()).filter(_.nonEmpty)

    def expressionType(node: StoredNode): Option[String] = node match {
      case literal: Literal if literal.code == "null" => Some(NullType)
      case call: Call                                 => typeOf(call)
      case identifier: Identifier                     => typeOf(identifier)
      case literal: Literal                           => typeOf(literal)
      case block: Block                               => typeOf(block)
      case _                                          => None
    }

    def isKnownType(typeName: String): Boolean =
      typeName == NullType || typeModel.isPrimitive(typeName) || typeModel.get(typeName).isDefined

    def hasUnknownType(node: StoredNode): Boolean =
      typeOf(node).forall(tpe => tpe == Defines.Any || tpe == Defines.UnresolvedNamespace)

    def setType(node: StoredNode, typeName: String): Boolean =
      types.get(node.id()).forall(_ != typeName) && { types(node.id()) = typeName; true }

    val allArlCalls = cpg.call
      .filter(inArlFile)
      .l
      .sortBy(call => (call.lineNumber.getOrElse(-1), call.id()))
    val arlCalls      = allArlCalls.filterNot(_.name.startsWith("<operator>."))
    val fieldAccesses = allArlCalls.filter(_.name == Operators.fieldAccess)
    val assignments   = allArlCalls.filter(call =>
      Set(
        Operators.assignment,
        Operators.assignmentPlus,
        Operators.assignmentMinus,
        Operators.assignmentMultiplication,
        Operators.assignmentDivision
      ).contains(call.name)
    )
    val operatorCalls   = allArlCalls.filter(_.name.startsWith("<operator>."))
    val resolvableCalls = arlCalls.filter(call =>
      call.dispatchType == DispatchTypes.DYNAMIC_DISPATCH || call.dispatchType == DispatchTypes.STATIC_DISPATCH
    )
    val resolvedCalls    = mutable.Map.empty[Long, ResolvedCall]
    val finalResolutions = mutable.Map.empty[Long, CallResolution]

    def expressionTypeOf(node: StoredNode): String =
      expressionType(node).getOrElse(Defines.Any)

    def orderedArguments(call: Call): List[Expression] =
      call.argument.l.sortBy(_.argumentIndex)

    def actualArguments(call: Call): List[Expression] = {
      val arguments = orderedArguments(call)
      if (call.dispatchType == DispatchTypes.DYNAMIC_DISPATCH) arguments.drop(1)
      else arguments
    }

    def staticOwner(call: Call): Option[String] = {
      val prefix     = call.methodFullName.takeWhile(_ != ':')
      val rawOwner   = prefix.stripSuffix(s".${call.name}")
      val unresolved = Set(Defines.UnresolvedNamespace, "", "<empty>")
      if (unresolved.contains(rawOwner) || rawOwner.contains(Defines.UnresolvedNamespace)) None
      else if (rawOwner.contains('.')) typeModel.get(rawOwner).map(_.fullName)
      else {
        xomSimpleNames
          .get(rawOwner)
          .filter(_.size == 1)
          .flatMap(_.headOption)
          .orElse(typeModel.get(s"java.lang.$rawOwner").map(_.fullName))
      }
    }

    def receiverType(call: Call): Option[String] =
      call.receiver
        .nextOption()
        .flatMap(expressionType)
        .orElse(orderedArguments(call).headOption.flatMap(expressionType))

    def hasApplicableArity(method: JavaMethodInfo, argumentCount: Int): Boolean =
      if (method.isVarargs) argumentCount >= method.paramTypes.size - 1
      else argumentCount == method.paramTypes.size

    def candidateMethods(call: Call, owner: String): List[JavaMethodInfo] = {
      val named = typeModel.methodsFor(owner, call.name)
      if (call.dispatchType == DispatchTypes.STATIC_DISPATCH && call.name != Defines.ConstructorMethodName)
        named.filter(_.isStatic)
      else named
    }

    def constructorType(call: Call, method: JavaMethodInfo): String =
      receiverType(call)
        .flatMap(typeName => typeModel.get(typeName).map(_.fullName))
        .orElse(staticOwner(call))
        .getOrElse(method.owner)

    def resolveCall(call: Call): CallResolution = {
      def noCandidate(owner: String, candidateNames: List[String]): Unresolved = {
        val hierarchyNames = typeModel.hierarchyNames(owner)
        val b2xCandidates  = b2x.toList
          .flatMap(model =>
            hierarchyNames.flatMap(typeName => model.candidates(typeName, call.name, actualArguments(call).size))
          )
          .map(_.key)
          .distinct
          .sorted
        if (
          !typeModel.hasMethodNamedInHierarchy(owner, call.name) &&
          b2xCandidates.nonEmpty
        ) Unresolved("bom-only", b2xCandidates)
        else Unresolved("no-candidate", candidateNames)
      }

      val owner = call.dispatchType match {
        case DispatchTypes.DYNAMIC_DISPATCH =>
          receiverType(call).flatMap(typeName => typeModel.get(typeName).map(_.fullName))
        case DispatchTypes.STATIC_DISPATCH => staticOwner(call)
        case _                             => None
      }
      owner match {
        case None                  => Unresolved("receiver-unknown", Nil)
        case Some(receiverOrOwner) =>
          val candidates     = candidateMethods(call, receiverOrOwner).sortBy(_.fullName)
          val arguments      = actualArguments(call)
          val argumentTypes  = arguments.map(expressionTypeOf)
          val candidateNames = candidates.map(_.fullName).distinct.sorted

          if (argumentTypes.contains(Defines.Any)) {
            val arityCandidates = candidates.filter(hasApplicableArity(_, argumentTypes.size))
            arityCandidates match {
              case single :: Nil => Resolved(single)
              case Nil           => noCandidate(receiverOrOwner, candidateNames)
              case many          => Unresolved("ambiguous", many.map(_.fullName).distinct.sorted)
            }
          } else {
            val fixedPhase1 = candidates.filter(method => fixedArityApplicable(method, argumentTypes, phase2 = false))
            val fixedPhase2 =
              if (fixedPhase1.nonEmpty) Nil
              else candidates.filter(method => fixedArityApplicable(method, argumentTypes, phase2 = true))
            val varargsPhase =
              if (fixedPhase1.nonEmpty || fixedPhase2.nonEmpty) Nil
              else candidates.filter(method => varargsApplicable(method, argumentTypes))

            val (applicable, isVarargsPhase) =
              if (fixedPhase1.nonEmpty) (fixedPhase1, false)
              else if (fixedPhase2.nonEmpty) (fixedPhase2, false)
              else (varargsPhase, true)

            if (applicable.isEmpty) noCandidate(receiverOrOwner, candidateNames)
            else {
              val maximal = applicable.filterNot { candidate =>
                applicable.exists(other =>
                  other.fullName != candidate.fullName && isMoreSpecific(
                    other,
                    candidate,
                    argumentTypes.size,
                    isVarargsPhase
                  )
                )
              }
              maximal match {
                case single :: Nil => Resolved(single)
                case many          => Unresolved("ambiguous", many.map(_.fullName).distinct.sorted)
              }
            }
          }
      }
    }

    def receiverTypeForFinding(call: Call): String = {
      val typeName = call.dispatchType match {
        case DispatchTypes.DYNAMIC_DISPATCH => receiverType(call)
        case DispatchTypes.STATIC_DISPATCH  => staticOwner(call)
        case _                              => None
      }
      typeName
        .filterNot(name => name == Defines.Any || name == Defines.UnresolvedNamespace)
        .map(name => typeModel.get(name).map(_.fullName).getOrElse(name))
        .getOrElse(Defines.Any)
    }

    def fixedArityApplicable(method: JavaMethodInfo, argumentTypes: List[String], phase2: Boolean): Boolean =
      method.paramTypes.size == argumentTypes.size &&
        argumentTypes.zip(method.paramTypes).forall { case (argumentType, parameterType) =>
          isInvocationConvertible(argumentType, parameterType, phase2)
        }

    def varargsApplicable(method: JavaMethodInfo, argumentTypes: List[String]): Boolean =
      method.isVarargs && method.paramTypes.nonEmpty && argumentTypes.size >= method.paramTypes.size - 1 && {
        val fixedCount   = method.paramTypes.size - 1
        val fixedMatches = argumentTypes.take(fixedCount).zip(method.paramTypes.take(fixedCount)).forall {
          case (argumentType, parameterType) => isInvocationConvertible(argumentType, parameterType, phase2 = true)
        }
        val component = method.paramTypes.last.stripSuffix("[]")
        fixedMatches && argumentTypes
          .drop(fixedCount)
          .forall(argumentType => isInvocationConvertible(argumentType, component, phase2 = true))
      }

    def isInvocationConvertible(argumentType: String, parameterType: String, phase2: Boolean): Boolean = {
      val strict =
        argumentType == parameterType ||
          (argumentType == NullType && typeModel.isReference(parameterType)) ||
          (typeModel.isPrimitive(argumentType) && typeModel.isPrimitive(parameterType) &&
            primitiveWidening(argumentType, parameterType)) ||
          (typeModel.isReference(argumentType) && typeModel.isReference(parameterType) &&
            typeModel.isSubtype(argumentType, parameterType))
      if (strict || !phase2) strict
      else {
        val boxedReference =
          BoxedTypes
            .get(argumentType)
            .exists(boxedType => typeModel.isReference(parameterType) && typeModel.isSubtype(boxedType, parameterType))
        val unboxedPrimitive =
          UnboxedTypes
            .get(argumentType)
            .exists(unboxedType =>
              typeModel.isPrimitive(parameterType) &&
                (unboxedType == parameterType || primitiveWidening(unboxedType, parameterType))
            )
        boxedReference || unboxedPrimitive
      }
    }

    def isMoreSpecific(
      first: JavaMethodInfo,
      second: JavaMethodInfo,
      argumentCount: Int,
      varargsPhase: Boolean
    ): Boolean = {
      def expanded(method: JavaMethodInfo): List[String] =
        if (!varargsPhase || !method.isVarargs) method.paramTypes
        else {
          val fixedCount = method.paramTypes.size - 1
          method.paramTypes.take(fixedCount) ++
            List.fill(math.max(1, argumentCount - fixedCount))(method.paramTypes.last.stripSuffix("[]"))
        }
      val firstParams  = expanded(first)
      val secondParams = expanded(second)
      firstParams.size == secondParams.size &&
      firstParams.zip(secondParams).forall { case (firstParam, secondParam) =>
        typeModel.isSubtype(firstParam, secondParam) ||
        (typeModel.isPrimitive(firstParam) && typeModel.isPrimitive(secondParam) &&
          primitiveWidening(firstParam, secondParam))
      } &&
      firstParams.zip(secondParams).exists { case (firstParam, secondParam) => firstParam != secondParam }
    }

    def primitiveWidening(from: String, to: String): Boolean =
      PrimitiveWidening.getOrElse(from, Set.empty).contains(to)

    def unboxed(typeName: String): String = UnboxedTypes.getOrElse(typeName, typeName)

    def numericPromotion(types: List[String]): Option[String] = {
      val promoted = types.map(unboxed)
      if (promoted.forall(NumericTypes.contains)) {
        Some(
          if (promoted.contains("double")) "double"
          else if (promoted.contains("float")) "float"
          else if (promoted.contains("long")) "long"
          else "int"
        )
      } else None
    }

    def conditionalType(first: String, second: String): Option[String] = {
      val unknownTypes = Set(Defines.Any, Defines.UnresolvedNamespace)
      if (unknownTypes.contains(first) || unknownTypes.contains(second)) None
      else if (first == second) Some(first)
      else if (
        Set("boolean", "java.lang.Boolean").contains(first) &&
        Set("boolean", "java.lang.Boolean").contains(second)
      ) Some("boolean")
      else {
        val firstPrimitive  = unboxed(first)
        val secondPrimitive = unboxed(second)
        if (NumericTypes.contains(firstPrimitive) && NumericTypes.contains(secondPrimitive)) {
          if (firstPrimitive == secondPrimitive) Some(firstPrimitive)
          else if (Set(firstPrimitive, secondPrimitive) == Set("byte", "short")) Some("short")
          else numericPromotion(List(firstPrimitive, secondPrimitive))
        } else if (first == NullType) {
          if (typeModel.isReference(second)) Some(second)
          else if (typeModel.isPrimitive(second)) BoxedTypes.get(second)
          else None
        } else if (second == NullType) {
          if (typeModel.isReference(first)) Some(first)
          else if (typeModel.isPrimitive(first)) BoxedTypes.get(first)
          else None
        } else if (
          typeModel.isReference(first) && typeModel.isReference(second) && typeModel.isSubtype(first, second)
        ) {
          Some(second)
        } else if (
          typeModel.isReference(first) && typeModel.isReference(second) && typeModel.isSubtype(second, first)
        ) {
          Some(first)
        } else None
      }
    }

    def unaryNumericPromotion(typeName: String): Option[String] = {
      val primitive = unboxed(typeName)
      Option.when(NumericTypes.contains(primitive)) {
        if (Set("byte", "short", "char").contains(primitive)) "int" else primitive
      }
    }

    def inferOperatorType(call: Call): Option[String] = {
      val arguments        = orderedArguments(call)
      val argTypes         = arguments.map(expressionTypeOf)
      val booleanOperators = Set(
        Operators.equals,
        Operators.notEquals,
        Operators.lessThan,
        Operators.greaterThan,
        Operators.lessEqualsThan,
        Operators.greaterEqualsThan,
        Operators.logicalAnd,
        Operators.logicalOr,
        Operators.logicalNot,
        Operators.instanceOf
      )
      val assignmentOperators = Set(
        Operators.assignment,
        Operators.assignmentPlus,
        Operators.assignmentMinus,
        Operators.assignmentMultiplication,
        Operators.assignmentDivision
      )
      if (booleanOperators.contains(call.name)) Some("boolean")
      else if (assignmentOperators.contains(call.name)) argTypes.headOption.filter(_ != Defines.Any)
      else if (call.name == Operators.conditional) {
        argTypes.lift(1).flatMap(first => argTypes.lift(2).flatMap(second => conditionalType(first, second)))
      } else if (call.name == Operators.indexAccess) {
        argTypes.headOption.filter(_.endsWith("[]")).map(_.dropRight(2))
      } else if (call.name == Operators.plus && argTypes.contains("java.lang.String")) {
        Some("java.lang.String")
      } else if (call.name == Operators.minus && argTypes.size == 1) {
        argTypes.headOption.flatMap(unaryNumericPromotion)
      } else if (
        Set(Operators.plus, Operators.minus, Operators.multiplication, Operators.division, Operators.modulo)
          .contains(call.name)
      ) {
        numericPromotion(argTypes)
      } else None
    }

    // Keep the existing bounded fixed-point loop: newly typed receivers, fields, and calls can unlock later rounds.
    var changed                                              = true
    var iteration                                            = 0
    def updateType(node: StoredNode, typeName: String): Unit =
      if (setType(node, typeName)) changed = true

    while (changed && iteration < MaxIterations) {
      changed = false
      iteration += 1
      val previousResolutions = resolvedCalls.toMap
      val nextResolutions     = mutable.Map.empty[Long, ResolvedCall]
      val nextOutcomes        = mutable.Map.empty[Long, CallResolution]

      fieldAccesses.foreach { call =>
        val arguments = orderedArguments(call)
        val baseType  = arguments.headOption.flatMap(expressionType)
        val fieldName = arguments.lift(1).collect { case field: FieldIdentifier => field.canonicalName }
        (baseType, fieldName) match {
          case (Some(typeName), Some("this")) =>
            updateType(call, typeName)
          case (Some(typeName), Some(field)) =>
            typeModel.memberType(typeName, field).foreach(memberType => updateType(call, memberType))
          case _ =>
        }
      }

      resolvableCalls.foreach { call =>
        val result = resolveCall(call)
        nextOutcomes(call.id()) = result
        result match {
          case Resolved(method) =>
            val returnType =
              if (method.name == Defines.ConstructorMethodName) constructorType(call, method) else method.returnType
            updateType(call, returnType)
            nextResolutions(call.id()) = ResolvedCall(method.fullName, method.signature, returnType, method.isStatic)
          case _: Unresolved =>
            if (previousResolutions.contains(call.id()) && originalTypes.get(call.id()).contains(Defines.Any)) {
              updateType(call, Defines.Any)
            }
        }
      }

      assignments.foreach { call =>
        val arguments = orderedArguments(call)
        (arguments.headOption, arguments.lift(1)) match {
          case (Some(lhs: Identifier), Some(rhs: Expression)) =>
            val rhsType = expressionTypeOf(rhs)
            if (rhsType != Defines.Any && rhsType != NullType && isKnownType(rhsType)) {
              if (hasUnknownType(lhs)) updateType(lhs, rhsType)
              lhs.refsTo.l.sortBy(_.id()).foreach { declaration =>
                declaration match {
                  case local: Local if hasUnknownType(local) =>
                    updateType(local, rhsType)
                  case parameter: MethodParameterIn if hasUnknownType(parameter) =>
                    updateType(parameter, rhsType)
                  case _ =>
                }
                declaration._refIn.l
                  .collect { case identifier: Identifier => identifier }
                  .sortBy(_.id())
                  .filter(hasUnknownType)
                  .foreach(identifier => updateType(identifier, rhsType))
              }
            }
          case _ =>
        }
      }

      operatorCalls.foreach { call =>
        if (typeOf(call).contains(Defines.Any)) {
          inferOperatorType(call).foreach(typeName => updateType(call, typeName))
        }
      }

      resolvedCalls.clear()
      resolvedCalls ++= nextResolutions
      finalResolutions.clear()
      finalResolutions ++= nextOutcomes
      if (resolvedCalls.toMap != previousResolutions) changed = true
    }

    resolvableCalls.foreach { call =>
      val result = resolveCall(call)
      finalResolutions(call.id()) = result
      result match {
        case Resolved(method) =>
          val returnType =
            if (method.name == Defines.ConstructorMethodName) constructorType(call, method) else method.returnType
          types(call.id()) = returnType
          resolvedCalls(call.id()) = ResolvedCall(method.fullName, method.signature, returnType, method.isStatic)
        case _: Unresolved =>
          resolvedCalls.remove(call.id())
          if (originalTypes.get(call.id()).contains(Defines.Any)) types(call.id()) = Defines.Any
      }
    }

    types.toList.sortBy(_._1).foreach { case (id, typeName) =>
      if (originalTypes.get(id).forall(_ != typeName)) {
        nodesById.get(id).foreach(node => builder.setNodeProperty(node, PropertyNames.TypeFullName, typeName))
      }
    }

    resolvedCalls.toList.sortBy(_._1).foreach { case (id, resolved) =>
      nodesById.get(id).foreach { node =>
        builder.setNodeProperty(node, PropertyNames.MethodFullName, resolved.methodFullName)
        builder.setNodeProperty(node, PropertyNames.Signature, resolved.signature)
        if (resolved.isStatic) {
          builder.setNodeProperty(node, PropertyNames.DispatchType, DispatchTypes.STATIC_DISPATCH)
        }
      }
    }

    resolvableCalls.foreach { call =>
      val methodFullName   = call.methodFullName
      val unresolvedTarget =
        methodFullName.contains(Defines.UnresolvedSignature) || methodFullName.contains(Defines.UnresolvedNamespace)
      if (unresolvedTarget && !resolvedCalls.contains(call.id())) {
        val result               = finalResolutions.getOrElse(call.id(), Unresolved("receiver-unknown", Nil))
        val (reason, candidates) = result match {
          case Unresolved(reason, names) => (reason, names.distinct.sorted)
          case Resolved(_)               => ("no-candidate", Nil)
        }
        val filename = call.file.name.headOption.getOrElse("")
        ArlFindings.finding(
          builder,
          Some(call),
          Codes.UnresolvedCallTarget,
          reason,
          s"unable to resolve Java call '${call.code}' ($reason)",
          filename,
          call.lineNumber,
          List(Keys.CallId -> call.id().toString) ++ candidates.map(candidate => Keys.Candidates -> candidate) ++ List(
            Keys.ReceiverType -> receiverTypeForFinding(call)
          )
        )
      }
    }
  }

  private val BoxedTypes = Map(
    "boolean" -> "java.lang.Boolean",
    "byte"    -> "java.lang.Byte",
    "short"   -> "java.lang.Short",
    "int"     -> "java.lang.Integer",
    "long"    -> "java.lang.Long",
    "float"   -> "java.lang.Float",
    "double"  -> "java.lang.Double",
    "char"    -> "java.lang.Character"
  )

  private val UnboxedTypes = BoxedTypes.map(_.swap)

  private val NumericTypes = Set("byte", "short", "int", "long", "float", "double", "char")

  private val PrimitiveWidening = Map(
    "byte"    -> Set("short", "int", "long", "float", "double"),
    "short"   -> Set("int", "long", "float", "double"),
    "char"    -> Set("int", "long", "float", "double"),
    "int"     -> Set("long", "float", "double"),
    "long"    -> Set("float", "double"),
    "float"   -> Set("double"),
    "double"  -> Set.empty[String],
    "boolean" -> Set.empty[String]
  )
}
