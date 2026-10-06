package io.joern.x2cpg.frontendspecific.arl2cpg

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{
  AstNode,
  Call,
  CfgNode,
  ControlStructure,
  FieldIdentifier,
  Finding,
  Identifier,
  Literal,
  Local,
  Member,
  Method,
  MethodParameterIn,
  TypeDecl
}
import io.shiftleft.codepropertygraph.generated.neighboraccessors.Lang.*
import io.shiftleft.semanticcpg.language.*

import scala.collection.mutable

/** Deterministic export contract: the root has `cpgFile` (filename only), `methods`, `types`, and `findings`. Methods
  * are sorted by `(fullName, id)` and contain `id`, `name`, `fullName`, `signature`, `filename`, `line`, `lineEnd`,
  * `arlKind`, and `nodes` (sorted by id); types are sorted by `(fullName, id)` and contain `id`, `name`, `fullName`,
  * `file`, sorted `inherits`, and `members` sorted by `(name, id)` with `id`, `name`, `typeFullName`, `code`, and
  * `line`; findings are sorted by node id and contain sorted key/value fields, with duplicate keys rejected.
  *
  * Every node has `id`, `label`, `order`, `code`, `line`, `columnNumber`, and AST-child ids sorted by `(order, id)`;
  * CFG nodes add sorted `cfgOut`. CALL adds `name`, `methodFullName`, `signature`, `typeFullName`, arguments as
  * `{id, index}` sorted by `(index, id)`, and callees as `{id, fullName, external}` sorted by `(fullName, id)`;
  * interval CALLs alone may add boolean `lowerClosed`/`upperClosed`. IDENTIFIER adds `name`, `typeFullName`, and sorted
  * reference ids; LOCAL adds `name` and `typeFullName`; METHOD_PARAMETER_IN adds `name`, `typeFullName`, and `index`;
  * LITERAL adds `typeFullName`; FIELD_IDENTIFIER adds `canonicalName`; CONTROL_STRUCTURE adds `controlStructureType`
  * and `condition`.
  *
  * All ids are CPG node ids encoded as JSON numbers. Missing positions and conditions are JSON null. Null literals
  * export `typeFullName` as `ANY`.
  */
object ArlExport {

  private val LowerClosedTag = "ARL_INTERVAL_LOWER_CLOSED"
  private val UpperClosedTag = "ARL_INTERVAL_UPPER_CLOSED"

  def toJson(cpg: Cpg, cpgFile: String): String = {
    val methods  = cpg.method.l.filterNot(_.isExternal).sortBy(method => (method.fullName, method.id))
    val types    = cpg.typeDecl.isExternal(false).l.sortBy(typeDecl => (typeDecl.fullName, typeDecl.id))
    val findings = cpg.finding.l.sortBy(_.id)
    val json     = ujson.Obj.from(
      Seq(
        "cpgFile"  -> ujson.Str(cpgFile),
        "methods"  -> ujson.Arr.from(methods.map(methodJson)),
        "types"    -> ujson.Arr.from(types.map(typeDeclJson)),
        "findings" -> ujson.Arr.from(findings.map(findingJson))
      )
    )
    ujson.write(json)
  }

  private def methodJson(method: Method): ujson.Obj = {
    val fields = Seq(
      "id"        -> number(method.id),
      "name"      -> ujson.Str(method.name),
      "fullName"  -> ujson.Str(method.fullName),
      "signature" -> ujson.Str(method.signature),
      "filename"  -> ujson.Str(method.filename),
      "line"      -> optionalNumber(method.lineNumber),
      "lineEnd"   -> optionalNumber(method.lineNumberEnd),
      "arlKind"   -> ujson.Str(arlKind(method)),
      "nodes"     -> ujson.Arr.from(method.ast.l.distinctBy(_.id).sortBy(_.id).map(nodeJson))
    )
    ujson.Obj.from(fields)
  }

  private def typeDeclJson(typeDecl: TypeDecl): ujson.Obj = {
    val inherits = typeDecl.inheritsFromTypeFullName.toList.sorted
    val members  = typeDecl.member.l.sortBy(member => (member.name, member.id))
    ujson.Obj.from(
      Seq(
        "id"       -> number(typeDecl.id),
        "name"     -> ujson.Str(typeDecl.name),
        "fullName" -> ujson.Str(typeDecl.fullName),
        "file"     -> ujson.Str(typeDecl.filename),
        "inherits" -> ujson.Arr.from(inherits.map(ujson.Str(_))),
        "members"  -> ujson.Arr.from(members.map(memberJson))
      )
    )
  }

  private def memberJson(member: Member): ujson.Obj =
    ujson.Obj.from(
      Seq(
        "id"           -> number(member.id),
        "name"         -> ujson.Str(member.name),
        "typeFullName" -> ujson.Str(member.typeFullName),
        "code"         -> ujson.Str(member.code),
        "line"         -> optionalNumber(member.lineNumber)
      )
    )

  private def findingJson(finding: Finding): ujson.Obj = {
    val pairs         = finding.keyValuePairs.map(pair => pair.key -> pair.value).toList
    val duplicateKeys = pairs
      .groupBy(_._1)
      .collect { case (key, values) if values.size > 1 => key }
      .toList
      .sorted
    if (duplicateKeys.nonEmpty) {
      throw new IllegalStateException(
        s"Finding ${finding.id} has duplicate key/value keys: ${duplicateKeys.mkString(", ")}"
      )
    }
    ujson.Obj.from(pairs.sortBy(_._1).map { case (key, value) => key -> ujson.Str(value) })
  }

  private def arlKind(method: Method): String = {
    val annotations = method.annotation.name("arlKind").l.sortBy(_.id)
    val kinds       = annotations
      .flatMap(_.parameterAssign.value.code.l)
      .distinct
      .sorted
    kinds match {
      case Nil         => "unknown"
      case kind :: Nil => kind
      case conflicting =>
        throw new IllegalStateException(
          s"Method ${method.fullName} has conflicting arlKind values: ${conflicting.mkString(", ")}"
        )
    }
  }

  private def nodeJson(node: AstNode): ujson.Obj = {
    val fields = mutable.ListBuffer[(String, ujson.Value)](
      ("id", number(node.id)),
      ("label", ujson.Str(node.label)),
      ("order", number(node.order)),
      ("code", ujson.Str(node.code)),
      ("line", optionalNumber(node.lineNumber)),
      ("columnNumber", optionalNumber(node.columnNumber)),
      (
        "children",
        ujson.Arr.from(node.astChildren.l.sortBy(child => (child.order, child.id)).map(child => number(child.id)))
      )
    )

    node match {
      case cfgNode: CfgNode =>
        fields += ((
          "cfgOut",
          ujson.Arr.from(cfgNode._cfgOut.cast[CfgNode].map(_.id).toList.distinct.sorted.map(number))
        ))
      case _ =>
    }

    node match {
      case identifier: Identifier =>
        fields += (("name", ujson.Str(identifier.name)))
        fields += (("typeFullName", ujson.Str(identifier.typeFullName)))
        fields += (("refs", ujson.Arr.from(identifier._refOut.map(_.id).toList.distinct.sorted.map(number))))
      case local: Local =>
        fields += (("name", ujson.Str(local.name)))
        fields += (("typeFullName", ujson.Str(local.typeFullName)))
      case parameter: MethodParameterIn =>
        fields += (("name", ujson.Str(parameter.name)))
        fields += (("typeFullName", ujson.Str(parameter.typeFullName)))
        fields += (("index", number(parameter.index.toLong)))
      case literal: Literal =>
        fields += (("typeFullName", ujson.Str(literalTypeFullName(literal))))
      case fieldIdentifier: FieldIdentifier =>
        fields += (("canonicalName", ujson.Str(fieldIdentifier.canonicalName)))
      case call: Call =>
        fields += (("name", ujson.Str(call.name)))
        fields += (("methodFullName", ujson.Str(call.methodFullName)))
        fields += (("signature", ujson.Str(call.signature)))
        fields += (("typeFullName", ujson.Str(call.typeFullName)))
        fields += ((
          "arguments",
          ujson.Arr.from(
            call.argument.l
              .sortBy(argument => (argument.argumentIndex, argument.id))
              .map { argument =>
                ujson.Obj("id" -> number(argument.id), "index" -> number(argument.argumentIndex.toLong))
              }
          )
        ))
        fields += ((
          "callees",
          ujson.Arr.from(
            call
              .callee(NoResolve)
              .l
              .distinctBy(_.id)
              .sortBy(callee => (callee.fullName, callee.id))
              .map { callee =>
                ujson.Obj(
                  "id"       -> number(callee.id),
                  "fullName" -> ujson.Str(callee.fullName),
                  "external" -> ujson.Bool(callee.isExternal)
                )
              }
          )
        ))
        val lowerClosed = tagBoolean(call, LowerClosedTag)
        val upperClosed = tagBoolean(call, UpperClosedTag)
        if (call.name != "<operator>.interval" && (lowerClosed.nonEmpty || upperClosed.nonEmpty)) {
          throw new IllegalStateException(s"Non-interval call ${call.id} has interval bracket tags")
        }
        lowerClosed.foreach(value => fields += (("lowerClosed", value)))
        upperClosed.foreach(value => fields += (("upperClosed", value)))
      case control: ControlStructure =>
        fields += (("controlStructureType", ujson.Str(control.controlStructureType)))
        val condition = control.condition.l match {
          case Nil               => ujson.Null
          case condition :: Nil  => number(condition.id)
          case multipleCondition =>
            throw new IllegalStateException(
              s"Control structure ${control.id} has multiple condition nodes: ${multipleCondition.map(_.id).mkString(", ")}"
            )
        }
        fields += (("condition", condition))
      case _ =>
    }

    ujson.Obj.from(fields)
  }

  private def literalTypeFullName(literal: Literal): String =
    if (literal.code.trim == "null") "ANY" else literal.typeFullName

  private def tagBoolean(call: Call, tagName: String): Option[ujson.Value] = {
    val values = call.tag.l.filter(_.name == tagName).map(_.value).distinct.sorted
    values match {
      case Nil            => None
      case "true" :: Nil  => Some(ujson.Bool(true))
      case "false" :: Nil => Some(ujson.Bool(false))
      case _              =>
        throw new IllegalStateException(
          s"Call ${call.id} has invalid or conflicting $tagName tag values: ${values.mkString(", ")}"
        )
    }
  }

  private def number(value: Long): ujson.Value = ujson.Num(value.toDouble)

  private def optionalNumber(value: Option[Int]): ujson.Value =
    value.map(numberValue => number(numberValue.toLong)).getOrElse(ujson.Null)
}
