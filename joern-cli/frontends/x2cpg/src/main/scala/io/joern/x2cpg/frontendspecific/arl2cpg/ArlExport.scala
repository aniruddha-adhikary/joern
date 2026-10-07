package io.joern.x2cpg.frontendspecific.arl2cpg

import io.joern.x2cpg.frontendspecific.arl2cpg.ArlFindings
import io.joern.x2cpg.frontendspecific.arl2cpg.ArlFindings.{Codes, Keys}
import io.joern.x2cpg.frontendspecific.arl2cpg.ArlTags
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

import java.nio.charset.StandardCharsets
import java.nio.file.Paths
import java.security.MessageDigest
import scala.collection.mutable

/** Deterministic export contract: the root has `cpgFile` (filename only), `methods`, `types`, `b2xAttributes`, and
  * `findings`. Methods are sorted by `(fullName, id)` and contain `id`, `stableId`, `stableKey`, `name`, `fullName`,
  * `signature`, `filename`, `line`, `lineEnd`, the METHOD's `arlKind`, and `nodes` (sorted by id). B2X attributes are
  * identified by the `ARL_B2X_ATTRIBUTE` tag; a reused Java METHOD keeps its original `arlKind`. Types are sorted by
  * `(fullName, id)` and contain `id`, `stableId`, `name`, `fullName`, `file`, sorted `inherits`, and `members` sorted
  * by `(name, id)` with `id`, `stableId`, `name`, `typeFullName`, `code`, and `line`. `b2xAttributes` is sorted by
  * `(astParentFullName, name, id)` and contains `class`, `name`, `type`, `methodStableId`, `shadowsMethod` (true when
  * the tagged METHOD's `arlKind` is not `b2x-attribute`), and an optional `literal` object with `kind` and optional
  * `value`. Findings are sorted by node id and contain sorted key/value fields, with duplicate non-list-valued keys
  * rejected. `candidates` is a sorted JSON array, always present on `unresolved-call-target` findings; `bomFiles` is a
  * JSON array on `bom-files-not-loaded` findings. Findings with `callId` also contain the referenced call's
  * `callStableId`.
  *
  * Every node has `id`, `stableId`, `label`, `order`, `code`, `line`, `columnNumber`, and AST-child ids sorted by
  * `(order, id)`; CFG nodes add sorted `cfgOut`. CALL adds `name`, `methodFullName`, `signature`, `typeFullName`,
  * arguments as `{id, index}` sorted by `(index, id)`, and callees as `{id, fullName, external}` sorted by
  * `(fullName, id)`; interval CALLs alone may add boolean `lowerClosed`/`upperClosed`. IDENTIFIER adds `name`,
  * `typeFullName`, and sorted reference ids; LOCAL adds `name` and `typeFullName`; METHOD_PARAMETER_IN adds `name`,
  * `typeFullName`, and `index`; LITERAL adds `typeFullName`; FIELD_IDENTIFIER adds `canonicalName`; CONTROL_STRUCTURE
  * adds `controlStructureType` and `condition`.
  *
  * All ids are CPG node ids encoded as JSON numbers. Missing positions and conditions are JSON null. Null literals
  * export `typeFullName` as `ANY`.
  *
  * Stable IDs are lowercase hexadecimal encodings of the first 16 bytes of SHA-256 over each key's UTF-8 bytes. A
  * METHOD key is `METHOD:<fullName>@<filename>`; methods sharing a full name and filename receive `#<ordinal>` suffixes
  * ordered by line and column. A TYPE_DECL key is `TYPE_DECL:<fullName>@<filename>`; and a MEMBER key is its parent
  * TYPE_DECL key followed by `/MEMBER:<name>`. An AST node under a method is keyed by the nearest enclosing METHOD key
  * followed by `/<LABEL>:<order>` for every AST step from that METHOD down to the node. Absolute filenames in keys are
  * made relative to `cpg.metaData.root`; exported filename fields are unchanged. Duplicate sibling `(LABEL, order)`
  * pairs and duplicate exported stable IDs throw instead of receiving tie-breakers.
  */
object ArlExport {

  private val LowerClosedTag = "ARL_INTERVAL_LOWER_CLOSED"
  private val UpperClosedTag = "ARL_INTERVAL_UPPER_CLOSED"

  def toJson(cpg: Cpg, cpgFile: String): String = {
    val methods             = cpg.method.l.filterNot(_.isExternal).sortBy(method => (method.fullName, method.id))
    val types               = cpg.typeDecl.isExternal(false).l.sortBy(typeDecl => (typeDecl.fullName, typeDecl.id))
    val findings            = cpg.finding.l.sortBy(_.id)
    val twinOrdinals        = methodTwinOrdinals(cpg, methods)
    val stableIds           = new StableIdRegistry
    val methodObjects       = methods.map(method => methodJson(cpg, method, stableIds, twinOrdinals))
    val typeObjects         = types.map(typeDecl => typeDeclJson(cpg, typeDecl, stableIds))
    val stableIdByNode      = stableIds.byNodeIdStableId
    val b2xAttributeObjects = methods
      .filter(method => method.tag.l.exists(_.name == ArlTags.B2xAttribute))
      .sortBy(method => (method.astParentFullName, method.name, method.id))
      .map(method => b2xAttributeJson(method, stableIdByNode))
    val findingObjects = findings.map(finding => findingJson(finding, stableIdByNode))
    val json           = ujson.Obj.from(
      Seq(
        "cpgFile"       -> ujson.Str(cpgFile),
        "methods"       -> ujson.Arr.from(methodObjects),
        "types"         -> ujson.Arr.from(typeObjects),
        "b2xAttributes" -> ujson.Arr.from(b2xAttributeObjects),
        "findings"      -> ujson.Arr.from(findingObjects)
      )
    )
    ujson.write(json)
  }

  private def methodJson(
    cpg: Cpg,
    method: Method,
    stableIds: StableIdRegistry,
    twinOrdinals: Map[Long, Int]
  ): ujson.Obj = {
    val stableKey = methodStableKey(cpg, method, twinOrdinals)
    val fields    = Seq(
      "id"        -> number(method.id),
      "stableId"  -> ujson.Str(stableIds.add(method.id, stableKey)),
      "stableKey" -> ujson.Str(stableKey),
      "name"      -> ujson.Str(method.name),
      "fullName"  -> ujson.Str(method.fullName),
      "signature" -> ujson.Str(method.signature),
      "filename"  -> ujson.Str(method.filename),
      "line"      -> optionalNumber(method.lineNumber),
      "lineEnd"   -> optionalNumber(method.lineNumberEnd),
      "arlKind"   -> ujson.Str(arlKind(method)),
      "nodes"     -> ujson.Arr.from(
        method.ast.l
          .distinctBy(_.id)
          .sortBy(_.id)
          .map(node => nodeJson(cpg, node, stableIds, twinOrdinals))
      )
    )
    ujson.Obj.from(fields)
  }

  private def typeDeclJson(cpg: Cpg, typeDecl: TypeDecl, stableIds: StableIdRegistry): ujson.Obj = {
    val inherits = typeDecl.inheritsFromTypeFullName.toList.sorted
    val members  = typeDecl.member.l.sortBy(member => (member.name, member.id))
    ujson.Obj.from(
      Seq(
        "id"       -> number(typeDecl.id),
        "stableId" -> ujson.Str(stableIds.add(typeDecl.id, typeDeclStableKey(cpg, typeDecl))),
        "name"     -> ujson.Str(typeDecl.name),
        "fullName" -> ujson.Str(typeDecl.fullName),
        "file"     -> ujson.Str(typeDecl.filename),
        "inherits" -> ujson.Arr.from(inherits.map(ujson.Str(_))),
        "members"  -> ujson.Arr.from(members.map(member => memberJson(cpg, member, typeDecl, stableIds)))
      )
    )
  }

  private def b2xAttributeJson(method: Method, stableIdsByNodeId: Map[Long, String]): ujson.Obj = {
    val kindValues  = method.tag.l.filter(_.name == ArlTags.LiteralKind).map(_.value).distinct.sorted
    val valueValues = method.tag.l.filter(_.name == ArlTags.LiteralValue).map(_.value).distinct.sorted
    if (kindValues.size > 1 || valueValues.size > 1) {
      throw new IllegalStateException(
        s"B2X attribute METHOD ${method.fullName} has conflicting literal tags: " +
          s"kinds=${kindValues.mkString(",")}, values=${valueValues.mkString(",")}"
      )
    }
    val literalFields = kindValues match {
      case Nil if valueValues.nonEmpty =>
        throw new IllegalStateException(s"B2X attribute METHOD ${method.fullName} has a literal value without a kind")
      case Nil         => Nil
      case kind :: Nil =>
        val fields = List("kind" -> ujson.Str(kind))
        fields ++ valueValues.headOption.map(value => "value" -> ujson.Str(value))
      case _ => throw new IllegalStateException(s"B2X attribute METHOD ${method.fullName} has invalid literal tags")
    }
    val fields = List(
      "class"          -> ujson.Str(method.astParentFullName),
      "name"           -> ujson.Str(method.name),
      "type"           -> ujson.Str(method.methodReturn.typeFullName),
      "shadowsMethod"  -> ujson.Bool(arlKind(method) != "b2x-attribute"),
      "methodStableId" -> ujson.Str(
        stableIdsByNodeId.getOrElse(
          method.id,
          throw new IllegalStateException(s"B2X attribute METHOD ${method.id} has no exported stableId")
        )
      )
    )
    val withLiteral =
      if (literalFields.isEmpty) fields
      else fields :+ ("literal" -> ujson.Obj.from(literalFields))
    ujson.Obj.from(withLiteral)
  }

  private def memberJson(cpg: Cpg, member: Member, typeDecl: TypeDecl, stableIds: StableIdRegistry): ujson.Obj =
    ujson.Obj.from(
      Seq(
        "id"       -> number(member.id),
        "stableId" -> ujson.Str(stableIds.add(member.id, s"${typeDeclStableKey(cpg, typeDecl)}/MEMBER:${member.name}")),
        "name"     -> ujson.Str(member.name),
        "typeFullName" -> ujson.Str(member.typeFullName),
        "code"         -> ujson.Str(member.code),
        "line"         -> optionalNumber(member.lineNumber)
      )
    )

  private def findingJson(finding: Finding, stableIdsByNodeId: Map[Long, String]): ujson.Obj = {
    val pairs         = finding.keyValuePairs.map(pair => pair.key -> pair.value).toList
    val valuesByKey   = pairs.groupMap(_._1)(_._2)
    val duplicateKeys = valuesByKey
      .collect { case (key, values) if values.size > 1 && !ArlFindings.ListValuedKeys.contains(key) => key }
      .toList
      .sorted
    if (duplicateKeys.nonEmpty) {
      throw new IllegalStateException(
        s"Finding ${finding.id} has duplicate key/value keys: ${duplicateKeys.mkString(", ")}"
      )
    }
    val withCallStableId = pairs.find(_._1 == "callId") match {
      case None              => pairs
      case Some((_, callId)) =>
        val callNodeId = callId.toLongOption.getOrElse {
          throw new IllegalStateException(s"Finding ${finding.id} has invalid callId '$callId'")
        }
        val callStableId = stableIdsByNodeId.getOrElse(
          callNodeId,
          throw new IllegalStateException(s"Finding ${finding.id} references unexported call node $callNodeId")
        )
        pairs :+ ("callStableId" -> callStableId)
    }
    val outputValuesByKey = withCallStableId.groupMap(_._1)(_._2)
    val code              = outputValuesByKey.get(Keys.Code).flatMap(_.headOption).getOrElse("")
    val requiredListKeys  =
      if (code == Codes.UnresolvedCallTarget) Set(Keys.Candidates) else Set.empty[String]
    val outputKeys = outputValuesByKey.keySet ++ requiredListKeys
    ujson.Obj.from(outputKeys.toList.sorted.map { key =>
      val values = outputValuesByKey.getOrElse(key, Nil)
      val value  =
        if (ArlFindings.ListValuedKeys.contains(key)) ujson.Arr.from(values.sorted.map(ujson.Str(_)))
        else ujson.Str(values.head)
      key -> value
    })
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

  private def nodeJson(
    cpg: Cpg,
    node: AstNode,
    stableIds: StableIdRegistry,
    twinOrdinals: Map[Long, Int]
  ): ujson.Obj = {
    val fields = mutable.ListBuffer[(String, ujson.Value)](
      ("id", number(node.id)),
      ("stableId", ujson.Str(stableIds.add(node.id, astNodeStableKey(cpg, node, twinOrdinals)))),
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
        fields += (("typeFullName", ujson.Str(literal.typeFullName)))
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

  private def methodTwinOrdinals(cpg: Cpg, methods: List[Method]): Map[Long, Int] =
    methods
      .groupBy(method => (method.fullName, stableFilename(cpg, method.filename)))
      .toList
      .filter(_._2.size > 1)
      .sortBy(_._1)
      .flatMap { case (_, twins) =>
        val positioned = twins.map { method =>
          val position = for {
            line   <- method.lineNumber
            column <- method.columnNumber
          } yield line -> column
          method -> position
        }
        val missingPositions = positioned.collect { case (method, None) => method }
        if (missingPositions.nonEmpty) {
          val names = missingPositions.map(describeMethod).sorted.mkString(", ")
          throw new IllegalStateException(s"Twin methods are missing a line or column position: $names")
        }

        val ordered = positioned
          .collect { case (method, Some((line, column))) => (method, line, column) }
          .sortBy { case (_, line, column) => (line, column) }
        val duplicatePositions = ordered
          .groupBy { case (_, line, column) => (line, column) }
          .collect {
            case ((line, column), samePosition) if samePosition.size > 1 =>
              s"($line, $column): ${samePosition.map(_._1).map(describeMethod).sorted.mkString(", ")}"
          }
          .toList
          .sorted
        if (duplicatePositions.nonEmpty) {
          throw new IllegalStateException(s"Twin methods share source positions: ${duplicatePositions.mkString("; ")}")
        }

        ordered.zipWithIndex.map { case ((method, _, _), ordinal) => method.id -> ordinal }
      }
      .toMap

  private def describeMethod(method: Method): String =
    s"${method.fullName}@${method.filename} (id=${method.id})"

  private def methodStableKey(cpg: Cpg, method: Method, twinOrdinals: Map[Long, Int]): String = {
    val base = s"METHOD:${method.fullName}@${stableFilename(cpg, method.filename)}"
    twinOrdinals.get(method.id).fold(base)(ordinal => s"$base#$ordinal")
  }

  private def typeDeclStableKey(cpg: Cpg, typeDecl: TypeDecl): String =
    s"TYPE_DECL:${typeDecl.fullName}@${stableFilename(cpg, typeDecl.filename)}"

  private def astNodeStableKey(cpg: Cpg, node: AstNode, twinOrdinals: Map[Long, Int]): String = {
    var current: AstNode = node
    var path             = List.empty[AstNode]
    var enclosingMethod  = Option.empty[Method]
    while (enclosingMethod.isEmpty) {
      current match {
        case method: Method => enclosingMethod = Some(method)
        case _              =>
          path = current :: path
          current = current.astParent
      }
    }
    val method = enclosingMethod.get
    val key = methodStableKey(cpg, method, twinOrdinals) + path.map(child => s"/${child.label}:${child.order}").mkString
    path.foreach { child =>
      val siblings = child.astParent.astChildren.l
        .filter(sibling => sibling.label == child.label && sibling.order == child.order)
        .distinctBy(_.id)
      if (siblings.size > 1) {
        throw new IllegalStateException(
          s"AST siblings ${siblings.map(_.id).sorted.mkString(", ")} share (${child.label}, ${child.order}) for key '$key'"
        )
      }
    }
    key
  }

  private def stableFilename(cpg: Cpg, filename: String): String = {
    val path       = Paths.get(filename).normalize()
    val stablePath =
      if (path.isAbsolute) {
        val root = Paths
          .get(cpg.metaData.root.headOption.getOrElse {
            throw new IllegalStateException(
              s"Cannot derive a relative stable filename for '$filename': CPG root is absent"
            )
          })
          .toAbsolutePath
          .normalize()
        try root.relativize(path.toAbsolutePath.normalize())
        catch {
          case exception: IllegalArgumentException =>
            throw new IllegalStateException(
              s"Cannot derive a relative stable filename for '$filename' against CPG root '$root'",
              exception
            )
        }
      } else path
    stablePath.toString.replace('\\', '/')
  }

  private final class StableIdRegistry {
    private val byStableId = mutable.Map.empty[String, (Long, String)]
    private val byNodeId   = mutable.Map.empty[Long, (String, String)]

    def add(nodeId: Long, key: String): String = {
      byNodeId.get(nodeId) match {
        case Some((existingKey, stableId)) if existingKey == key => stableId
        case Some((existingKey, _))                              =>
          throw new IllegalStateException(s"Node $nodeId has conflicting stable-ID keys '$existingKey' and '$key'")
        case None =>
          val stableId = stableIdForKey(key)
          byStableId.get(stableId).foreach { case (otherNodeId, otherKey) =>
            throw new IllegalStateException(
              s"Exported nodes $otherNodeId and $nodeId share stableId $stableId for keys '$otherKey' and '$key'"
            )
          }
          byStableId(stableId) = nodeId -> key
          byNodeId(nodeId) = key        -> stableId
          stableId
      }
    }

    def byNodeIdStableId: Map[Long, String] = byNodeId.view.mapValues(_._2).toMap
  }

  private def stableIdForKey(key: String): String =
    MessageDigest
      .getInstance("SHA-256")
      .digest(key.getBytes(StandardCharsets.UTF_8))
      .take(16)
      .map(byte => f"${byte & 0xff}%02x")
      .mkString

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
