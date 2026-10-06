package io.joern.x2cpg.frontendspecific.arl2cpg

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, Call, CfgNode, ControlStructure, Method}
import io.shiftleft.semanticcpg.language.*

import scala.collection.mutable

object ArlExport {

  private val LowerClosedTag = "ARL_INTERVAL_LOWER_CLOSED"
  private val UpperClosedTag = "ARL_INTERVAL_UPPER_CLOSED"

  def toJson(cpg: Cpg): String = {
    val methods = cpg.method.l.filterNot(_.isExternal).sortBy(method => (method.fullName, method.id))
    val json    = ujson.Obj("methods" -> ujson.Arr.from(methods.map(methodJson)))
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
      "arlKind"   -> ujson.Str(arlKind(method)),
      "nodes"     -> ujson.Arr.from(method.ast.l.distinctBy(_.id).sortBy(_.id).map(nodeJson))
    )
    ujson.Obj.from(fields)
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
        fields += (("cfgOut", ujson.Arr.from(cfgNode.cfgNext.l.map(_.id).distinct.sorted.map(number))))
      case _ =>
    }

    node match {
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
              .map(argument => number(argument.id))
          )
        ))
        fields += ((
          "callees",
          ujson.Arr.from(call.callee(NoResolve).l.map(_.fullName).distinct.sorted.map(ujson.Str(_)))
        ))
        tagBoolean(call, LowerClosedTag).foreach(value => fields += (("lowerClosed", value)))
        tagBoolean(call, UpperClosedTag).foreach(value => fields += (("upperClosed", value)))
      case control: ControlStructure =>
        fields += (("controlStructureType", ujson.Str(control.controlStructureType)))
        fields += ((
          "condition",
          control.condition.l.headOption.map(condition => number(condition.id)).getOrElse(ujson.Null)
        ))
      case _ =>
    }

    ujson.Obj.from(fields)
  }

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
