package io.joern.arl2cpg.b2x

import org.w3c.dom.{Element, Node}

import java.nio.file.{Files, Path}
import javax.xml.XMLConstants
import javax.xml.parsers.DocumentBuilderFactory
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/** A member of the BOM-to-XOM mapping that carries a body: a `method`, a `constructor`, or an attribute `getter` /
  * `setter` (`kind`). `paramTypes` are the declared parameter types, in order; `body` is the verbatim text.
  */
final case class B2xMember(
  businessClass: String,
  kind: String,
  name: String,
  paramTypes: List[String],
  language: String,
  body: String
) {
  def arity: Int = paramTypes.size

  /** Stable identity: `com.acme.Outcome.rejectWith(com.acme.Reason,int)`. */
  def key: String = s"$businessClass.$name(${paramTypes.mkString(",")})"

  def isArl: Boolean = language.equalsIgnoreCase("arl")
}

/** A mapping element the reader does not model. Not skipped: it becomes a finding, because a mapping we ignore is an
  * effect missing from the graph.
  */
final case class B2xUnhandled(path: String, excerpt: String)

/** The `b2x.b2x` BOM-to-XOM mapping IBM writes to `RULES_ENGINE/default/resources/ruleset/` inside a deployed archive.
  * Ported from arlgraph `B2x.java`: the set of elements read is closed, every other element is recorded in
  * [[unhandled]].
  */
final class B2xModel private (
  val path: String,
  val classes: Set[String],
  val members: List[B2xMember],
  val unhandled: List[B2xUnhandled]
) {

  def hasClass(businessClass: String): Boolean = classes.contains(businessClass)

  /** Method bodies of `businessClass` named `name` taking `arity` arguments. Several means an overload the call site
    * cannot disambiguate: ARL gives argument expressions, not their types.
    */
  def candidates(businessClass: String, name: String, arity: Int): List[B2xMember] =
    members.filter(m => m.businessClass == businessClass && m.name == name && m.arity == arity && m.kind == "method")

  def getter(businessClass: String, attribute: String): Option[B2xMember] =
    members.find(m => m.businessClass == businessClass && m.name == attribute && m.kind == "getter")
}

object B2xModel {

  private val RootChildren  = Set("id", "lang", "class")
  private val ClassChildren =
    Set(
      "businessName",
      "executionName",
      "extenderName",
      "method",
      "constructor",
      "attribute",
      "superClass",
      "genericInfo",
      "import",
      "tester"
    )
  private val MemberChildren    = Set("name", "parameter", "body", "returnType", "static")
  private val AttributeChildren = Set("name", "getter", "setter", "type", "static")

  def parse(path: Path): B2xModel = {
    val factory = DocumentBuilderFactory.newInstance()
    factory.setNamespaceAware(true)
    factory.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true)
    factory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true)
    val document = factory.newDocumentBuilder().parse(Files.newInputStream(path))
    val reader   = new Reader
    reader.readRoot(document.getDocumentElement)
    new B2xModel(path.toString, reader.classes.toSet, reader.members.toList, reader.unhandled.toList)
  }

  private class Reader {
    val classes: mutable.LinkedHashSet[String]      = mutable.LinkedHashSet.empty
    val members: mutable.ListBuffer[B2xMember]      = mutable.ListBuffer.empty
    val unhandled: mutable.ListBuffer[B2xUnhandled] = mutable.ListBuffer.empty

    def readRoot(root: Element): Unit =
      elements(root).foreach { child =>
        val tag = localName(child)
        if (!RootChildren.contains(tag)) unhandled += B2xUnhandled(tag, excerpt(child))
        else if (tag == "class") readClass(child)
      }

    private def readClass(cls: Element): Unit =
      childText(cls, "businessName") match {
        case None               => unhandled += B2xUnhandled("class-without-businessName", excerpt(cls))
        case Some(businessName) =>
          classes += businessName
          elements(cls).foreach { child =>
            localName(child) match {
              case tag if !ClassChildren.contains(tag) => unhandled += B2xUnhandled(s"class/$tag", excerpt(child))
              case "method"                            => readMember(businessName, child, "method")
              case "constructor"                       => readMember(businessName, child, "constructor")
              case "attribute"                         => readAttribute(businessName, child)
              case _                                   => // BOM restates these; no fact derives from them here
            }
          }
      }

    private def readMember(businessClass: String, member: Element, kind: String): Unit = {
      val name = if (kind == "constructor") Some("") else childText(member, "name")
      name match {
        case None             => unhandled += B2xUnhandled(s"$kind-without-name", excerpt(member))
        case Some(memberName) =>
          val params = mutable.ListBuffer.empty[String]
          var body   = Option.empty[Element]
          elements(member).foreach { child =>
            localName(child) match {
              case tag if !MemberChildren.contains(tag) => unhandled += B2xUnhandled(s"$kind/$tag", excerpt(child))
              case "parameter"                          => params += child.getAttribute("type")
              case "body"                               => body = Some(child)
              case _                                    =>
            }
          }
          body.foreach { b =>
            members += B2xMember(
              businessClass,
              kind,
              memberName,
              params.toList,
              b.getAttribute("language"),
              b.getTextContent.trim
            )
          }
      }
    }

    private def readAttribute(businessClass: String, attribute: Element): Unit =
      childText(attribute, "name") match {
        case None       => unhandled += B2xUnhandled("attribute-without-name", excerpt(attribute))
        case Some(name) =>
          elements(attribute).foreach { child =>
            localName(child) match {
              case tag if !AttributeChildren.contains(tag) =>
                unhandled += B2xUnhandled(s"attribute/$tag", excerpt(child))
              case tag @ ("getter" | "setter") =>
                members += B2xMember(
                  businessClass,
                  tag,
                  name,
                  Nil,
                  child.getAttribute("language"),
                  child.getTextContent.trim
                )
              case _ =>
            }
          }
      }
  }

  private def elements(parent: Element): List[Element] = {
    val children = parent.getChildNodes
    (0 until children.getLength).map(children.item).collect { case e: Element => e }.toList
  }

  private def localName(e: Element): String = Option(e.getLocalName).getOrElse(e.getTagName)

  private def childText(parent: Element, tag: String): Option[String] =
    elements(parent).find(localName(_) == tag).map(_.getTextContent.trim)

  private def excerpt(e: Element): String = {
    val text = e.getTextContent.trim.replaceAll("\\s+", " ")
    s"<${localName(e)}>${text.take(60)}"
  }
}
