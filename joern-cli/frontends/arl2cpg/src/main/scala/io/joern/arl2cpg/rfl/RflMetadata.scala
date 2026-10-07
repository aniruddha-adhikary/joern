package io.joern.arl2cpg.rfl

import io.joern.arl2cpg.util.InputFiles
import org.w3c.dom.{Document, Element}

import java.nio.file.{Files, Path, Paths}
import javax.xml.parsers.DocumentBuilderFactory
import scala.collection.mutable
import scala.util.control.NonFatal

/** Original ODM ruleflow metadata parsed from a `.rfl` sidecar file.
  *
  * The ODM build flattens authored ruleflows into one ARL file; two flows may share the same internal task names.
  * Identity is the `<uuid>`, not the `<name>`: tasks are matched against the `Identifier` attributes of the elements
  * under `<TaskList>`, and `<SubflowTask>` additionally carries the uuid of the flow it invokes.
  */
case class RuleflowMeta(
  name: String,
  uuid: String,
  taskIds: Set[String],
  subflowTaskIds: Set[String],
  subflowTargets: Map[String, String], // taskId -> target flow uuid
  path: String
)

object RflMetadata {

  /** Recursively collect and parse every `*.rfl` file under each directory. */
  def load(dirs: Seq[String]): List[RuleflowMeta] = {
    dirs.toList.sorted.flatMap { dir =>
      InputFiles
        .walk(Paths.get(dir), "--rfl-src", path => Files.isRegularFile(path) && path.toString.endsWith(".rfl"))
        .map(parse)
    }
  }

  private def parse(path: Path): RuleflowMeta = {
    try {
      val factory = DocumentBuilderFactory.newInstance()
      factory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true)
      factory.setFeature("http://xml.org/sax/features/external-general-entities", false)
      factory.setFeature("http://xml.org/sax/features/external-parameter-entities", false)
      factory.setXIncludeAware(false)
      factory.setExpandEntityReferences(false)
      val doc = factory.newDocumentBuilder().parse(path.toFile)
      doc.getDocumentElement.normalize()

      val name           = firstChildText(doc, "name").getOrElse("")
      val uuid           = firstChildText(doc, "uuid").getOrElse("")
      val taskIds        = mutable.Set.empty[String]
      val subflowTaskIds = mutable.Set.empty[String]
      val subflowTargets = mutable.Map.empty[String, String]
      taskElements(doc).foreach { elem =>
        Option(elem.getAttribute("Identifier")).filter(_.nonEmpty).foreach { id =>
          taskIds += id
          if (elem.getTagName == "SubflowTask") {
            subflowTaskIds += id
            Option(elem.getAttribute("Uuid")).filter(_.nonEmpty).foreach(target => subflowTargets(id) = target)
          }
        }
      }
      RuleflowMeta(name, uuid, taskIds.toSet, subflowTaskIds.toSet, subflowTargets.toMap, path.toString)
    } catch {
      case NonFatal(exception) =>
        throw new IllegalArgumentException(s"Failed to parse --rfl-src file '$path'", exception)
    }
  }

  /** Text of the first top-level element with the given tag. */
  private def firstChildText(doc: Document, tag: String): Option[String] = {
    val nodes = doc.getDocumentElement.getElementsByTagName(tag)
    Option(nodes.item(0)).map(_.getTextContent.trim).filter(_.nonEmpty)
  }

  /** All elements under any `TaskList` element in the document. */
  private def taskElements(doc: Document): List[Element] = {
    val lists = doc.getElementsByTagName("TaskList")
    (0 until lists.getLength).toList.flatMap { listIdx =>
      val children = lists.item(listIdx).getChildNodes
      (0 until children.getLength).toList.flatMap(childIdx =>
        Option(children.item(childIdx)).collect { case elem: Element => elem }
      )
    }
  }
}
