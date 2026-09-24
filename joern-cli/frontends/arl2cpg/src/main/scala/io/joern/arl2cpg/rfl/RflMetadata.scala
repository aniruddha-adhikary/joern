package io.joern.arl2cpg.rfl

import org.slf4j.LoggerFactory
import org.w3c.dom.{Document, Element}

import java.nio.file.{Files, Path, Paths}
import javax.xml.parsers.DocumentBuilderFactory
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.Try

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
  subflowTargets: Map[String, String], // taskId -> target flow uuid
  path: String
)

object RflMetadata {
  private val logger = LoggerFactory.getLogger(getClass)

  /** Recursively collect and parse every `*.rfl` file under each directory; malformed files are skipped. */
  def load(dirs: Seq[String]): List[RuleflowMeta] = {
    dirs.toList.flatMap { dir =>
      val root = Paths.get(dir)
      if (!Files.isDirectory(root)) {
        logger.warn(s"--rfl-src '$dir' is not a directory; skipping")
        List.empty
      } else {
        val stream = Files.walk(root)
        try {
          stream
            .iterator()
            .asScala
            .filter(path => Files.isRegularFile(path) && path.toString.endsWith(".rfl"))
            .toList
            .flatMap(parse)
        } finally stream.close()
      }
    }
  }

  private def parse(path: Path): Option[RuleflowMeta] = {
    Try {
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
      val subflowTargets = mutable.Map.empty[String, String]
      taskElements(doc).foreach { elem =>
        Option(elem.getAttribute("Identifier")).filter(_.nonEmpty).foreach { id =>
          taskIds += id
          if (elem.getTagName == "SubflowTask") {
            Option(elem.getAttribute("Uuid")).filter(_.nonEmpty).foreach(target => subflowTargets(id) = target)
          }
        }
      }
      RuleflowMeta(name, uuid, taskIds.toSet, subflowTargets.toMap, path.toString)
    } match {
      case scala.util.Success(meta)      => Option(meta)
      case scala.util.Failure(exception) =>
        logger.warn(s"Failed to parse rfl metadata '$path'; skipping", exception)
        None
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
