package io.joern.arl2cpg.util

import java.nio.file.{FileVisitOption, Files, Path}
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

object InputFiles {

  def walk(root: Path, optionName: String, accept: Path => Boolean): List[Path] = {
    if (!Files.exists(root)) {
      throw new IllegalArgumentException(s"$optionName path '$root' does not exist")
    }
    if (!Files.isDirectory(root)) {
      throw new IllegalArgumentException(s"$optionName path '$root' is not a directory")
    }
    val paths = try {
      val stream = Files.walk(root, FileVisitOption.FOLLOW_LINKS)
      try stream.iterator().asScala.filter(accept).toList.sortBy(_.toString)
      finally stream.close()
    } catch {
      case NonFatal(exception) =>
        throw new IllegalArgumentException(s"Failed to walk $optionName path '$root'", exception)
    }
    if (paths.isEmpty) {
      throw new IllegalArgumentException(s"$optionName path '$root' contains no matching files")
    }
    paths
  }
}
