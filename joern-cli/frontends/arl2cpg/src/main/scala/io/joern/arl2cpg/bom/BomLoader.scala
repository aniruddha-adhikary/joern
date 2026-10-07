package io.joern.arl2cpg.bom

import io.joern.arl2cpg.util.InputFiles
import io.joern.x2cpg.frontendspecific.arl2cpg.ArlFindings.{Codes, Keys}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.util.jar.JarFile
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

object BomModelLoader {

  private final case class BomInput(displayPath: String, identity: String, content: String)

  private final case class BomSource(
    identity: String,
    displayName: String,
    files: Map[String, BomInput],
    directory: Option[Path],
    roots: List[String]
  )

  private final case class RootSelection(classpath: Map[String, List[String]], external: List[BomSource])

  private final case class BomClosure(
    inputs: List[BomInput],
    loadedPaths: Set[String],
    includeFindings: List[BomDiagnostic]
  )

  private val DefaultBomRoot = "ilog/rules/bom/boot.bom"

  /** The engine root is `ilog/rules/bom/boot.bom`: `ilog/rules/factory/IlrReflect.class` references that resource, not
    * `com/ibm/rules/runtime/odm-boot.bom`, which is a separate model root.
    */
  def load(classpath: Seq[String], bomPaths: Seq[String], bomRoots: Seq[String] = Nil): BomModel = {
    val classpathSources = classpath.map(loadClasspathPath).distinctBy(_.identity)
    val selection        =
      if (bomRoots.isEmpty) {
        RootSelection(
          classpathSources.iterator
            .map(source =>
              source.identity -> (if (source.files.contains(DefaultBomRoot)) List(DefaultBomRoot) else Nil)
            )
            .toMap,
          Nil
        )
      } else {
        resolveClasspathRoots(bomRoots, classpathSources)
      }
    val projectSources    = bomPaths.map(loadBomPath)
    val classpathClosures = classpathSources.map { source =>
      source -> closure(source, selection.classpath.getOrElse(source.identity, Nil))
    }
    val externalClosures = selection.external.map(source => source -> closure(source, source.roots))
    val projectClosures  = projectSources.map(source => source -> closure(source, source.roots))

    val selectedClasspathFiles = classpathClosures.flatMap { case (source, selected) =>
      val notLoaded = source.files.keySet.diff(selected.loadedPaths).toList.sorted
      Option.when(notLoaded.nonEmpty)(
        BomDiagnostic(
          Codes.BomFilesNotLoaded,
          source.files(notLoaded.head).displayPath,
          s"BOM files outside the selected root closure for classpath entry '${source.displayName}' were not loaded",
          notLoaded.map(file => Keys.BomFiles -> file)
        )
      )
    }
    val closures       = classpathClosures.map(_._2) ++ externalClosures.map(_._2) ++ projectClosures.map(_._2)
    val selectedInputs = closures.flatMap(_.inputs).distinctBy(_.identity)
    val files          = selectedInputs.map(input => BomParserFacade.parse(input.displayPath, input.content)).toList

    val declarations = files.flatMap(file => file.types.flatMap(_.allTypes.map(BomTypeSource(file.path, _))))
    val grouped      = declarations.groupBy(_.declaration.fullName)
    val duplicates   = grouped.toList.collect { case (fullName, sources) if sources.size > 1 => fullName -> sources }
    val duplicateFindings = duplicates.sortBy(_._1).map { case (fullName, sources) =>
      val sourceFiles = sources.map(_.file).distinct.sorted
      BomDiagnostic(
        Codes.BomDuplicateClass,
        sourceFiles.head,
        s"BOM class '$fullName' is declared in multiple files: ${sourceFiles.mkString(", ")}"
      )
    }
    val duplicateNames  = duplicates.iterator.map(_._1).toSet
    val uniqueTypes     = declarations.filterNot(source => duplicateNames.contains(source.declaration.fullName))
    val includeFindings = closures.flatMap(_.includeFindings)

    new BomModel(files, uniqueTypes, (includeFindings ++ duplicateFindings ++ selectedClasspathFiles).toList)
  }

  private def closure(source: BomSource, roots: List[String]): BomClosure = {
    val visited         = mutable.Set.empty[String]
    val inputs          = mutable.ListBuffer.empty[BomInput]
    val includeFindings = mutable.ListBuffer.empty[BomDiagnostic]

    def visit(relativePath: String): Unit = {
      if (visited.add(relativePath)) {
        source.files.get(relativePath).foreach { input =>
          val file = BomParserFacade.parse(input.displayPath, input.content)
          inputs += input
          file.includes.foreach { include =>
            resolveInclude(relativePath, include, source.files.keySet) match {
              case Some(target) => visit(target)
              case None         =>
                includeFindings += BomDiagnostic(
                  Codes.BomIncludeMissing,
                  file.path,
                  s"BOM include '$include' from '${file.path}' is missing under the same root"
                )
            }
          }
        }
      }
    }

    roots.distinct.sorted.foreach(visit)
    BomClosure(inputs.toList, visited.toSet, includeFindings.toList)
  }

  private def resolveInclude(currentPath: String, include: String, available: Set[String]): Option[String] = {
    val includePath    = normalizedRelative(include)
    val parentPath     = Paths.get(currentPath).getParent
    val relativeToFile =
      Option(parentPath).flatMap(parent => normalizedRelative(parent.resolve(include.replace('\\', '/')).toString))
    (includePath.toList ++ relativeToFile.toList).distinct.find(available.contains)
  }

  private def resolveClasspathRoots(roots: Seq[String], sources: Seq[BomSource]): RootSelection = {
    val selected = mutable.Map.empty[String, mutable.ListBuffer[String]]
    val external = mutable.ListBuffer.empty[BomSource]
    roots.foreach { root =>
      val pathValue          = Paths.get(root)
      val normalized         = normalizedRelative(root)
      val normalizedAbsolute = pathValue.toAbsolutePath.normalize()
      val absolutePath = if (Files.exists(normalizedAbsolute)) normalizedAbsolute.toRealPath() else normalizedAbsolute
      val matchingSources = sources.flatMap { source =>
        val pathFromDirectory = source.directory
          .filter(_ => pathValue.isAbsolute || Files.exists(absolutePath))
          .filter(directory => absolutePath.startsWith(directory))
          .flatMap(directory => normalizedRelative(directory.relativize(absolutePath).toString))
        val relativeEntry = normalized.filter(source.files.contains)
        pathFromDirectory.orElse(relativeEntry).filter(source.files.contains).map(source -> _)
      }
      if (matchingSources.nonEmpty) {
        matchingSources.foreach { case (source, relativePath) =>
          selected.getOrElseUpdate(source.identity, mutable.ListBuffer.empty) += relativePath
        }
      } else {
        if (!Files.isRegularFile(absolutePath) || !absolutePath.getFileName.toString.endsWith(".bom")) {
          throw new IllegalArgumentException(
            s"--bom-root '$root' was not found in an --xom-classpath entry or as a BOM file path"
          )
        }
        external += loadExternalRoot(absolutePath)
      }
    }
    RootSelection(selected.view.mapValues(_.distinct.toList).toMap, external.toList)
  }

  private def loadClasspathPath(pathString: String): BomSource = {
    val path = Paths.get(pathString).toAbsolutePath.normalize()
    if (!Files.exists(path)) {
      throw new IllegalArgumentException(s"--xom-classpath path '$pathString' does not exist")
    } else if (Files.isDirectory(path)) {
      loadDirectory(path, roots = Nil, optionName = "--xom-classpath", allowNoBom = true)
    } else if (Files.isRegularFile(path)) {
      loadJar(path)
    } else {
      throw new IllegalArgumentException(s"--xom-classpath path '$pathString' is not a file or directory")
    }
  }

  private def loadBomPath(pathString: String): BomSource = {
    val path = Paths.get(pathString).toAbsolutePath.normalize()
    if (!Files.exists(path)) {
      throw new IllegalArgumentException(s"--bom path '$pathString' does not exist")
    } else if (Files.isDirectory(path)) {
      val source = loadDirectory(path, roots = Nil, optionName = "--bom")
      source.copy(roots = source.files.keys.toList.sorted)
    } else if (Files.isRegularFile(path)) {
      val root   = path.getParent
      val source = loadDirectory(root, roots = Nil, optionName = "--bom")
      source.copy(roots = List(relative(root, path)))
    } else {
      throw new IllegalArgumentException(s"--bom path '$pathString' is not a file or directory")
    }
  }

  private def loadDirectory(
    root: Path,
    roots: List[String],
    optionName: String,
    allowNoBom: Boolean = false
  ): BomSource = {
    val paths = if (allowNoBom) {
      InputFiles
        .walk(root, optionName, Files.isRegularFile(_))
        .filter(_.getFileName.toString.endsWith(".bom"))
    } else {
      bomPathsUnder(root, optionName)
    }
    val normalizedRoot = root.toRealPath()
    val files          = paths.map { path =>
      val relativePath = relative(root, path)
      relativePath -> BomInput(relativePath, path.toRealPath().toString, Files.readString(path, StandardCharsets.UTF_8))
    }.toMap
    BomSource(s"dir:$normalizedRoot", root.getFileName.toString, files, Some(normalizedRoot), roots)
  }

  private def loadExternalRoot(path: Path): BomSource = {
    val root  = path.getParent
    val files = bomPathsUnder(root, "--bom-root").map { file =>
      val relativePath = relative(root, file)
      relativePath -> BomInput(relativePath, file.toRealPath().toString, Files.readString(file, StandardCharsets.UTF_8))
    }.toMap
    BomSource(
      s"external:${path.toRealPath()}",
      root.getFileName.toString,
      files,
      Some(root.toRealPath()),
      List(relative(root, path))
    )
  }

  private def loadJar(path: Path): BomSource = {
    try {
      val jar = new JarFile(path.toFile)
      try {
        val entries = jar
          .entries()
          .asScala
          .filter(entry => !entry.isDirectory && entry.getName.endsWith(".bom"))
          .toList
          .sortBy(_.getName)
        val files = entries.map { entry =>
          val stream = jar.getInputStream(entry)
          val bytes  = try stream.readAllBytes()
          finally stream.close()
          val entryName = entry.getName.replace('\\', '/')
          entryName -> BomInput(
            s"${path.getFileName}!/$entryName",
            s"${path.toRealPath()}!/$entryName",
            new String(bytes, StandardCharsets.UTF_8)
          )
        }.toMap
        BomSource(s"jar:${path.toRealPath()}", path.getFileName.toString, files, None, Nil)
      } finally {
        jar.close()
      }
    } catch {
      case NonFatal(exception) =>
        throw new IllegalArgumentException(s"--xom-classpath jar '$path' is unreadable", exception)
    }
  }

  private def bomPathsUnder(root: Path, optionName: String): List[Path] =
    InputFiles.walk(root, optionName, path => Files.isRegularFile(path) && path.getFileName.toString.endsWith(".bom"))

  private def relative(root: Path, path: Path): String =
    root.relativize(path).toString.replace('\\', '/')

  private def normalizedRelative(value: String): Option[String] =
    try {
      val path = Paths.get(value.replace('\\', '/')).normalize()
      val name = path.toString.replace('\\', '/')
      Option.when(!path.isAbsolute && name.nonEmpty && name != "." && name != ".." && !name.startsWith("../"))(name)
    } catch {
      case NonFatal(_) => None
    }
}
