package io.joern.arl2cpg.passes.resolution

import io.joern.arl2cpg.util.InputFiles
import io.joern.x2cpg.frontendspecific.arl2cpg.ArlFindings.Codes
import io.joern.arl2cpg.bom.{
  BomDiagnostic,
  BomMember,
  BomMemberKind,
  BomModel,
  BomProperty,
  BomTypeDecl,
  BomTypeRef,
  BomTypeSource
}
import io.shiftleft.codepropertygraph.generated.nodes.TypeDecl
import io.shiftleft.semanticcpg.language.*
import org.objectweb.asm.{ClassReader, ClassVisitor, FieldVisitor, MethodVisitor, Opcodes, Type}
import org.slf4j.LoggerFactory

import java.net.URI
import java.nio.file.{FileSystems, Files, Path, Paths}
import java.util.jar.JarFile
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

final case class BomOrigin(file: String, translation: Option[String])

final case class JavaMethodInfo(
  owner: String,
  name: String,
  paramTypes: List[String],
  returnType: String,
  isStatic: Boolean,
  isVarargs: Boolean,
  fullName: String,
  signature: String,
  isPublic: Boolean,
  bom: Option[BomOrigin] = None
)

final case class JavaTypeInfo(
  fullName: String,
  superClass: Option[String],
  interfaces: Seq[String],
  isInterface: Boolean,
  methods: Seq[JavaMethodInfo],
  fields: Map[String, String]
)

/** Java type information from the XOM CPG, ARL signature members, ordered classpath entries, and the running JDK. */
final class TypeModel(sourceDecls: Seq[TypeDecl], classpath: Seq[String], bom: BomModel = BomModel.empty) {

  private val logger = LoggerFactory.getLogger(getClass)

  private sealed trait ClassLocation
  private final case class DirectoryClass(root: Path, relativePath: Path) extends ClassLocation
  private final case class JarClass(jar: Path, entryName: String)         extends ClassLocation

  private val sourceTypes: Map[String, JavaTypeInfo] =
    sourceDecls
      .sortBy(td => (td.fullName, td.filename))
      .map(sourceTypeInfo)
      .map(info => info.fullName -> info)
      .toMap

  private val sourceNamesBySimpleName: Map[String, Seq[String]] =
    sourceDecls
      .filterNot(_.code.startsWith("signature "))
      .map(_.fullName)
      .distinct
      .sorted
      .groupBy(_.split('.').last)

  private val classpathIndex: Map[String, ClassLocation] = indexClasspath()
  private val parsedClasspathTypes                       = mutable.Map.empty[String, Option[JavaTypeInfo]]
  private val parsedJdkTypes                             = mutable.Map.empty[String, Option[JavaTypeInfo]]
  private val bomTypeNames                               = bom.types.map(_.declaration.fullName).toSet
  private val bomTypeDiagnostics                         = mutable.ListBuffer.empty[BomDiagnostic]
  private lazy val bomTypes: Map[String, JavaTypeInfo]   = {
    bom.types
      .sortBy(source => (source.declaration.fullName, source.file))
      .map(source => source.declaration.fullName -> bomTypeInfo(source))
      .toMap
  }

  def bomDiagnostics: List[BomDiagnostic] = bomTypeDiagnostics.toList

  private lazy val jrtFileSystem =
    try Some(FileSystems.getFileSystem(URI.create("jrt:/")))
    catch {
      case NonFatal(exception) =>
        logger.warn("Unable to access the running JDK's jrt:/ filesystem", exception)
        None
    }

  def xomTypeNames: Set[String] = sourceTypes.keySet

  def uniqueXomTypeForSimpleName(name: String): Option[String] =
    sourceNamesBySimpleName.get(name).filter(_.size == 1).flatMap(_.headOption)

  def get(fullName: String): Option[JavaTypeInfo] =
    getExact(fullName)
      .orElse {
        nestedBinaryNameVariants(fullName).flatMap(getExact).take(1).toList.headOption
      }
      .orElse(arrayType(fullName))

  private def getExact(fullName: String): Option[JavaTypeInfo] = {
    val javaType = sourceTypes
      .get(fullName)
      .orElse {
        parsedClasspathTypes.getOrElseUpdate(fullName, classpathIndex.get(fullName).flatMap(readClass))
      }
      .orElse {
        parsedJdkTypes.getOrElseUpdate(fullName, readJdkClass(fullName))
      }
    (javaType, bomTypes.get(fullName)) match {
      case (Some(java), Some(bomType)) => Some(mergeBomType(java, bomType))
      case (Some(java), None)          => Some(java)
      case (None, Some(bomType))       => Some(bomType)
      case (None, None)                => None
    }
  }

  private def nestedBinaryNameVariants(fullName: String): Iterator[String] = {
    val dots = fullName.indices.filter(index => fullName(index) == '.')
    dots.indices.iterator.map { index =>
      dots.takeRight(index + 1).foldLeft(fullName)((name, dot) => name.updated(dot, '$'))
    }
  }

  def isPrimitive(typeName: String): Boolean = PrimitiveTypes.contains(typeName)

  def isReference(typeName: String): Boolean = typeName != "null" && !isPrimitive(typeName) && typeName != "void"

  def isSubtype(from: String, to: String): Boolean = {
    if (from == to) true
    else if (from == "null") isReference(to)
    else if (from.endsWith("[]") && to.endsWith("[]")) {
      val fromComponent = from.dropRight(2)
      val toComponent   = to.dropRight(2)
      if (isPrimitive(fromComponent) || isPrimitive(toComponent)) fromComponent == toComponent
      else isSubtype(fromComponent, toComponent)
    } else if (from.endsWith("[]")) {
      Set("java.lang.Object", "java.lang.Cloneable", "java.io.Serializable").contains(to)
    } else if (isPrimitive(from) || isPrimitive(to)) {
      false
    } else {
      val visited                         = mutable.Set.empty[String]
      def visit(current: String): Boolean =
        current == to || (visited.add(current) && get(current).exists { info =>
          directSupertypes(info).exists(visit)
        })
      visit(from)
    }
  }

  def methodsFor(typeName: String, name: String): List[JavaMethodInfo] = {
    val declared =
      if (name == "<init>")
        get(typeName).toList.flatMap(_.methods.filter(method => method.name == name && method.isPublic))
      else hierarchy(typeName).flatMap(_.methods.filter(method => method.name == name && method.isPublic))
    val withObjectMethods =
      if (name != "<init>" && get(typeName).exists(_.isInterface))
        get("java.lang.Object").toList.flatMap(_.methods.filter(method => method.name == name && method.isPublic))
      else Nil
    val seen = mutable.Set.empty[(String, List[String])]
    (declared ++ withObjectMethods)
      .filter(method => seen.add(method.name -> method.paramTypes))
      .toList
      .sortBy(method => (method.owner, method.fullName))
  }

  def hierarchyNames(typeName: String): List[String] = hierarchy(typeName).map(_.fullName)

  def hasMethodNamedInHierarchy(typeName: String, name: String): Boolean =
    hierarchy(typeName).exists(_.methods.exists(_.name == name))

  def memberType(typeName: String, memberName: String): Option[String] =
    hierarchy(typeName).iterator
      .flatMap { info =>
        info.fields.get(memberName).filter(isUsableMemberType).iterator ++
          info.methods.iterator
            .filter(method =>
              method.isPublic && method.paramTypes.isEmpty && method.name == s"get${capitalize(memberName)}" &&
                isUsableMemberType(method.returnType)
            )
            .map(_.returnType) ++
          info.methods.iterator
            .filter(method =>
              method.isPublic && method.paramTypes.isEmpty && method.name == s"is${capitalize(memberName)}" &&
                isUsableMemberType(method.returnType)
            )
            .map(_.returnType)
      }
      .find(_.nonEmpty)

  private def hierarchy(typeName: String): List[JavaTypeInfo] = {
    val seen                                       = mutable.Set.empty[String]
    def visit(current: String): List[JavaTypeInfo] =
      if (!seen.add(current)) Nil
      else
        get(current).toList.flatMap { info =>
          info +: directSupertypes(info).flatMap(visit)
        }
    visit(typeName)
  }

  private def directSupertypes(info: JavaTypeInfo): List[String] = {
    val interfaces = info.interfaces.toList.sorted
    if (info.fullName == "java.lang.Object") Nil
    else if (info.isInterface) interfaces :+ "java.lang.Object"
    else {
      val superClass = info.superClass.toList
      val explicit   = superClass ++ interfaces
      if (superClass.contains("java.lang.Object")) explicit
      else explicit :+ "java.lang.Object"
    }
  }

  private def sourceTypeInfo(typeDecl: TypeDecl): JavaTypeInfo = {
    val isInterface = typeDecl.code.contains("interface ")
    val parents     = typeDecl.inheritsFromTypeFullName.toList
    val superClass  =
      if (isInterface) None
      else parents.headOption.orElse(Option.when(typeDecl.fullName != "java.lang.Object")("java.lang.Object"))
    val interfaces =
      if (isInterface) parents
      else parents.drop(superClass.size)

    val methods = typeDecl.method.l.sortBy(_.fullName).map { method =>
      val parameters = method.parameter.l.sortBy(_.index).filterNot(parameter => parameter.name == "this")
      val paramTypes = parameters.map(_.typeFullName)
      val signature  = method.signature
      val modifiers  = method.modifier.map(_.modifierType).toSet
      JavaMethodInfo(
        typeDecl.fullName,
        method.name,
        paramTypes,
        method.methodReturn.typeFullName,
        method.modifier.exists(_.modifierType == "STATIC"),
        parameters.lastOption.exists(_.isVariadic),
        method.fullName,
        signature,
        modifiers.contains("PUBLIC") || (isInterface && !modifiers.contains("PRIVATE"))
      )
    }
    val fields = typeDecl.member.l.sortBy(_.name).map(member => member.name -> member.typeFullName).toMap
    JavaTypeInfo(typeDecl.fullName, superClass, interfaces.sorted, isInterface, methods, fields)
  }

  private def bomTypeInfo(source: BomTypeSource): JavaTypeInfo = {
    val declaration    = source.declaration
    val typeParameters = declaration.typeParameters.map(parameter => parameter.name -> parameter).toMap

    def resolve(reference: BomTypeRef, context: String, resolving: Set[String] = Set.empty): Option[String] = {
      val rawName                  = reference.name
      val baseName: Option[String] = rawName match {
        case "object"                                                => Some("java.lang.Object")
        case "string"                                                => Some("java.lang.String")
        case name if name == "void" || PrimitiveTypes.contains(name) => Some(name)
        case name if typeParameters.contains(name)                   =>
          if (resolving.contains(name)) None
          else
            typeParameters(name).bounds.headOption
              .flatMap(bound => resolve(bound, context, resolving + name))
              .orElse(Some("java.lang.Object"))
        case name if !name.contains(".") =>
          Some(if (declaration.packageName.nonEmpty) s"${declaration.packageName}.$name" else name)
        case name => Some(name)
      }
      val resolvedBase = baseName.flatMap { candidate =>
        if (candidate == "void" || PrimitiveTypes.contains(candidate)) Some(candidate)
        else if (rawName == "object" || rawName == "string") Some(candidate)
        else if (typeParameters.contains(rawName) && !resolving.contains(rawName)) Some(candidate)
        else if (!rawName.contains(".") && !typeParameters.contains(rawName)) {
          Option.when(bomTypeNames.contains(candidate))(candidate)
        } else knownTypeName(candidate)
      }
      resolvedBase.map(_ + ("[]" * reference.dimensions))
    }

    def required(reference: BomTypeRef, context: String): Option[String] =
      resolve(reference, context).orElse {
        bomTypeDiagnostics += BomDiagnostic(
          Codes.BomTypeUnresolved,
          source.file,
          s"unresolved BOM type '${reference.erasedName}' in $context"
        )
        None
      }

    val superClass =
      declaration.superClass.flatMap(reference => required(reference, s"${declaration.fullName} superclass"))
    val interfaces =
      declaration.interfaces.flatMap(reference => required(reference, s"${declaration.fullName} interface"))
    val methods = declaration.members
      .flatMap { member =>
        member.kind match {
          case BomMemberKind.Attribute => None
          case _                       =>
            val returnType =
              if (member.kind == BomMemberKind.Constructor) Some("void")
              else
                member.memberType
                  .flatMap(reference => required(reference, s"${declaration.fullName}.${member.name} return type"))
            val parameterTypes = member.parameters.map { parameter =>
              required(parameter.tpe, s"${declaration.fullName}.${member.name} parameter")
            }
            val throwsTypes =
              member.throwsTypes.map(reference => required(reference, s"${declaration.fullName}.${member.name} throws"))
            if (returnType.isEmpty || parameterTypes.exists(_.isEmpty) || throwsTypes.exists(_.isEmpty)) None
            else {
              val params     = parameterTypes.flatten
              val signature  = s"${returnType.get}(${params.mkString(",")})"
              val methodName = if (member.kind == BomMemberKind.Constructor) "<init>" else member.name
              Some(
                JavaMethodInfo(
                  declaration.fullName,
                  methodName,
                  params,
                  returnType.get,
                  member.modifiers.contains("static"),
                  member.parameters.lastOption.exists(_.isVarargs),
                  s"${declaration.fullName}.$methodName:$signature",
                  signature,
                  !member.modifiers.contains("private") && !member.modifiers.contains("protected"),
                  Some(BomOrigin(source.file, member.properties.find(_.key == "translation.irl").flatMap(_.value)))
                )
              )
            }
        }
      }
      .sortBy(_.fullName)
    val fields = declaration.members.flatMap { member =>
      Option
        .when(member.kind == BomMemberKind.Attribute) {
          member.memberType
            .flatMap(reference => required(reference, s"${declaration.fullName}.${member.name} field"))
            .map(member.name -> _)
        }
        .flatten
    }.toMap
    val effectiveSuperClass =
      if (declaration.kind == io.joern.arl2cpg.bom.BomTypeKind.Interface) None
      else superClass.orElse(Option.when(declaration.fullName != "java.lang.Object")("java.lang.Object"))
    JavaTypeInfo(
      declaration.fullName,
      effectiveSuperClass,
      interfaces,
      declaration.kind == io.joern.arl2cpg.bom.BomTypeKind.Interface,
      methods,
      fields
    )
  }

  private def knownTypeName(candidate: String): Option[String] = {
    val candidates = Iterator(candidate) ++ nestedBinaryNameVariants(candidate)
    candidates.find { name =>
      bomTypeNames.contains(name) ||
      sourceTypes.contains(name) ||
      classpathIndex.contains(name) ||
      readJdkClass(name).nonEmpty
    }
  }

  private def mergeBomType(javaType: JavaTypeInfo, bomType: JavaTypeInfo): JavaTypeInfo = {
    val javaMethods = javaType.methods.map(method => method.name -> method.paramTypes).toSet
    val methods     =
      (javaType.methods ++ bomType.methods.filterNot(method => javaMethods.contains(method.name -> method.paramTypes)))
        .sortBy(_.fullName)
    val fields = javaType.fields ++ bomType.fields.filterNot { case (name, _) => javaType.fields.contains(name) }
    javaType.copy(methods = methods, fields = fields)
  }

  private def indexClasspath(): Map[String, ClassLocation] = {
    val index = mutable.LinkedHashMap.empty[String, ClassLocation]
    classpath.foreach { pathString =>
      val path = Paths.get(pathString)
      if (!Files.exists(path)) {
        throw new IllegalArgumentException(s"--xom-classpath path '$pathString' does not exist")
      } else if (Files.isDirectory(path)) {
        InputFiles
          .walk(
            path,
            "--xom-classpath",
            file =>
              Files.isRegularFile(file) && {
                val name = file.getFileName.toString
                name.endsWith(".class") || name.endsWith(".jar")
              }
          )
          .foreach { file =>
            if (file.getFileName.toString.endsWith(".class")) {
              val relative = path.relativize(file)
              index.getOrElseUpdate(className(relative.toString), DirectoryClass(path, relative))
            } else {
              indexJar(index, file)
            }
          }
      } else if (Files.isRegularFile(path)) {
        indexJar(index, path)
      } else {
        throw new IllegalArgumentException(s"--xom-classpath path '$pathString' is not a file or directory")
      }
    }
    index.toMap
  }

  private def indexJar(index: mutable.LinkedHashMap[String, ClassLocation], path: Path): Unit = {
    try {
      val jar = new JarFile(path.toFile)
      try {
        jar
          .entries()
          .asScala
          .filter(entry => !entry.isDirectory && entry.getName.endsWith(".class"))
          .toList
          .sortBy(_.getName)
          .foreach(entry => index.getOrElseUpdate(className(entry.getName), JarClass(path, entry.getName)))
      } finally jar.close()
    } catch {
      case NonFatal(exception) =>
        throw new IllegalArgumentException(s"--xom-classpath jar '$path' is unreadable", exception)
    }
  }

  private def className(classFilePath: String): String =
    classFilePath.stripSuffix(".class").replace('\\', '/').replace('/', '.')

  private def readClass(location: ClassLocation): Option[JavaTypeInfo] =
    try {
      val bytes = location match {
        case DirectoryClass(root, relativePath) =>
          Files.readAllBytes(root.resolve(relativePath))
        case JarClass(path, entryName) =>
          val jar = new JarFile(path.toFile)
          try {
            val entry = jar.getJarEntry(entryName)
            val in    = jar.getInputStream(entry)
            try in.readAllBytes()
            finally in.close()
          } finally jar.close()
      }
      Some(parseClass(bytes))
    } catch {
      case NonFatal(exception) =>
        logger.warn(s"Unable to parse class from classpath location '$location'", exception)
        None
    }

  private def readJdkClass(fullName: String): Option[JavaTypeInfo] =
    jrtFileSystem.flatMap { fileSystem =>
      val packageName = fullName.split('.').dropRight(1).mkString(".")
      val classPath   = fullName.replace('.', '/') + ".class"
      val packagePath = fileSystem.getPath("/packages", packageName)
      if (!Files.exists(packagePath)) None
      else {
        val stream  = Files.list(packagePath)
        val modules =
          try stream.iterator().asScala.map(_.getFileName.toString).toList.sorted
          finally stream.close()
        modules.iterator
          .map(module => fileSystem.getPath("/modules", module, classPath))
          .find(path => Files.isRegularFile(path))
          .map(path => Files.readAllBytes(path))
          .map(parseClass)
      }
    }

  private def parseClass(bytes: Array[Byte]): JavaTypeInfo = {
    var owner       = ""
    var superClass  = Option.empty[String]
    var interfaces  = Seq.empty[String]
    var isInterface = false
    val methods     = mutable.ListBuffer.empty[JavaMethodInfo]
    val fields      = mutable.LinkedHashMap.empty[String, String]

    val visitor = new ClassVisitor(Opcodes.ASM9) {
      override def visit(
        version: Int,
        access: Int,
        name: String,
        signature: String,
        superName: String,
        interfaceNames: Array[String]
      ): Unit = {
        owner = name.replace('/', '.')
        superClass = Option(superName).map(_.replace('/', '.'))
        interfaces = Option(interfaceNames).toSeq.flatten.map(_.replace('/', '.')).sorted
        isInterface = (access & Opcodes.ACC_INTERFACE) != 0
      }

      override def visitField(
        access: Int,
        name: String,
        descriptor: String,
        signature: String,
        value: Any
      ): FieldVisitor = {
        fields.getOrElseUpdate(name, Type.getType(descriptor).getClassName)
        null
      }

      override def visitMethod(
        access: Int,
        name: String,
        descriptor: String,
        signature: String,
        exceptions: Array[String]
      ): MethodVisitor = {
        val ignoredFlags = Opcodes.ACC_PRIVATE | Opcodes.ACC_SYNTHETIC | Opcodes.ACC_BRIDGE
        if ((access & ignoredFlags) == 0) {
          val methodType = Type.getMethodType(descriptor)
          val paramTypes = methodType.getArgumentTypes.toList.map(_.getClassName)
          val returnType = if (name == "<init>") "void" else methodType.getReturnType.getClassName
          val methodSig  = s"$returnType(${paramTypes.mkString(",")})"
          methods += JavaMethodInfo(
            owner,
            name,
            paramTypes,
            returnType,
            (access & Opcodes.ACC_STATIC) != 0,
            (access & Opcodes.ACC_VARARGS) != 0,
            s"$owner.$name:$methodSig",
            methodSig,
            (access & Opcodes.ACC_PUBLIC) != 0
          )
        }
        null
      }
    }
    new ClassReader(bytes).accept(visitor, ClassReader.SKIP_CODE | ClassReader.SKIP_DEBUG | ClassReader.SKIP_FRAMES)
    JavaTypeInfo(owner, superClass, interfaces, isInterface, methods.toList.sortBy(_.fullName), fields.toMap)
  }

  private def arrayType(fullName: String): Option[JavaTypeInfo] =
    Option.when(fullName.endsWith("[]")) {
      JavaTypeInfo(
        fullName,
        Some("java.lang.Object"),
        Seq("java.lang.Cloneable", "java.io.Serializable"),
        isInterface = false,
        methods = Seq(arrayContainsMethod(fullName)),
        fields = Map.empty
      )
    }

  /** ARL/BOM array membership is not a Java method: IBM rewrites it to a collection utility. */
  private def arrayContainsMethod(arrayTypeName: String): JavaMethodInfo = {
    val componentType = arrayTypeName.stripSuffix("[]")
    val signature     = s"boolean($componentType)"
    JavaMethodInfo(
      arrayTypeName,
      "contains",
      List(componentType),
      "boolean",
      isStatic = false,
      isVarargs = false,
      s"$arrayTypeName.contains:$signature",
      signature,
      isPublic = true
    )
  }

  private def capitalize(value: String): String =
    value.headOption.map(_.toUpper.toString + value.drop(1)).getOrElse(value)

  private def isUsableMemberType(typeName: String): Boolean =
    typeName.nonEmpty && typeName != "ANY" && typeName != "<unresolvedNamespace>"

  private val PrimitiveTypes = Set("boolean", "byte", "short", "int", "long", "float", "double", "char", "void")
}
