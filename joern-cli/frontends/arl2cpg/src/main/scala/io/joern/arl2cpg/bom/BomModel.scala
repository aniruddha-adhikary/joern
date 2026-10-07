package io.joern.arl2cpg.bom

import io.joern.arl2cpg.parser.{BomLexer, BomParser}
import org.antlr.v4.runtime.*

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

enum BomTypeKind {
  case Class, Interface
}

enum BomMemberKind {
  case Constructor, Method, Attribute, Operator
}

final case class BomTypeArgument(isWildcard: Boolean, bound: Option[String], tpe: Option[BomTypeRef])

final case class BomTypeRef(name: String, typeArguments: List[BomTypeArgument], dimensions: Int) {
  def erasedName: String = {
    val normalized = name match {
      case "object" => "java.lang.Object"
      case "string" => "java.lang.String"
      case other    => other
    }
    normalized + ("[]" * dimensions)
  }
}

final case class BomTypeParameter(name: String, bounds: List[BomTypeRef])

final case class BomProperty(key: String, value: Option[String], entries: List[BomProperty] = Nil)

final case class BomDomainValue(
  value: String,
  castType: Option[BomTypeRef],
  staticReference: Option[String],
  classType: Option[BomTypeRef]
)

sealed trait BomDomain
final case class BomDomainSet(values: List[BomDomainValue]) extends BomDomain
final case class BomDomainRange(
  leftDelimiter: String,
  lower: BomDomainValue,
  upper: BomDomainValue,
  rightDelimiter: String
) extends BomDomain
final case class BomCardinalityDomain(minimum: BomDomainValue, maximum: BomDomainValue, elementType: Option[BomTypeRef])
    extends BomDomain

final case class BomParameter(
  name: Option[String],
  tpe: BomTypeRef,
  isVarargs: Boolean,
  modifiers: Set[String],
  domain: Option[BomDomain],
  annotations: List[String],
  properties: List[BomProperty]
)

final case class BomMember(
  name: String,
  kind: BomMemberKind,
  modifiers: Set[String],
  memberType: Option[BomTypeRef],
  parameters: List[BomParameter],
  throwsTypes: List[BomTypeRef],
  domain: Option[BomDomain],
  annotations: List[String],
  properties: List[BomProperty],
  initializer: Option[String]
)

final case class BomTypeDecl(
  packageName: String,
  name: String,
  fullName: String,
  kind: BomTypeKind,
  modifiers: Set[String],
  typeParameters: List[BomTypeParameter],
  superClass: Option[BomTypeRef],
  interfaces: List[BomTypeRef],
  annotations: List[String],
  properties: List[BomProperty],
  domains: List[BomDomain],
  members: List[BomMember],
  nestedTypes: List[BomTypeDecl]
) {
  def allTypes: List[BomTypeDecl] = this :: nestedTypes.flatMap(_.allTypes)
}

final case class BomFile(
  path: String,
  includes: List[String],
  directives: List[String],
  properties: List[BomProperty],
  types: List[BomTypeDecl]
)

final case class BomTypeSource(file: String, declaration: BomTypeDecl)

final case class BomDiagnostic(
  code: String,
  file: String,
  message: String,
  additionalKeyValues: List[(String, String)] = Nil
)

final case class BomModel(files: List[BomFile], types: List[BomTypeSource], diagnostics: List[BomDiagnostic])

object BomModel {
  val empty: BomModel = new BomModel(Nil, Nil, Nil)

  def load(classpath: Seq[String], bomPaths: Seq[String], bomRoots: Seq[String] = Nil): BomModel =
    BomModelLoader.load(classpath, bomPaths, bomRoots)
}

final case class BomSyntaxError(file: String, line: Int, column: Int, message: String) {
  override def toString: String = s"$file:$line:$column: $message"
}

final class BomSyntaxException(val errors: List[BomSyntaxError])
    extends RuntimeException(errors.map(_.toString).mkString("\n"))

object BomParserFacade {

  private final class CollectingErrorListener(filename: String) extends BaseErrorListener {
    val errors: mutable.ListBuffer[BomSyntaxError] = mutable.ListBuffer.empty

    override def syntaxError(
      recognizer: Recognizer[?, ?],
      offendingSymbol: Any,
      line: Int,
      charPositionInLine: Int,
      msg: String,
      exception: RecognitionException
    ): Unit = errors += BomSyntaxError(filename, line, charPositionInLine, msg)
  }

  def parse(path: Path, displayPath: String = ""): BomFile = {
    val name = if (displayPath.nonEmpty) displayPath else path.toString
    parse(name, Files.readString(path, StandardCharsets.UTF_8))
  }

  def parse(filename: String, content: String): BomFile = {
    val listener   = new CollectingErrorListener(filename)
    val charStream = CharStreams.fromString(content, filename)
    val lexer      = new BomLexer(charStream)
    val parser     = new BomParser(new CommonTokenStream(lexer))
    lexer.removeErrorListeners()
    lexer.addErrorListener(listener)
    parser.removeErrorListeners()
    parser.addErrorListener(listener)
    val tree = parser.bomFile()
    if (listener.errors.nonEmpty) throw new BomSyntaxException(listener.errors.toList)
    BomModelBuilder.build(filename, tree)
  }
}

private object BomModelBuilder {

  def build(filename: String, tree: BomParser.BomFileContext): BomFile = {
    val includes   = mutable.ListBuffer.empty[String]
    val directives = mutable.ListBuffer.empty[String]
    val properties = mutable.ListBuffer.empty[BomProperty]
    val types      = mutable.ListBuffer.empty[BomTypeDecl]
    var pkg        = ""

    tree.bomItem().asScala.foreach { item =>
      if (item.includeDecl() != null) includes += decodeString(item.includeDecl().stringLiteral().getText)
      else if (item.packageDecl() != null) pkg = qualifiedName(item.packageDecl().qualifiedName())
      else if (item.propertyDecl() != null) properties += property(item.propertyDecl())
      else if (item.annotationDecl() != null) directives += qualifiedName(item.annotationDecl().qualifiedName())
      else if (item.typeDecl() != null) types += typeDecl(item.typeDecl(), pkg, None)
    }
    BomFile(filename, includes.toList, directives.toList, properties.toList, types.toList)
  }

  private def typeDecl(ctx: BomParser.TypeDeclContext, pkg: String, enclosingName: Option[String]): BomTypeDecl =
    if (ctx.classDecl() != null) classDecl(ctx.classDecl(), pkg, enclosingName)
    else interfaceDecl(ctx.interfaceDecl(), pkg, enclosingName)

  private def classDecl(ctx: BomParser.ClassDeclContext, pkg: String, enclosingName: Option[String]): BomTypeDecl = {
    val name     = identifier(ctx.name())
    val fullName = fullTypeName(pkg, enclosingName, name)
    val body     = ctx.classBody().classBodyItem().asScala.toList
    val nested   = body
      .filter(_.typeDecl() != null)
      .map(item => typeDecl(item.typeDecl(), pkg, Some(fullName)))
    val declarations  = classBodyDeclarations(body, name)
    val extendedTypes = Option(ctx.classExtends()).toList.flatMap(_.typeRef().asScala.map(typeRef))
    BomTypeDecl(
      pkg,
      name,
      fullName,
      BomTypeKind.Class,
      modifiers(ctx.modifier()),
      typeParameters(ctx.typeParameters()),
      extendedTypes.headOption,
      extendedTypes.drop(1) ++ Option(ctx.classImplements()).toList.flatMap(_.typeRef().asScala.map(typeRef)),
      annotations(ctx.metadata()),
      properties(ctx.metadata()),
      declarations._1,
      declarations._2,
      nested
    )
  }

  private def interfaceDecl(
    ctx: BomParser.InterfaceDeclContext,
    pkg: String,
    enclosingName: Option[String]
  ): BomTypeDecl = {
    val name     = identifier(ctx.name())
    val fullName = fullTypeName(pkg, enclosingName, name)
    val body     = ctx.classBody().classBodyItem().asScala.toList
    val nested   = body
      .filter(_.typeDecl() != null)
      .map(item => typeDecl(item.typeDecl(), pkg, Some(fullName)))
    val declarations = classBodyDeclarations(body, name)
    BomTypeDecl(
      pkg,
      name,
      fullName,
      BomTypeKind.Interface,
      modifiers(ctx.modifier()),
      typeParameters(ctx.typeParameters()),
      None,
      Option(ctx.interfaceExtends()).toList.flatMap(_.typeRef().asScala.map(typeRef)),
      annotations(ctx.metadata()),
      properties(ctx.metadata()),
      declarations._1,
      declarations._2,
      nested
    )
  }

  private def classBodyDeclarations(
    body: List[BomParser.ClassBodyItemContext],
    enclosingName: String
  ): (List[BomDomain], List[BomMember]) = {
    val domains = mutable.ListBuffer.empty[BomDomain]
    val members = mutable.ListBuffer.empty[BomMember]
    body.foreach { item =>
      if (item.domainDeclaration() != null) domains += domain(item.domainDeclaration().domainBody())
      else if (item.constructorDecl() != null) members += constructor(item.constructorDecl())
      else if (item.methodDecl() != null) members += method(item.methodDecl())
      else if (item.operatorDecl() != null) members += operator(item.operatorDecl())
      else if (item.fieldDecl() != null) members ++= fields(item.fieldDecl())
    }
    body.filter(item => item.propertyDecl() != null).foreach { item =>
      members += BomMember(
        name = propertyKey(item.propertyDecl().propertyKey()),
        kind = BomMemberKind.Attribute,
        modifiers = Set.empty,
        memberType = None,
        parameters = Nil,
        throwsTypes = Nil,
        domain = None,
        annotations = Nil,
        properties = List(property(item.propertyDecl())),
        initializer = None
      )
    }
    (domains.toList, members.toList)
  }

  private def constructor(ctx: BomParser.ConstructorDeclContext): BomMember =
    BomMember(
      identifier(ctx.name()),
      BomMemberKind.Constructor,
      modifiers(ctx.modifier()),
      None,
      parameters(ctx.formalParameters()),
      throwsTypes(ctx.throwsClause()),
      Option(ctx.domainClause()).map(c => domain(c.domainBody())),
      annotations(ctx.metadata()),
      properties(ctx.metadata()),
      None
    )

  private def method(ctx: BomParser.MethodDeclContext): BomMember =
    BomMember(
      identifier(ctx.name()),
      BomMemberKind.Method,
      modifiers(ctx.modifier()),
      Some(typeRef(ctx.typeRef())),
      parameters(ctx.formalParameters()),
      throwsTypes(ctx.throwsClause()),
      Option(ctx.domainClause()).map(c => domain(c.domainBody())),
      annotations(ctx.metadata()),
      properties(ctx.metadata()),
      None
    )

  private def operator(ctx: BomParser.OperatorDeclContext): BomMember =
    BomMember(
      "operator",
      BomMemberKind.Operator,
      modifiers(ctx.modifier()),
      Some(typeRef(ctx.typeRef())),
      parameters(ctx.formalParameters()),
      throwsTypes(ctx.throwsClause()),
      Option(ctx.domainClause()).map(c => domain(c.domainBody())),
      annotations(ctx.metadata()),
      properties(ctx.metadata()),
      None
    )

  private def fields(ctx: BomParser.FieldDeclContext): List[BomMember] = {
    val baseType    = typeRef(ctx.typeRef())
    val fieldDomain = Option(ctx.domainClause()).map(c => domain(c.domainBody()))
    ctx.fieldDeclarator().asScala.toList.map { field =>
      val arrayDimensions = field.arraySuffix().size()
      val fieldType       =
        if (arrayDimensions == 0) baseType
        else baseType.copy(dimensions = baseType.dimensions + arrayDimensions)
      BomMember(
        identifier(field.name()),
        BomMemberKind.Attribute,
        modifiers(ctx.modifier()),
        Some(fieldType),
        Nil,
        Nil,
        fieldDomain,
        annotations(ctx.metadata()),
        properties(ctx.metadata()),
        Option(field.fieldInitializer()).map(_.getText)
      )
    }
  }

  private def parameters(ctx: BomParser.FormalParametersContext): List[BomParameter] =
    Option(ctx).toList.flatMap(_.formalParameter().asScala).map { parameter =>
      val arrayDimensions = parameter.arraySuffix().size()
      val baseType        = typeRef(parameter.typeRef())
      val parameterType   =
        if (arrayDimensions == 0) baseType
        else baseType.copy(dimensions = baseType.dimensions + arrayDimensions)
      BomParameter(
        Option(parameter.name()).map(identifier),
        parameterType,
        parameter.children.asScala.exists(_.getText == "..."),
        parameter.parameterModifier().asScala.map(_.getText).toSet,
        Option(parameter.domainClause()).map(c => domain(c.domainBody())),
        annotations(parameter.metadata()),
        properties(parameter.metadata())
      )
    }

  private def throwsTypes(ctx: BomParser.ThrowsClauseContext): List[BomTypeRef] =
    Option(ctx).toList.flatMap(_.typeRef().asScala.map(typeRef))

  private def typeParameters(ctx: BomParser.TypeParametersContext): List[BomTypeParameter] =
    Option(ctx).toList.flatMap(_.typeParameter().asScala).map { parameter =>
      BomTypeParameter(identifier(parameter.name()), parameter.typeRef().asScala.map(typeRef).toList)
    }

  private def typeRef(ctx: BomParser.TypeRefContext): BomTypeRef = {
    val arguments = Option(ctx.typeArguments()).toList.flatMap(_.typeArgument().asScala).map { argument =>
      val wildcard = argument.getChild(0).getText == "?"
      val bound    = Option(argument.typeBound()).map(_.getText)
      val tpe      = Option(argument.typeRef()).map(typeRef)
      BomTypeArgument(wildcard, bound, tpe)
    }
    val typeName  = ctx.typeName()
    val typeParts = List(typeName.typePart().getText) ++ typeName.name().asScala.map(_.getText)
    BomTypeRef(typeParts.map(identifierText).mkString("."), arguments, ctx.arraySuffix().size())
  }

  private def domain(ctx: BomParser.DomainBodyContext): BomDomain =
    if (ctx.domainSet() != null) {
      BomDomainSet(ctx.domainSet().domainValue().asScala.map(domainValue).toList)
    } else if (ctx.domainRange() != null) {
      val range = ctx.domainRange()
      BomDomainRange(
        range.leftBracket().getText,
        domainValue(range.domainValue(0)),
        domainValue(range.domainValue(1)),
        range.rightBracket().getText
      )
    } else {
      val cardinality = ctx.domainCardinality()
      BomCardinalityDomain(
        domainValue(cardinality.domainValue(0)),
        domainValue(cardinality.domainValue(1)),
        Option(cardinality.typeRef()).map(typeRef)
      )
    }

  private def domainValue(ctx: BomParser.DomainValueContext): BomDomainValue = {
    val atom            = ctx.domainAtom()
    val staticReference = Option(atom.staticDomainValue()).map(c => qualifiedName(c.qualifiedName()))
    val classType       = Option(atom.classDomainValue()).map(c => typeRef(c.typeRef()))
    val rawValue        =
      if (atom.stringLiteral() != null) decodeString(atom.stringLiteral().getText)
      else if (atom.signedNumber() != null) atom.signedNumber().getText
      else if (staticReference.nonEmpty) staticReference.get
      else if (classType.nonEmpty) classType.get.erasedName
      else atom.getText
    BomDomainValue(rawValue, Option(ctx.typeCast()).map(c => typeRef(c.typeRef())), staticReference, classType)
  }

  private def annotations(metadata: java.util.List[BomParser.MetadataContext]): List[String] =
    metadata.asScala.toList.flatMap(m => Option(m.annotationDecl()).map(c => qualifiedName(c.qualifiedName())))

  private def properties(metadata: java.util.List[BomParser.MetadataContext]): List[BomProperty] =
    metadata.asScala.toList.flatMap(m => Option(m.propertyDecl()).map(property))

  private def property(ctx: BomParser.PropertyDeclContext): BomProperty =
    BomProperty(
      propertyKey(ctx.propertyKey()),
      Option(ctx.stringLiteral()).map(c => decodeString(c.getText)),
      Option(ctx.propertyBlock()).map(propertyEntries).getOrElse(Nil)
    )

  private def propertyEntries(ctx: BomParser.PropertyBlockContext): List[BomProperty] =
    ctx.propertyBlockEntry().asScala.toList.map { entry =>
      BomProperty(
        propertyKey(entry.propertyKey()),
        Option(entry.stringLiteral()).map(c => decodeString(c.getText)),
        Option(entry.propertyBlock()).map(propertyEntries).getOrElse(Nil)
      )
    }

  private def propertyKey(ctx: BomParser.PropertyKeyContext): String =
    Option(ctx.qualifiedName()).map(qualifiedName).getOrElse(decodeString(ctx.stringLiteral().getText))

  private def modifiers(ctx: java.util.List[BomParser.ModifierContext]): Set[String] =
    ctx.asScala.iterator.map(_.getText).toSet

  private def qualifiedName(ctx: BomParser.QualifiedNameContext): String =
    ctx.name().asScala.map(c => identifier(c)).mkString(".")

  private def identifier(ctx: BomParser.NameContext): String = identifierText(ctx.getText)

  private def identifierText(text: String): String = text.stripPrefix("@")

  private def fullTypeName(pkg: String, enclosingName: Option[String], name: String): String =
    enclosingName match {
      case Some(parent) => s"$parent$$$name"
      case None         => if (pkg.nonEmpty) s"$pkg.$name" else name
    }

  private def decodeString(token: String): String = {
    val content = token.substring(1, token.length - 1)
    val result  = new StringBuilder
    var index   = 0
    while (index < content.length) {
      val current = content.charAt(index)
      if (current != '\\') {
        result.append(current)
        index += 1
      } else {
        index += 1
        if (index >= content.length) throw new IllegalArgumentException("unterminated BOM string escape")
        val escaped = content.charAt(index)
        escaped match {
          case 'b'  => result.append('\b')
          case 'f'  => result.append('\f')
          case 'n'  => result.append('\n')
          case 'r'  => result.append('\r')
          case 't'  => result.append('\t')
          case '"'  => result.append('"')
          case '\'' => result.append('\'')
          case '\\' => result.append('\\')
          case 'u'  =>
            if (index + 4 >= content.length) throw new IllegalArgumentException("short BOM unicode escape")
            val digits = content.substring(index + 1, index + 5)
            result.append(Integer.parseInt(digits, 16).toChar)
            index += 4
          case octal if octal >= '0' && octal <= '7' =>
            val start = index
            var end   = index + 1
            while (end < content.length && end < start + 3 && content.charAt(end) >= '0' && content.charAt(end) <= '7')
              end += 1
            result.append(Integer.parseInt(content.substring(start, end), 8).toChar)
            index = end - 1
          case other => result.append(other)
        }
        index += 1
      }
    }
    result.toString
  }
}
