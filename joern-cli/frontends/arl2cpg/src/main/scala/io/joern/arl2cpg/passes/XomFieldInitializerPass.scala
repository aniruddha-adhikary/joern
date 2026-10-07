package io.joern.arl2cpg.passes

import com.github.javaparser.JavaParser
import com.github.javaparser.ast.Node.Parsedness
import com.github.javaparser.ast.body.FieldDeclaration
import com.github.javaparser.ast.expr.{
  BooleanLiteralExpr,
  CharLiteralExpr,
  DoubleLiteralExpr,
  Expression,
  IntegerLiteralExpr,
  LongLiteralExpr,
  NullLiteralExpr,
  StringLiteralExpr,
  TextBlockLiteralExpr,
  UnaryExpr
}
import io.joern.javasrc2cpg.util.SourceParser
import io.joern.x2cpg.frontendspecific.arl2cpg.{ArlFindings, ArlTags}
import io.shiftleft.codepropertygraph.generated.{Cpg, DiffGraphBuilder}
import io.shiftleft.codepropertygraph.generated.nodes.Member
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language.*

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

class XomFieldInitializerPass(cpg: Cpg, xomSrcPaths: Set[String]) extends CpgPass(cpg) {

  private case class LiteralMetadata(kind: String, value: Option[String])

  override def run(builder: DiffGraphBuilder): Unit = {
    val roots = xomSrcPaths.toSeq.sorted.map(normalize)
    val files = roots
      .flatMap(root => javaFiles(root).map(file => root -> file))
      .groupBy(_._2)
      .toSeq
      .sortBy(_._1.toString)
      .map { case (_, rootAndFiles) => rootAndFiles.minBy(_._1.toString) }
    val members = cpg.member.l

    files.foreach { case (root, file) =>
      processFile(builder, root, file, members)
    }
  }

  private def processFile(builder: DiffGraphBuilder, root: Path, file: Path, members: List[Member]): Unit = {
    val source      = Files.readString(file, StandardCharsets.UTF_8)
    val parseResult = new JavaParser(SourceParser.parserConfiguration(storeTokens = true)).parse(file)
    val problems    = parseResult.getProblems.asScala.toList
    parseResult.getResult.toScala match {
      case Some(compilationUnit) if problems.isEmpty && compilationUnit.getParsed == Parsedness.PARSED =>
        val fields = compilationUnit
          .findAll(classOf[FieldDeclaration])
          .asScala
          .toList
          .flatMap(_.getVariables.asScala.toList)
          .sortBy(variable => {
            val begin = variable.getBegin.toScala
            (
              begin.map(_.line).getOrElse(Int.MaxValue),
              begin.map(_.column).getOrElse(Int.MaxValue),
              variable.getNameAsString
            )
          })
        fields.foreach { variable =>
          variable.getInitializer.toScala.foreach { initializer =>
            val matches = variable.getBegin.toScala.toList.flatMap { begin =>
              members.filter { member =>
                member.name == variable.getNameAsString &&
                member.lineNumber.contains(begin.line) &&
                sourcePath(member.typeDecl.filename, root) == file
              }
            }
            if (matches.size == 1) {
              val member            = matches.head
              val initializerSource = sourceSlice(source, initializer)
              ArlTags.tag(builder, member, ArlTags.InitializerCode, initializerSource)
              val literal = literalMetadata(initializer, initializerSource, source)
              ArlTags.tag(builder, member, ArlTags.LiteralKind, literal.kind)
              literal.value.foreach(value => ArlTags.tag(builder, member, ArlTags.LiteralValue, value))
            } else {
              ArlFindings.finding(
                builder,
                None,
                ArlFindings.Codes.XomFieldUnmatched,
                ArlFindings.Codes.XomFieldUnmatched,
                s"could not uniquely match XOM field '${variable.getNameAsString}' in '$file'",
                file.toString,
                variable.getBegin.toScala.map(_.line),
                List("field" -> variable.getNameAsString, "matches" -> matches.size.toString)
              )
            }
          }
        }
      case _ =>
        val problemText =
          if (problems.nonEmpty) problems.map(_.getMessage).mkString("; ")
          else "JavaParser produced no compilation unit"
        ArlFindings.finding(
          builder,
          None,
          ArlFindings.Codes.XomSourceUnparsed,
          ArlFindings.Codes.XomSourceUnparsed,
          s"could not parse XOM source '$file': $problemText",
          file.toString,
          None,
          List("problems" -> problemText)
        )
    }
  }

  private def javaFiles(root: Path): List[Path] = {
    val stream = Files.walk(root)
    try {
      stream.iterator.asScala
        .filter(path => Files.isRegularFile(path) && path.toString.endsWith(".java"))
        .map(normalize)
        .toList
        .sortBy(_.toString)
    } finally {
      stream.close()
    }
  }

  private def normalize(path: String): Path = Paths.get(path).toAbsolutePath.normalize()

  private def normalize(path: Path): Path = path.toAbsolutePath.normalize()

  private def sourcePath(filename: String, root: Path): Path = {
    val path = Paths.get(filename)
    normalize(if (path.isAbsolute) path else root.resolve(path))
  }

  private def sourceSlice(source: String, node: com.github.javaparser.ast.Node): String = {
    val range = node.getRange.toScala.getOrElse {
      throw new IllegalStateException(s"JavaParser node '${node.getClass.getSimpleName}' has no source range")
    }
    val begin = offset(source, range.begin.line, range.begin.column)
    val end   = offset(source, range.end.line, range.end.column + 1)
    source.substring(begin, end)
  }

  private def offset(source: String, line: Int, column: Int): Int = {
    require(line >= 1 && column >= 1, s"Invalid Java source position $line:$column")
    var currentLine = 1
    var index       = 0
    while (currentLine < line && index < source.length) {
      if (source.charAt(index) == '\n') currentLine += 1
      index += 1
    }
    require(currentLine == line, s"Java source position $line:$column is outside the source")
    val result = index + column - 1
    require(result <= source.length, s"Java source position $line:$column is outside the source")
    result
  }

  private def literalMetadata(expression: Expression, source: String, fullSource: String): LiteralMetadata =
    expression match {
      case value: StringLiteralExpr    => LiteralMetadata("string", Some(value.asString))
      case value: TextBlockLiteralExpr => LiteralMetadata("string", Some(value.asString))
      case value: CharLiteralExpr      => LiteralMetadata("char", Some(value.asChar.toString))
      case value: IntegerLiteralExpr   => LiteralMetadata("int", Some(value.asNumber.toString))
      case value: LongLiteralExpr      => LiteralMetadata("long", Some(value.asNumber.toString))
      case _: DoubleLiteralExpr        =>
        val trimmed = source.trim
        val suffix  = if (trimmed.endsWith("f") || trimmed.endsWith("F")) 1 else 0
        LiteralMetadata(if (suffix == 1) "float" else "double", Some(trimmed.dropRight(suffix)))
      case value: BooleanLiteralExpr => LiteralMetadata("boolean", Some(value.getValue.toString))
      case _: NullLiteralExpr        => LiteralMetadata("null", None)
      case unary: UnaryExpr if unary.getOperator == UnaryExpr.Operator.MINUS =>
        val operand = sourceSlice(fullSource, unary.getExpression)
        literalMetadata(unary.getExpression, operand, fullSource) match {
          case LiteralMetadata(kind @ ("int" | "long" | "double" | "float"), Some(value)) =>
            LiteralMetadata(kind, Some(s"-$value"))
          case _ => LiteralMetadata("expression", None)
        }
      case _ => LiteralMetadata("expression", None)
    }
}
