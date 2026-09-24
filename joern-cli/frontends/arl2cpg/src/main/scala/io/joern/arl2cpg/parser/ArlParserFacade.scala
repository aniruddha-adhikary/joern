package io.joern.arl2cpg.parser

import io.joern.arl2cpg.parser.ARLParser.CompilationUnitContext
import org.antlr.v4.runtime.*
import org.slf4j.LoggerFactory

import java.nio.file.{Files, Path}
import scala.util.Try

/** Result of parsing a single `.arl` file: the parse tree plus the number of syntax errors the ANTLR error listener
  * reported. Error recovery is the default ANTLR strategy — a file with errors still yields a tree, and parsing never
  * aborts the whole file.
  */
case class ArlParseResult(filename: String, content: String, compilationUnit: CompilationUnitContext, errorCount: Int)

object ArlParserFacade {

  private val logger = LoggerFactory.getLogger(getClass)

  private class CountingErrorListener(filename: String) extends BaseErrorListener {

    var errorCount: Int = 0

    override def syntaxError(
      recognizer: Recognizer[?, ?],
      offendingSymbol: Any,
      line: Int,
      charPositionInLine: Int,
      msg: String,
      exception: RecognitionException
    ): Unit = {
      errorCount += 1
      logger.warn(s"$filename:$line:$charPositionInLine $msg")
    }
  }

  def parse(filename: String): Try[ArlParseResult] = {
    Try {
      val content     = Files.readString(Path.of(filename))
      val charStream  = CharStreams.fromString(content, filename)
      val lexer       = new ARLLexer(charStream)
      val tokenStream = new CommonTokenStream(lexer)
      val parser      = new ARLParser(tokenStream)
      val listener    = new CountingErrorListener(filename)

      lexer.removeErrorListeners()
      lexer.addErrorListener(listener)
      parser.removeErrorListeners()
      parser.addErrorListener(listener)

      val unit = parser.compilationUnit()
      ArlParseResult(filename, content, unit, listener.errorCount)
    }
  }

  /** Total syntax errors across all parse results. */
  def errorCount(results: Iterable[ArlParseResult]): Int = results.map(_.errorCount).sum
}
