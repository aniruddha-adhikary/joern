package io.joern.arl2cpg.astcreation

import io.joern.arl2cpg.Config
import io.joern.arl2cpg.parser.{ARLParser, ArlParseResult}
import io.joern.x2cpg.{Ast, AstCreatorBase, Defines, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{DiffGraphBuilder, DispatchTypes, EvaluationStrategies, NodeTypes}
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.tree.{ParseTree, TerminalNode}

import java.nio.file.Paths
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/** Top-level AST creator for a single `.arl` file. The work is split into traits mirroring the grammar layers:
  * AstForRules (signature/ruleset/rule), AstForWhen (Layer B patterns), AstForStatements and AstForExpressions (Layer
  * C), AstForFlow (Layer A ruleflow/tasks) and TypeResolver.
  */
class AstCreator(val parseResult: ArlParseResult, val config: Config)(implicit withSchemaValidation: ValidationMode)
    extends AstCreatorBase[ParserRuleContext, AstCreator](parseResult.filename)
    with AstForRules
    with AstForWhen
    with AstForStatements
    with AstForExpressions
    with AstForFlow
    with TypeResolver {

  protected val compilationUnit: ARLParser.CompilationUnitContext = parseResult.compilationUnit
  protected val fileContent: String                               = parseResult.content

  /** Package declared via `package a.b;` (Designer files). */
  protected var packageName: Option[String] = None

  /** Full name of the signature TYPE_DECL (`S`), used for `this` parameters. */
  protected var signatureFullName: Option[String] = None

  /** Signature member name -> resolved typeFullName. */
  protected val signatureMembers: mutable.Map[String, String] = mutable.Map.empty

  /** Names of the rules declared in this file (backticks stripped), for `rules:` selector matching. */
  protected val fileRuleNames: mutable.ListBuffer[String] = mutable.ListBuffer.empty

  /** Rule names collected in a pre-pass (includes rules declared later in the file). */
  protected var allRuleNames: List[String] = List.empty

  /** A value bound in the current method scope: its type plus the declaring LOCAL/METHOD_PARAMETER_IN node so
    * identifiers that reference it can carry a REF edge to the declaration.
    */
  protected case class BoundValue(typeFullName: String, node: NewNode)

  /** Per-method visible value names (bindings, locals, task params) -> BoundValue. */
  protected val valueScope: mutable.Stack[mutable.Map[String, BoundValue]] =
    mutable.Stack(mutable.Map.empty)

  /** The `this` METHOD_PARAMETER_IN of the method currently being built; set before its body is lowered. */
  protected var thisParam: Option[NewMethodParameterIn] = None

  /** Implicit receiver inside a pattern's test expression: (bindingName, typeFullName). */
  protected val implicitReceiver: mutable.Stack[Option[(String, String)]] = mutable.Stack(None)

  /** Counter for synthetic `$T_<n>` bindings, reset per rule. */
  protected var syntheticBindingCount: Int = 0

  /** fullName of the containing TYPE_DECL (`R`). */
  protected var containerFullName: String = ""

  override def createAst(): DiffGraphBuilder = {
    // A total parse failure can leave `compilationUnit` null; still emit the FILE node.
    val ast = Option(compilationUnit)
      .map(astForCompilationUnit)
      .getOrElse(Ast(NewFile().name(parseResult.filename).order(1)))
    Ast.storeInDiffGraph(ast, diffGraph)
    diffGraph
  }

  // ------------------------------------------------------------------
  // position / code helpers required by AstNodeBuilder
  // ------------------------------------------------------------------

  override protected def line(node: ParserRuleContext): Option[Int] =
    Option(node.getStart).map(_.getLine)

  override protected def column(node: ParserRuleContext): Option[Int] =
    Option(node.getStart).map(_.getCharPositionInLine)

  override protected def lineEnd(node: ParserRuleContext): Option[Int] =
    Option(node.getStop).map(_.getLine)

  override protected def columnEnd(node: ParserRuleContext): Option[Int] =
    Option(node.getStop).map(stop => stop.getCharPositionInLine + stop.getText.length)

  override protected def code(node: ParserRuleContext): String = {
    val start = node.getStart
    val stop  = node.getStop
    if (start != null && stop != null && start.getStartIndex >= 0 && stop.getStopIndex >= start.getStartIndex) {
      fileContent.substring(start.getStartIndex, stop.getStopIndex + 1)
    } else {
      node.getText
    }
  }

  override protected def offset(node: ParserRuleContext): Option[(Int, Int)] = {
    val start = node.getStart
    val stop  = node.getStop
    if (start != null && stop != null) Option((start.getStartIndex, stop.getStopIndex + 1)) else None
  }

  // ------------------------------------------------------------------
  // shared helpers
  // ------------------------------------------------------------------

  protected def textOf(ctx: ParserRuleContext): String = code(ctx)

  protected def nameOf(ruleName: ARLParser.RuleNameContext): String =
    Option(ruleName).map(rn => stripBackticks(rn.getText)).getOrElse("<rule>")

  protected def nameOf(flowId: ARLParser.FlowIdContext): String =
    Option(flowId).map(id => stripBackticks(id.getText)).getOrElse("<task>")

  protected def thisParamType: String = signatureFullName.getOrElse(containerFullName)

  /** `this` PARAMETER_IN (index 0) carried by every METHOD of the containing TYPE_DECL. */
  protected def thisParamAst(node: ParserRuleContext): Ast = {
    val param =
      parameterInNode(node, "this", "this", 0, isVariadic = false, EvaluationStrategies.BY_REFERENCE, thisParamType)
    thisParam = Option(param)
    Ast(param)
  }

  /** IDENTIFIER for `this`, typed by the signature (or container) type, REF'd to the `this` parameter. */
  protected def thisIdentifierAst(node: ParserRuleContext, typeFullName: String): Ast = {
    val ident = identifierNode(node, "this", "this", typeFullName)
    thisParam.fold(Ast(ident))(param => Ast(ident).withRefEdge(ident, param))
  }

  /** IDENTIFIER for a name bound in the current scope, with a REF edge to its declaration when known. */
  protected def boundIdentifierAst(node: ParserRuleContext, name: String, code: String, typeFullName: String): Ast = {
    val ident = identifierNode(node, name, code, typeFullName)
    boundValueNode(name).fold(Ast(ident))(decl => Ast(ident).withRefEdge(ident, decl))
  }

  /** fieldAccess on an implicit `this` for a signature parameter. */
  protected def signatureParamFieldAccessAst(node: ParserRuleContext, name: String): Ast = {
    val memberType = signatureMembers.getOrElse(name, Defines.Any)
    fieldAccessAst(node, node, thisIdentifierAst(node, thisParamType), name, name, memberType)
  }

  /** Whether a name is bound as a value (binding, local or task parameter) in the current scope. */
  protected def isBoundValue(name: String): Boolean = valueScope.top.contains(name)

  protected def boundValueType(name: String): String =
    valueScope.top.get(name).map(_.typeFullName).getOrElse(Defines.Any)

  /** Run `body` with a snapshot of the current scope restored afterwards: bindings declared inside a nested block do
    * not leak to the enclosing method scope (lexical block scoping).
    */
  protected def withBlockScope[T](body: => T): T = {
    val snapshot = valueScope.top.clone()
    try body
    finally {
      valueScope.top.clear()
      valueScope.top ++= snapshot
    }
  }

  protected def boundValueNode(name: String): Option[NewNode] =
    valueScope.top.get(name).map(_.node)

  protected def declareValue(name: String, typeFullName: String, node: NewNode): Unit = {
    valueScope.top(name) = BoundValue(typeFullName, node)
  }

  /** Unknown-node fallback; never throws. */
  protected def unknownAst(node: ParserRuleContext): Ast =
    Ast(unknownNode(node, code(node)))

  /** `code` of an Ast's root node (empty for empty Asts). */
  protected def rootCode(ast: Ast): String =
    ast.root.collect { case n: AstNodeNew => n.code }.getOrElse("")

  /** `typeFullName` of an Ast's root node, defaulting to `ANY`. */
  protected def rootType(ast: Ast): String =
    ast.root
      .flatMap(_.properties.get("typeFullName"))
      .map(_.toString)
      .filter(_.nonEmpty)
      .getOrElse(Defines.Any)

  /** `T.m:<unresolvedSignature>` style method full name. */
  protected def unresolvedMethodFullName(namespace: String, name: String, argCount: Int): String =
    s"$namespace.$name:${Defines.UnresolvedSignature}($argCount)"

  /** Children of a context; empty when ANTLR error recovery leaves `children` unset. */
  protected def childrenOf(ctx: ParserRuleContext): List[ParseTree] =
    Option(ctx).flatMap(ruleCtx => Option(ruleCtx.children)).map(_.asScala.toList).getOrElse(Nil)

  /** Children of a context that are themselves parser contexts, in order. */
  protected def ruleChildren(ctx: ParserRuleContext): List[ParserRuleContext] =
    childrenOf(ctx).collect { case c: ParserRuleContext => c }

  /** Terminal children text of a context (e.g. keywords). */
  protected def terminalTexts(ctx: ParserRuleContext): List[String] =
    childrenOf(ctx).collect { case t: TerminalNode => t.getText }
}
