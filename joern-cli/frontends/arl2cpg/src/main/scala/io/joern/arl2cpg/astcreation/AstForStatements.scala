package io.joern.arl2cpg.astcreation

import io.joern.arl2cpg.parser.ARLParser
import io.joern.x2cpg.{Ast, Defines}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, DispatchTypes, EvaluationStrategies, Operators}
import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.tree.TerminalNode

import scala.jdk.CollectionConverters.*

/** Layer C — Java-forked statements and blocks, plus the rule-action level (`then`, `match many`). */
trait AstForStatements {
  this: AstCreator =>

  // ------------------------------------------------------------------
  // blocks & statements
  // ------------------------------------------------------------------

  protected def astForBlock(ctx: ARLParser.BlockContext): Ast =
    blockAst(blockNode(ctx), ctx.statement().asScala.toList.flatMap(astsForStatement))

  /** A single statement lowers to zero or more ASTs (a localVarDecl with an initialiser produces LOCAL + assignment).
    */
  protected def astsForStatement(ctx: ARLParser.StatementContext): List[Ast] = {
    childrenOf(ctx).headOption match {
      case Some(b: ARLParser.BlockContext) => List(astForBlock(b))
      case Some(t: TerminalNode)           =>
        t.getText match {
          case "if"                                       => List(astForIfStatement(ctx))
          case "for"                                      => List(astForForStatement(ctx))
          case "while"                                    => List(astForWhileStatement(ctx))
          case "return"                                   => List(astForReturnStatement(ctx))
          case "insert" | "retract" | "update" | "modify" => astsForKeywordStatement(ctx, t.getText)
          case ";"                                        => List.empty
          case _                                          => List(unknownAst(ctx))
        }
      case Some(l: ARLParser.LocalVarDeclContext) => astsForLocalVarDecl(l)
      case Some(e: ARLParser.ExpressionContext)   =>
        // `expression ';'` or `expression block`.
        val exprAst = astForExpression(e)
        Option(ctx.block()) match {
          case Some(block) => List(exprAst, astForBlock(block))
          case None        => List(exprAst)
        }
      case _ => List(unknownAst(ctx))
    }
  }

  private def astForIfStatement(ctx: ARLParser.StatementContext): Ast = {
    val condition = Option(ctx.expression()).map(astForExpression).getOrElse(unknownAst(ctx))
    val stmts     = ctx.statement().asScala.toList
    val thenAst   = stmts.headOption.map(astsForStatement).getOrElse(List.empty)
    val elseAst   = stmts.drop(1).flatMap(astsForStatement)
    ifThenElseAst(
      ctx,
      Option(condition),
      wrapMultipleInBlock(thenAst, line(ctx)),
      elseAst match {
        case Nil  => None
        case asts => Option(wrapMultipleInBlock(asts, line(ctx)))
      }
    )
  }

  private def astForForStatement(ctx: ARLParser.StatementContext): Ast = {
    // `for ( T id : e ) stmt`  (enhanced) or `for ( init? ; cond? ; update? ) stmt`.
    val stmts = ctx.statement().asScala.toList
    val body  = wrapMultipleInBlock(stmts.flatMap(astsForStatement), line(ctx))
    if (Option(ctx.`type`()).isDefined) {
      // enhanced for: for ( type name : expression ) statement
      val varName = Option(ctx.Identifier())
        .map(_.getText)
        .orElse(Option(ctx.BacktickId()).map(_.getText))
        .getOrElse("<unknown>")
      val tName = typeFullName(ctx.`type`())
      val local = localNode(ctx, varName, s"${ctx.`type`().getText} $varName", tName)
      declareValue(varName, tName, local)
      val iterExpr = Option(ctx.expression()).map(astForExpression).getOrElse(unknownAst(ctx))
      forAst(ctx, Seq(Ast(local)), Seq.empty, Seq(iterExpr), Seq.empty, Seq(body))
    } else {
      val initAsts = Option(ctx.forInit()).toList.flatMap { init =>
        Option(init.localVarDecl()).map(astsForLocalVarDecl).getOrElse {
          Option(init.expressionList()).toList.flatMap(_.expression().asScala.toList).map(astForExpression)
        }
      }
      // Classic for: children of the header are forInit? ; expression? ; expressionList? — the lone
      // expression() child (if present) is the condition; expressionList() is the update.
      val condAst    = Option(ctx.expression()).map(astForExpression)
      val updateAsts = Option(ctx.expressionList()).toList.flatMap(_.expression().asScala.toList.map(astForExpression))
      forAst(ctx, Seq.empty, initAsts, condAst.toList, updateAsts, Seq(body))
    }
  }

  private def astForWhileStatement(ctx: ARLParser.StatementContext): Ast = {
    val condition = Option(ctx.expression()).map(astForExpression).getOrElse(unknownAst(ctx))
    val body      = wrapMultipleInBlock(ctx.statement().asScala.toList.flatMap(astsForStatement), line(ctx))
    whileAst(ctx, Option(condition), Seq(body))
  }

  private def astForReturnStatement(ctx: ARLParser.StatementContext): Ast = {
    val args = Option(ctx.expression()).map(astForExpression).toList
    returnAst(returnNode(ctx, code(ctx)), args)
  }

  /** `T x = e;` → LOCAL x + `x = e` assignment. */
  protected def astsForLocalVarDecl(ctx: ARLParser.LocalVarDeclContext): List[Ast] = {
    val name  = Option(ctx.Identifier()).map(_.getText).getOrElse("<unknown>")
    val tName = Option(ctx.`type`()).map(typeFullName).getOrElse(Defines.Any)
    val local = localNode(ctx, name, code(ctx), tName)
    declareValue(name, tName, local)
    val initAst = Option(ctx.expression()).map { expr =>
      val lhs    = boundIdentifierAst(ctx, name, name, tName)
      val assign = callNode(
        ctx,
        s"$name = ${code(expr)}",
        Operators.assignment,
        Operators.assignment,
        DispatchTypes.STATIC_DISPATCH
      )
      callAst(assign, Seq(lhs, astForExpression(expr)))
    }
    Ast(local) +: initAst.toList
  }

  /** `insert x;` / `retract x;` / `update x;` / `modify x;` → static CALL of that name with x as argument 1. */
  private def astsForKeywordStatement(ctx: ARLParser.StatementContext, keyword: String): List[Ast] = {
    val exprAst = Option(ctx.expression()).map(astForExpression).getOrElse(unknownAst(ctx))
    val call    = callNode(
      ctx,
      code(ctx),
      keyword,
      unresolvedMethodFullName(Defines.UnresolvedNamespace, keyword, 1),
      DispatchTypes.STATIC_DISPATCH,
      Option(Defines.UnresolvedSignature + "(1)"),
      Option(Defines.Any)
    )
    val callAst_ = callAst(call, Seq(exprAst))
    Option(ctx.block()) match {
      case Some(block) => List(callAst_, astForBlock(block))
      case None        => List(callAst_)
    }
  }

  // ------------------------------------------------------------------
  // rule actions
  // ------------------------------------------------------------------

  protected def astForRuleAction(ctx: ARLParser.RuleActionContext): List[Ast] = {
    childrenOf(ctx).headOption match {
      case Some(t: ARLParser.ThenBlockContext)        => List(astForThenBlock(t))
      case Some(t: ARLParser.ThenNamedBlockContext)   => List(astForThenNamedBlock(t))
      case Some(t: ARLParser.ThenRowContext)          => List(astForThenRow(t))
      case Some(m: ARLParser.MatchManyContext)        => List(astForMatchMany(m))
      case Some(t: TerminalNode) if t.getText == "if" => List(astForRuleIf(ctx))
      case _                                          => List(unknownAst(ctx))
    }
  }

  private def astForRuleIf(ctx: ARLParser.RuleActionContext): Ast = {
    // children: 'if' '(' expression ')' '{' ruleAction* '}' ('else' '{' ruleAction* '}')?
    // ruleAction(i) returns them all flattened; ANTLR gives no per-branch split so we count braces.
    val actions = ctx.ruleAction().asScala.toList
    // Determine the split point: the number of then-branch actions is unknown from the context alone,
    // so scan the token stream for the 'else' between the two '{' groups. Simpler: '{' positions.
    val bracePositions = childrenOf(ctx).zipWithIndex.collect {
      case (t: TerminalNode, i) if t.getText == "{" || t.getText == "}" => (t.getText, i)
    }
    // First '{' opens then-block; matching '}' closes it; if another '{' exists, there is an else.
    val elseIdx =
      childrenOf(ctx).indexWhere(child => child.isInstanceOf[TerminalNode] && child.getText == "else")
    if (elseIdx >= 0) {
      // count then-branch actions = actions whose context ends before the 'else' token's char position
      val elseTok                    = childrenOf(ctx)(elseIdx).asInstanceOf[TerminalNode]
      val elseCharIx                 = elseTok.getSymbol.getStartIndex
      val (thenActions, elseActions) = actions.partition(action => action.getStart.getStartIndex < elseCharIx)
      ifThenElseAst(
        ctx,
        Option(astForExpression(ctx.expression())),
        wrapMultipleInBlock(thenActions.flatMap(astForRuleAction), line(ctx)),
        Option(wrapMultipleInBlock(elseActions.flatMap(astForRuleAction), line(ctx)))
      )
    } else {
      ifThenElseAst(
        ctx,
        Option(astForExpression(ctx.expression())),
        wrapMultipleInBlock(actions.flatMap(astForRuleAction), line(ctx)),
        None
      )
    }
  }

  /** `then block` → the block's statements. */
  private def astForThenBlock(ctx: ARLParser.ThenBlockContext): Ast =
    astForBlock(ctx.block())

  /** `then then {…}` / `then else {…}` → BLOCK with code `then then`/`then else`. */
  private def astForThenNamedBlock(ctx: ARLParser.ThenNamedBlockContext): Ast = {
    val label = terminalTexts(ctx)
      .collectFirst { case "else" => "else" }
      .orElse(Option(ctx.id()).map(_.getText))
      .getOrElse("")
    val block = NewBlock()
      .code(s"then $label")
      .typeFullName(Defines.Any)
      .lineNumber(line(ctx))
      .columnNumber(column(ctx))
    blockAst(block, ctx.block().statement().asScala.toList.flatMap(astsForStatement))
  }

  /** `then N block` (decision-table row) → BLOCK with code `then N`. */
  private def astForThenRow(ctx: ARLParser.ThenRowContext): Ast = {
    val block = NewBlock()
      .code(s"then ${ctx.Integer().getText}")
      .typeFullName(Defines.Any)
      .lineNumber(line(ctx))
      .columnNumber(column(ctx))
    blockAst(block, ctx.block().statement().asScala.toList.flatMap(astsForStatement))
  }

  // ------------------------------------------------------------------
  // match many
  // ------------------------------------------------------------------

  /** `match many { case (e_i): out_i; default: out_d }` → BLOCK(code `match many`) of independent IFs. */
  private def astForMatchMany(ctx: ARLParser.MatchManyContext): Ast = {
    val cases     = ctx.matchCase().asScala.toList
    val caseConds = cases.collect { case caseCtx if Option(caseCtx.expression()).isDefined => caseCtx }
    val ifAsts    = cases.flatMap { mc =>
      if (Option(mc.expression()).isDefined) {
        val cond = astForExpression(mc.expression())
        val out  = astForMatchOutcome(mc.matchOutcome())
        List(ifThenElseAst(mc, Option(cond), out, None))
      } else Nil
    }
    val defaultAst = cases.find(mc => Option(mc.expression()).isEmpty).flatMap { mc =>
      val condAst =
        if (caseConds.isEmpty) {
          trueLitAst(mc)
        } else {
          val orChain = caseConds
            .map(caseCtx => astForExpression(caseCtx.expression()))
            .reduceLeftOption((left, right) => binaryOpAst(mc, Operators.logicalOr, left, right))
            .get
          val notCall = operatorCallNode(mc, s"!(${mc.getText})", Operators.logicalNot, Option(Defines.Any))
          callAst(notCall, List(orChain))
        }
      Option(ifThenElseAst(mc, Option(condAst), astForMatchOutcome(mc.matchOutcome()), None))
    }
    val block = NewBlock()
      .code("match many")
      .typeFullName(Defines.Any)
      .lineNumber(line(ctx))
      .columnNumber(column(ctx))
    blockAst(block, ifAsts ++ defaultAst.toList)
  }

  private def trueLitAst(ctx: ParserRuleContext): Ast =
    Ast(literalNode(ctx, "true", "boolean"))

  private def binaryOpAst(ctx: ParserRuleContext, op: String, left: Ast, right: Ast): Ast = {
    val call = operatorCallNode(ctx, s"${rootCode(left)} $op ${rootCode(right)}", op, Option(Defines.Any))
    callAst(call, Seq(left, right))
  }

  /** matchOutcome: thenRow/thenBlock/matchMany/if/…/`when {…} outcome`. */
  protected def astForMatchOutcome(ctx: ARLParser.MatchOutcomeContext): Ast = {
    childrenOf(ctx).headOption match {
      case Some(t: ARLParser.ThenRowContext)            => astForThenRow(t)
      case Some(t: ARLParser.ThenBlockContext)          => astForThenBlock(t)
      case Some(m: ARLParser.MatchManyContext)          => astForMatchMany(m)
      case Some(t: TerminalNode) if t.getText == "if"   => astForMatchOutcomeIf(ctx)
      case Some(t: TerminalNode) if t.getText == "when" =>
        // binding stmts + IF(conjuncts) { outcome }
        val whenCtx            = ctx.whenStatement().asScala.toList
        val parts              = whenCtx.map(astsForWhenStatement)
        val (stmts, conjuncts) = (parts.flatMap(_._1), parts.flatMap(_._2))
        val condition          =
          conjuncts
            .reduceLeftOption((left, right) => binaryOpAst(ctx, Operators.logicalAnd, left, right))
            .getOrElse(trueLitAst(ctx))
        val outcome = ctx.matchOutcome().asScala.headOption.map(astForMatchOutcome).getOrElse(Ast())
        val ifAst   = ifThenElseAst(ctx, Option(condition), outcome, None)
        wrapMultipleInBlock(stmts :+ ifAst, line(ctx))
      case _ => unknownAst(ctx)
    }
  }

  private def astForMatchOutcomeIf(ctx: ARLParser.MatchOutcomeContext): Ast = {
    val outcomes = ctx.matchOutcome().asScala.toList
    val elseIdx  =
      childrenOf(ctx).indexWhere(child => child.isInstanceOf[TerminalNode] && child.getText == "else")
    if (elseIdx >= 0) {
      val elseCharIx         = childrenOf(ctx)(elseIdx).asInstanceOf[TerminalNode].getSymbol.getStartIndex
      val (thenOut, elseOut) = outcomes.partition(outcome => outcome.getStart.getStartIndex < elseCharIx)
      ifThenElseAst(
        ctx,
        Option(astForExpression(ctx.expression())),
        wrapMultipleInBlock(thenOut.map(astForMatchOutcome), line(ctx)),
        Option(wrapMultipleInBlock(elseOut.map(astForMatchOutcome), line(ctx)))
      )
    } else {
      ifThenElseAst(
        ctx,
        Option(astForExpression(ctx.expression())),
        wrapMultipleInBlock(outcomes.map(astForMatchOutcome), line(ctx)),
        None
      )
    }
  }
}
