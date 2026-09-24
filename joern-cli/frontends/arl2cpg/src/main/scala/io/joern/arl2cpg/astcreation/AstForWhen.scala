package io.joern.arl2cpg.astcreation

import io.joern.arl2cpg.ArlOperators
import io.joern.arl2cpg.parser.ARLParser
import io.joern.x2cpg.{Ast, Defines}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EvaluationStrategies, NodeTypes, Operators}
import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.tree.TerminalNode

import scala.jdk.CollectionConverters.*

/** Layer B — `when { ... }` pattern-matching sublanguage.
  *
  * Each whenStatement contributes *binding statements* (emitted in order before the IF) and *conjuncts* (ANDed into the
  * IF condition). Returned as a pair `(bindingStatements, conjuncts)`.
  */
trait AstForWhen {
  this: AstCreator =>

  /** (bindingStatements, conjuncts) produced by a `when { ... }` block. */
  protected def astsForWhen(ctx: ARLParser.RuleWhenContext): (List[Ast], List[Ast]) = {
    val parts = ctx.whenStatement().asScala.toList.map(astsForWhenStatement)
    (parts.flatMap(_._1), parts.flatMap(_._2))
  }

  protected def astsForWhenStatement(ctx: ARLParser.WhenStatementContext): (List[Ast], List[Ast]) = {
    ctx.children.asScala.headOption match {
      case Some(c: ARLParser.EvaluatePatternContext)  => astsForEvaluate(c)
      case Some(c: ARLParser.WherePatternContext)     => (List.empty, List(astForExpression(c.expression())))
      case Some(c: ARLParser.AggregatePatternContext) => astsForAggregate(c, negated = false)
      case Some(c: ARLParser.ExistsPatternContext)    =>
        astsForExists(c.classPattern().asScala.toList, negated = false, ctx)
      case Some(c: ARLParser.NotPatternContext) =>
        Option(c.aggregatePattern()) match {
          case Some(agg) => astsForAggregate(agg, negated = true)
          case None      => astsForExists(c.classPattern().asScala.toList, negated = true, ctx)
        }
      case Some(c: ARLParser.BindingPatternContext) => astsForBindingPattern(c, negate = false)
      case Some(c: ARLParser.ClassPatternContext)   => astsForClassPattern(c)
      case _                                        => (List(unknownAst(ctx)), List.empty)
    }
  }

  // ------------------------------------------------------------------
  // class / binding patterns
  // ------------------------------------------------------------------

  private def nextSyntheticBinding(typeName: String): String = {
    syntheticBindingCount += 1
    s"$$${typeName.split('.').lastOption.getOrElse(typeName)}_$syntheticBindingCount"
  }

  private def bindingLocalAndSourceStmts(
    ctx: ParserRuleContext,
    bindingName: String,
    typeName: String,
    sourceKind: Option[String], // "from" | "in" | None (working memory)
    sourceExpr: Option[ARLParser.ExpressionContext]
  ): List[Ast] = {
    val local = localNode(ctx, bindingName, s"$bindingName: $typeName", typeName)
    declareValue(bindingName, typeName)

    val typeRef = Ast(typeRefNode(ctx, typeName, typeName))
    val rhsCall =
      (sourceKind, sourceExpr) match {
        case (_, None) =>
          operatorCallNode(ctx, s"$typeName", ArlOperators.workingMemory, Option(typeName))
        case (Some("in"), Some(src)) =>
          operatorCallNode(ctx, code(ctx), ArlOperators.matchIn, Option(typeName))
        case (_, Some(_)) =>
          operatorCallNode(ctx, code(ctx), Operators.cast, Option(typeName))
      }
    val rhsArgs    = sourceExpr.map(astForExpression).toList
    val rhs        = callAst(rhsCall, typeRef +: rhsArgs)
    val lhs        = Ast(identifierNode(ctx, bindingName, bindingName, typeName))
    val assignCall =
      callNode(
        ctx,
        s"$bindingName = ${rhsCall.code}",
        Operators.assignment,
        Operators.assignment,
        DispatchTypes.STATIC_DISPATCH
      )
    List(Ast(local), callAst(assignCall, Seq(lhs, rhs)))
  }

  /** `b: T(test?) from|in? e;` — a named binding pattern. */
  private def astsForBindingPattern(ctx: ARLParser.BindingPatternContext, negate: Boolean): (List[Ast], List[Ast]) = {
    val bindingName = stripBackticks(ctx.bindingName().getText)
    val typeName    = typeFullNameFromQualifiedName(ctx.qualifiedName())
    val kind        = terminalTexts(ctx).collectFirst { case "from" => "from"; case "in" => "in" }
    val exprs       = ctx.expression().asScala.toList
    // The test is the parenthesised expression; a source (from/in) is the trailing one.
    val (testExpr, sourceExpr) = exprs match {
      case single :: Nil =>
        if (kind.isDefined) (None, Some(single)) else (Some(single), None)
      case test :: src :: Nil => (Some(test), Some(src))
      case _                  => (None, exprs.lastOption)
    }
    patternPatternAsts(ctx, bindingName, typeName, testExpr, kind, sourceExpr)
  }

  private def typeFullNameFromQualifiedName(qn: ARLParser.QualifiedNameContext): String =
    resolveSimpleOrQualified(qualifiedNameText(qn))

  /** Shared lowering of a pattern with an optional binding: local + source stmt, plus the test as conjunct evaluated
    * with `bindingName` as implicit receiver.
    */
  private def patternPatternAsts(
    ctx: ParserRuleContext,
    bindingName: String,
    typeName: String,
    testExpr: Option[ARLParser.ExpressionContext],
    sourceKind: Option[String],
    sourceExpr: Option[ARLParser.ExpressionContext]
  ): (List[Ast], List[Ast]) = {
    val stmts    = bindingLocalAndSourceStmts(ctx, bindingName, typeName, sourceKind, sourceExpr)
    val conjunct = testExpr.map { test =>
      implicitReceiver.push(Some(bindingName -> typeName))
      try astForExpression(test)
      finally implicitReceiver.pop()
    }
    (stmts, conjunct.toList)
  }

  /** `T(test?) [from|in e];` — anonymous pattern: synthetic `$T_<n>` binding. */
  private def astsForClassPattern(ctx: ARLParser.ClassPatternContext): (List[Ast], List[Ast]) = {
    val typeName               = typeFullNameFromQualifiedName(ctx.qualifiedName())
    val bindingName            = nextSyntheticBinding(typeName)
    val kind                   = terminalTexts(ctx).collectFirst { case "from" => "from"; case "in" => "in" }
    val exprs                  = ctx.expression().asScala.toList
    val (testExpr, sourceExpr) = exprs match {
      case single :: Nil      => if (kind.isDefined) (None, Some(single)) else (Some(single), None)
      case test :: src :: Nil => (Some(test), Some(src))
      case _                  => (None, exprs.lastOption)
    }
    patternPatternAsts(ctx, bindingName, typeName, testExpr, kind, sourceExpr)
  }

  // ------------------------------------------------------------------
  // evaluate / where
  // ------------------------------------------------------------------

  /** `evaluate ( [b:] e1; e2; … );` — binding form binds e1, remaining expressions become conjuncts. */
  private def astsForEvaluate(ctx: ARLParser.EvaluatePatternContext): (List[Ast], List[Ast]) = {
    val exprs = ctx.expression().asScala.toList
    Option(ctx.patternBinding()) match {
      case Some(binding) =>
        val bindingName = stripBackticks(binding.getText.stripSuffix(":")).trim
        val local       = localNode(ctx, bindingName, s"$bindingName", Defines.Any)
        declareValue(bindingName, Defines.Any)
        val lhs    = Ast(identifierNode(ctx, bindingName, bindingName, Defines.Any))
        val rhs    = exprs.headOption.map(astForExpression).getOrElse(unknownAst(ctx))
        val assign = callNode(
          ctx,
          s"$bindingName = ${rootCode(rhs)}",
          Operators.assignment,
          Operators.assignment,
          DispatchTypes.STATIC_DISPATCH
        )
        val stmtAsts = List(Ast(local), callAst(assign, Seq(lhs, rhs)))
        (stmtAsts, exprs.drop(1).map(astForExpression))
      case None =>
        (List.empty, exprs.map(astForExpression))
    }
  }

  // ------------------------------------------------------------------
  // exists / not
  // ------------------------------------------------------------------

  /** `exists { classPatterns }` — binding stmts per pattern, one `<operator>.exists` conjunct of the tests. */
  private def astsForExists(
    patterns: List[ARLParser.ClassPatternContext],
    negated: Boolean,
    ctx: ParserRuleContext
  ): (List[Ast], List[Ast]) = {
    val lowered = patterns.map { pattern =>
      val typeName    = typeFullNameFromQualifiedName(pattern.qualifiedName())
      val bindingName = nextSyntheticBinding(typeName)
      (pattern, bindingName, typeName)
    }
    val stmtAsts = lowered.flatMap { case (pattern, bindingName, typeName) =>
      val kind            = terminalTexts(pattern).collectFirst { case "from" => "from"; case "in" => "in" }
      val exprs           = pattern.expression().asScala.toList
      val (_, sourceExpr) = exprs match {
        case single :: Nil   => (None, if (kind.isDefined) Some(single) else None)
        case _ :: src :: Nil => (None, Some(src))
        case _               => (None, exprs.lastOption.filter(_ => kind.isDefined))
      }
      bindingLocalAndSourceStmts(pattern, bindingName, typeName, kind, sourceExpr)
    }
    // Tests of each pattern, with that pattern's binding as implicit receiver.
    val testAsts = lowered.flatMap { case (pattern, bindingName, typeName) =>
      val exprs    = pattern.expression().asScala.toList
      val kind     = terminalTexts(pattern).collectFirst { case "from" => "from"; case "in" => "in" }
      val testExpr = (exprs, kind) match {
        case (single :: Nil, None)       => Some(single)
        case (test :: _ :: Nil, Some(_)) => Some(test)
        case (single :: Nil, Some(_))    => None
        case _                           => None
      }
      testExpr.map { test =>
        implicitReceiver.push(Some(bindingName -> typeName))
        try astForExpression(test)
        finally implicitReceiver.pop()
      }
    }
    val existsArgs =
      if (testAsts.nonEmpty) testAsts
      else
        lowered.map { case (pattern, bindingName, typeName) =>
          Ast(identifierNode(pattern, bindingName, bindingName, typeName))
        }
    val existsCall = callAst(
      operatorCallNode(ctx, s"exists ${patterns.map(code).mkString}", ArlOperators.exists, Option(Defines.Any)),
      existsArgs
    )
    val conjunct =
      if (negated) {
        callAst(
          operatorCallNode(ctx, s"not ${rootCode(existsCall)}", Operators.logicalNot, Option(Defines.Any)),
          List(existsCall)
        )
      } else existsCall
    (stmtAsts, List(conjunct))
  }

  // ------------------------------------------------------------------
  // aggregate
  // ------------------------------------------------------------------

  /** `label: aggregate { collects } do { proj; }` — locals for collects, then the aggregate call. */
  private def astsForAggregate(ctx: ARLParser.AggregatePatternContext, negated: Boolean): (List[Ast], List[Ast]) = {
    val labelName = ctx.aggregateLabel().getText match {
      case backticked if backticked.startsWith("`") => stripBackticks(backticked)
      case plain                                    => plain.trim
    }

    val collectResults = ctx.collectPattern().asScala.toList.map { collect =>
      val bindingName            = stripBackticks(collect.bindingName().getText)
      val typeName               = typeFullNameFromQualifiedName(collect.qualifiedName())
      val exprs                  = collect.expression().asScala.toList
      val inSource               = terminalTexts(collect).contains("in")
      val (testExpr, sourceExpr) = exprs match {
        case single :: Nil      => if (inSource) (None, Some(single)) else (Some(single), None)
        case test :: src :: Nil => (Some(test), Some(src))
        case _                  => (None, exprs.lastOption.filter(_ => inSource))
      }
      val stmts =
        bindingLocalAndSourceStmts(collect, bindingName, typeName, if (inSource) Some("in") else None, sourceExpr)
      val testAst = testExpr.map { test =>
        implicitReceiver.push(Some(bindingName -> typeName))
        try astForExpression(test)
        finally implicitReceiver.pop()
      }
      (stmts, bindingName, typeName, testAst)
    }

    val projection          = ctx.projection()
    val (projType, isCount) =
      if (terminalTexts(projection).contains("count") || projection.getText.startsWith("count")) ("int", true)
      else (resolveSimpleOrQualified(qualifiedNameText(projection.qualifiedName())), false)
    val projTypeRef = Ast(typeRefNode(projection, projType, projType))
    val labelType   = if (isCount) "int" else projType

    val labelLocal = localNode(ctx, labelName, labelName, labelType)
    declareValue(labelName, labelType)

    val aggArgs =
      List(projTypeRef) ++
        collectResults.map { case (_, bindingName, typeName, _) =>
          Ast(identifierNode(ctx, bindingName, bindingName, typeName))
        } ++
        collectResults.flatMap(_._4)
    val aggCall = operatorCallNode(ctx, code(ctx), ArlOperators.aggregate, Option(labelType))
    val aggRhs  = callAst(aggCall, aggArgs)
    val lhs     = Ast(identifierNode(ctx, labelName, labelName, labelType))
    val assign  = callNode(
      ctx,
      s"$labelName = ${code(ctx)}",
      Operators.assignment,
      Operators.assignment,
      DispatchTypes.STATIC_DISPATCH
    )

    val stmtAsts  = collectResults.flatMap(_._1) ++ List(Ast(labelLocal), callAst(assign, Seq(lhs, aggRhs)))
    val conjuncts =
      if (negated) {
        val labelIdent = Ast(identifierNode(ctx, labelName, labelName, labelType))
        List(
          callAst(operatorCallNode(ctx, s"not $labelName", Operators.logicalNot, Option(Defines.Any)), List(labelIdent))
        )
      } else List.empty
    (stmtAsts, conjuncts)
  }
}
