package io.joern.arl2cpg.astcreation

import io.joern.arl2cpg.ArlOperators
import io.joern.arl2cpg.parser.ARLParser
import io.joern.x2cpg.{Ast, Defines}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{
  ControlStructureTypes,
  DispatchTypes,
  EvaluationStrategies,
  NodeTypes,
  Operators
}
import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.misc.Interval
import org.antlr.v4.runtime.tree.TerminalNode

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/** Layer A — ruleflow and tasks. */
trait AstForFlow {
  this: AstCreator =>

  /** `ruleflow F($p) { maintask T; }` → METHOD F whose body calls task T. */
  protected def astForRuleflow(ctx: ARLParser.RuleflowDeclContext): Ast = {
    val name = nameOf(ctx.flowId(0))
    // `.ruleflow.` infix avoids colliding with a maintask flowtask of the same name.
    val fullName = s"$containerFullName.ruleflow.$name:void()"
    valueScope.push(mutable.Map.empty)

    val method = methodNode(
      ctx,
      name,
      code(ctx),
      fullName,
      Option("void()"),
      parseResult.filename,
      astParentType = Option(NodeTypes.TYPE_DECL),
      astParentFullName = Option(containerFullName)
    )
    val paramName = ctx.dollarRef().getText
    val paramNode =
      parameterInNode(ctx, paramName, paramName, 1, isVariadic = false, EvaluationStrategies.BY_REFERENCE, Defines.Any)
    declareValue(paramName, Defines.Any, paramNode)
    val params   = Seq(thisParamAst(ctx), Ast(paramNode))
    val taskName = nameOf(ctx.flowId(1)) // 'maintask' flowId is the second flowId
    val call     = callNode(
      ctx,
      taskName,
      taskName,
      s"$containerFullName.$taskName:void()",
      DispatchTypes.STATIC_DISPATCH,
      Option("void()"),
      Option("void")
    )
    val body = blockAst(blockNode(ctx), List(callAst(call, List.empty)))
    val ast  = methodAst(method, params, body, methodReturnNode(ctx, "void"))
    valueScope.pop()
    ast
  }

  protected def astForFlowElement(ctx: ARLParser.FlowElementContext): List[Ast] = {
    childrenOf(ctx).headOption match {
      case Some(f: ARLParser.FlowtaskDeclContext)     => List(astForFlowtask(f))
      case Some(r: ARLParser.RuletaskDeclContext)     => List(astForRuletask(r))
      case Some(f: ARLParser.FunctiontaskDeclContext) => List(astForFunctiontask(f))
      case _                                          => List(unknownAst(ctx))
    }
  }

  private def flowMethodNode(
    ctx: ParserRuleContext,
    taskName: String,
    signature: String = "void()",
    fullNameSuffix: String = "void()"
  ): NewMethod =
    methodNode(
      ctx,
      taskName,
      code(ctx),
      s"$containerFullName.$taskName:$fullNameSuffix",
      Option(signature),
      parseResult.filename,
      astParentType = Option(NodeTypes.TYPE_DECL),
      astParentFullName = Option(containerFullName)
    )

  /** `flowtask name($p) { initial? flowSeq? final? }` → METHOD with initial+main+final in order. */
  private def astForFlowtask(ctx: ARLParser.FlowtaskDeclContext): Ast = {
    val name = nameOf(ctx.flowId())
    valueScope.push(mutable.Map.empty)
    val method    = flowMethodNode(ctx, name)
    val thisAst   = thisParamAst(ctx)
    val paramName = ctx.dollarRef().getText
    val paramNode =
      parameterInNode(ctx, paramName, paramName, 1, isVariadic = false, EvaluationStrategies.BY_REFERENCE, Defines.Any)
    declareValue(paramName, Defines.Any, paramNode)
    val params    = Seq(thisAst, Ast(paramNode))
    val bodyStmts =
      Option(ctx.initialBlock()).toList.flatMap(blk => blockChildrenAsts(blk.block())) ++
        Option(ctx.flowSeq()).toList.flatMap(astForFlowSeqStatements) ++
        Option(ctx.finalBlock()).toList.flatMap(blk => blockChildrenAsts(blk.block()))
    val body = blockAst(blockNode(ctx), bodyStmts)
    val ast  = methodAst(method, params, body, methodReturnNode(ctx, "void"))
    valueScope.pop()
    ast
  }

  /** `functiontask name($p?) { initial? statement* final? }`. */
  private def astForFunctiontask(ctx: ARLParser.FunctiontaskDeclContext): Ast = {
    val name = nameOf(ctx.flowId())
    valueScope.push(mutable.Map.empty)
    val method  = flowMethodNode(ctx, name)
    val thisAst = thisParamAst(ctx)
    val params  = Option(ctx.dollarRef()).map { dollar =>
      val paramName = dollar.getText
      val paramNode = parameterInNode(
        ctx,
        paramName,
        paramName,
        1,
        isVariadic = false,
        EvaluationStrategies.BY_REFERENCE,
        Defines.Any
      )
      declareValue(paramName, Defines.Any, paramNode)
      Ast(paramNode)
    }.toList
    val bodyStmts =
      Option(ctx.initialBlock()).toList.flatMap(blk => blockChildrenAsts(blk.block())) ++
        ctx.statement().asScala.toList.flatMap(astsForStatement) ++
        Option(ctx.finalBlock()).toList.flatMap(blk => blockChildrenAsts(blk.block()))
    val body = blockAst(blockNode(ctx), bodyStmts)
    val ast  = methodAst(method, thisAst +: params, body, methodReturnNode(ctx, "void"))
    valueScope.pop()
    ast
  }

  private def blockChildrenAsts(ctx: ARLParser.BlockContext): List[Ast] =
    ctx.statement().asScala.toList.flatMap(astsForStatement)

  /** `ruletask name(id) { initial? props* rules: sel; select? final? }`. */
  private def astForRuletask(ctx: ARLParser.RuletaskDeclContext): Ast = {
    val name = nameOf(ctx.flowId())
    valueScope.push(mutable.Map.empty)
    val method    = flowMethodNode(ctx, name)
    val thisAst   = thisParamAst(ctx)
    val paramName = Option(ctx.Identifier()).map(_.getText)
    val params    = paramName.map { param =>
      val paramNode =
        parameterInNode(ctx, param, param, 1, isVariadic = false, EvaluationStrategies.BY_REFERENCE, Defines.Any)
      declareValue(param, Defines.Any, paramNode)
      Ast(paramNode)
    }.toList

    val propAnnotations = ctx.ruletaskProperty().asScala.toList.map { prop =>
      val propName = childrenOf(prop).headOption.map(_.getText).getOrElse("property")
      val valueAst = Option(prop.expression())
        .map(astForExpression)
        .getOrElse(Ast(annotationLiteralNode(prop, Option(prop.Identifier()).map(_.getText).getOrElse(prop.getText))))
      val assign = annotationAssignmentAst("value", code(prop), valueAst)
      annotationAst(annotationNode(prop, code(prop), propName, propName), List(assign))
    }

    // rules: <selector>; — one STATIC call per matching rule of the same file.
    // `code()` slices the original file content so whitespace inside names is preserved.
    val selectorText = Option(ctx.ruleSelector()).map(code).getOrElse("").trim
    val entries      = selectorText.split(',').toList.map(_.trim).filter(_.nonEmpty)
    val matchedRules = entries.flatMap(selectorMatchingRules)

    val selectAsts = Option(ctx.selectBlock()).map(sb => astsForSelectBlock(sb, name))
    val hasSelect  = selectAsts.isDefined
    // A select predicate filters a candidate set; an empty rules clause means "all rules of the file".
    val candidateRules = if (entries.isEmpty && hasSelect) allRuleNames else matchedRules
    val ruleCallAsts   = candidateRules.map { ruleName =>
      val call = callNode(
        ctx,
        ruleName,
        ruleName,
        s"$containerFullName.$ruleName:void()",
        DispatchTypes.STATIC_DISPATCH,
        Option("void()"),
        Option("void")
      )
      callAst(call, List.empty)
    }
    // With a select block the candidates are arguments of one <operator>.dynamicSelect call (first arg:
    // the predicate METHOD_REF) so membership stays a superset and consumers can tell candidates from
    // statically-selected rules. Without it, bare rule calls as before.
    val selectionStmts =
      selectAsts match {
        case Some(select) =>
          val dynCall = callNode(
            ctx,
            Option(ctx.selectBlock()).map(code).getOrElse("select"),
            ArlOperators.dynamicSelect,
            ArlOperators.dynamicSelect,
            DispatchTypes.STATIC_DISPATCH,
            Option("void()"),
            Option("void")
          )
          List(callAst(dynCall, select.methodRef.toList ++ ruleCallAsts))
        case None => ruleCallAsts
      }

    val rulesAnnotation = {
      val value  = if (selectorText.nonEmpty) selectorText else if (hasSelect) "<dynamic>" else ""
      val assign = annotationAssignmentAst("value", value, Ast(annotationLiteralNode(ctx, value)))
      annotationAst(annotationNode(ctx, s"rules: $selectorText", "rules", "rules"), List(assign))
    }
    val selectionAnnotation = {
      val value  = if (hasSelect) "dynamic" else "static"
      val assign = annotationAssignmentAst("value", value, Ast(annotationLiteralNode(ctx, value)))
      annotationAst(annotationNode(ctx, s"selection: $value", "selection", "selection"), List(assign))
    }

    val bodyStmts =
      Option(ctx.initialBlock()).toList.flatMap(blk => blockChildrenAsts(blk.block())) ++
        selectionStmts ++
        Option(ctx.finalBlock()).toList.flatMap(blk => blockChildrenAsts(blk.block()))
    val body = blockAst(blockNode(ctx), bodyStmts)
    val ast  = methodAstWithAnnotations(
      method,
      thisAst +: params,
      body,
      methodReturnNode(ctx, "void"),
      annotations = propAnnotations :+ rulesAnnotation :+ selectionAnnotation
    )
    valueScope.pop()
    ast.withChildren(selectAsts.flatMap(_.nestedMethod).toList)
  }

  private case class SelectLowered(methodRef: Option[Ast], nestedMethod: Option[Ast])

  /** `select (T v) block` → nested METHOD `R.<task>$select:boolean(T)` referenced by a METHOD_REF. */
  private def astsForSelectBlock(ctx: ARLParser.SelectBlockContext, taskName: String): SelectLowered = {
    val varName = Option(ctx.Identifier())
      .map(_.getText)
      .orElse(Option(ctx.BacktickId()).map(backtick => stripBackticks(backtick.getText)))
      .getOrElse("selected")
    val varType = typeFullName(ctx.`type`())
    val selName = s"$taskName$$select"
    val selSig  = s"boolean($varType)"
    val selFull = s"$containerFullName.$selName:$selSig"

    valueScope.push(mutable.Map.empty)
    val varParam =
      parameterInNode(ctx, varName, varName, 1, isVariadic = false, EvaluationStrategies.BY_REFERENCE, varType)
    declareValue(varName, varType, varParam)
    val method = flowMethodNode(ctx, selName, signature = selSig, fullNameSuffix = selSig)
    val params = Seq(thisParamAst(ctx), Ast(varParam))
    val body   = blockAst(blockNode(ctx), blockChildrenAsts(ctx.block()))
    val selAst = methodAst(method, params, body, methodReturnNode(ctx, "boolean"))
    valueScope.pop()
    val ref = Ast(methodRefNode(ctx, code(ctx), selFull, s"$selName:$selSig"))
    SelectLowered(Option(ref), Option(selAst))
  }

  /** Selector entries: `pkg.rule` exact, `pkg.*` prefix, `*` all. Whitespace runs are normalized — compiled selectors
    * may pad names differently than the rule declarations (`GBP D_01` ≡ `GBP D_01`).
    */
  private def selectorMatchingRules(entry: String): List[String] = {
    val normalized = entry.replaceAll("\\s+", " ").trim
    normalized match {
      case "*"                             => allRuleNames
      case prefix if prefix.endsWith(".*") =>
        val stem = prefix.stripSuffix(".*") + "."
        allRuleNames.filter(rule => rule.replaceAll("\\s+", " ").startsWith(stem))
      case exact => allRuleNames.filter(rule => rule.replaceAll("\\s+", " ") == exact)
    }
  }

  // ------------------------------------------------------------------
  // flow statements
  // ------------------------------------------------------------------

  private def astForFlowSeqStatements(ctx: ARLParser.FlowSeqContext): List[Ast] =
    ctx.flowStatement().asScala.toList.flatMap(astsForFlowStatement)

  protected def astsForFlowStatement(ctx: ARLParser.FlowStatementContext): List[Ast] = {
    childrenOf(ctx).headOption match {
      case Some(c: ARLParser.CallTaskContext)     => List(astForCallTask(c))
      case Some(i: ARLParser.FlowIfContext)       => List(astForFlowIf(i))
      case Some(f: ARLParser.ForkStmtContext)     => List(astForFork(f))
      case Some(l: ARLParser.LabeledBlockContext) => List(astForLabeledBlock(l))
      case Some(g: ARLParser.GotoStmtContext)     => List(astForGoto(g))
      case Some(s: ARLParser.StatementContext)    => astsForStatement(s)
      case _                                      => List(unknownAst(ctx))
    }
  }

  /** `call task: flow > task;` → CALL to the full `flow>task` name, whitespace preserved per part. */
  private def astForCallTask(ctx: ARLParser.CallTaskContext): Ast = {
    // taskNamePart is `taskWord+`, so getText would collapse internal whitespace
    // (`probe subflow` → `probesubflow`); slice the original text from the char stream.
    val parts = ctx.taskRef().taskNamePart().asScala.toList.map { part =>
      Option(part.getStart)
        .flatMap(start => Option(start.getInputStream))
        .map(input =>
          input
            .getText(Interval.of(part.getStart.getStartIndex, part.getStop.getStopIndex))
            .trim
        )
        .getOrElse(part.getText.trim)
    }
    val taskName = if (parts.nonEmpty) parts.mkString(">") else ctx.taskRef().getText.trim
    val call     = callNode(
      ctx,
      code(ctx),
      taskName,
      s"$containerFullName.$taskName:void()",
      DispatchTypes.STATIC_DISPATCH,
      Option("void()"),
      Option("void")
    )
    callAst(call, List.empty)
  }

  private def astForFlowIf(ctx: ARLParser.FlowIfContext): Ast = {
    val cond  = astForExpression(ctx.expression())
    val seqs  = ctx.flowSeq().asScala.toList
    val thenA = wrapMultipleInBlock(seqs.headOption.toList.flatMap(astForFlowSeqStatements), line(ctx))
    val elseA =
      seqs.drop(1).headOption.map(seq => wrapMultipleInBlock(astForFlowSeqStatements(seq), line(ctx)))
    ifThenElseAst(ctx, Option(cond), thenA, elseA)
  }

  /** `fork {a} && {b} …` → BLOCK code `fork` with one BLOCK per branch. */
  private def astForFork(ctx: ARLParser.ForkStmtContext): Ast = {
    val branches = ctx.flowSeq().asScala.toList.map { seq =>
      blockAst(blockNode(seq), astForFlowSeqStatements(seq))
    }
    val forkBlock = NewBlock()
      .code("fork")
      .typeFullName(Defines.Any)
      .lineNumber(line(ctx))
      .columnNumber(column(ctx))
    blockAst(forkBlock, branches)
  }

  /** `L: {…}` → BLOCK with code `L:`. */
  private def astForLabeledBlock(ctx: ARLParser.LabeledBlockContext): Ast = {
    val label = Option(ctx.Identifier()).map(_.getText).getOrElse("<label>")
    val block = NewBlock()
      .code(s"$label:")
      .typeFullName(Defines.Any)
      .lineNumber(line(ctx))
      .columnNumber(column(ctx))
    blockAst(block, astForFlowSeqStatements(ctx.flowSeq()))
  }

  /** `goto L;` → CONTROL_STRUCTURE GOTO. */
  private def astForGoto(ctx: ARLParser.GotoStmtContext): Ast = {
    val label = Option(ctx.Identifier()).map(_.getText).getOrElse("<label>")
    gotoAst(ctx, s"goto $label", label)
  }
}
