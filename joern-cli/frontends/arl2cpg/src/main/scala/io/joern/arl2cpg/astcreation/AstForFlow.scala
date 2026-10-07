package io.joern.arl2cpg.astcreation

import io.joern.arl2cpg.ArlOperators
import io.joern.arl2cpg.identity.{TaskIdentityKey, TaskIdentityRecord}
import io.joern.arl2cpg.parser.ARLParser
import io.joern.arl2cpg.rfl.RuleflowMeta
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

import java.nio.file.Paths
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
    // A ruleflow is scoped by name: unique rfl carrying `<name>X</name>`.
    val scope = rflMeta.filter(meta => meta.name == name) match {
      case List(one) => Option(one)
      case _         => None
    }
    val prevScope = currentTaskScope
    currentTaskScope = scope
    val call = callNode(
      ctx,
      taskName,
      taskName,
      resolvedCallFullName(taskName),
      DispatchTypes.STATIC_DISPATCH,
      Option("void()"),
      Option("void")
    )
    val body = blockAst(blockNode(ctx), List(callAst(call, List.empty)))
    val ast  = methodAstWithAnnotations(
      method,
      params,
      body,
      methodReturnNode(ctx, "void"),
      annotations = List(valueAnnotationAst(ctx, "arlKind", "ruleflow")) ++ scope.toList.flatMap(meta =>
        List(valueAnnotationAst(ctx, "ruleflowUuid", meta.uuid), valueAnnotationAst(ctx, "ruleflowName", meta.name))
      ) ++ List(valueAnnotationAst(ctx, "ruleflowScope", ruleflowScopeState(name)))
    )
    currentTaskScope = prevScope
    valueScope.pop()
    ast
  }

  private def ruleflowScopeState(name: String): String = {
    val matches = rflMeta.count(meta => meta.name == name)
    if (matches == 1) "resolved" else if (matches > 1) "ambiguous" else "unknown"
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
    fullNameSuffix: String = "void()",
    uuidSuffix: String = ""
  ): NewMethod =
    methodNode(
      ctx,
      taskName,
      code(ctx),
      s"$containerFullName.$taskName$uuidSuffix:$fullNameSuffix",
      Option(signature),
      parseResult.filename,
      astParentType = Option(NodeTypes.TYPE_DECL),
      astParentFullName = Option(containerFullName)
    )

  /** `flowtask name($p) { initial? flowSeq? final? }` → METHOD with initial+main+final in order. */
  private def astForFlowtask(ctx: ARLParser.FlowtaskDeclContext): Ast = {
    val name = nameOf(ctx.flowId())
    valueScope.push(mutable.Map.empty)
    val (scope, identity) = declScope(ctx, name)
    val prevScope         = currentTaskScope
    currentTaskScope = scope
    val method    = flowMethodNode(ctx, name, uuidSuffix = scopedSuffix(name, scope))
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
    val ast  = methodAstWithAnnotations(
      method,
      params,
      body,
      methodReturnNode(ctx, "void"),
      annotations = valueAnnotationAst(ctx, "arlKind", "flowtask") :: scopeAnnotationAsts(ctx, name, scope, identity)
    )
    currentTaskScope = prevScope
    valueScope.pop()
    ast
  }

  /** `functiontask name($p?) { initial? statement* final? }`. */
  private def astForFunctiontask(ctx: ARLParser.FunctiontaskDeclContext): Ast = {
    val name = nameOf(ctx.flowId())
    valueScope.push(mutable.Map.empty)
    val (scope, identity) = declScope(ctx, name)
    val prevScope         = currentTaskScope
    currentTaskScope = scope
    val method  = flowMethodNode(ctx, name, uuidSuffix = scopedSuffix(name, scope))
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
    val ast  = methodAstWithAnnotations(
      method,
      thisAst +: params,
      body,
      methodReturnNode(ctx, "void"),
      annotations =
        valueAnnotationAst(ctx, "arlKind", "functiontask") :: scopeAnnotationAsts(ctx, name, scope, identity)
    )
    currentTaskScope = prevScope
    valueScope.pop()
    ast
  }

  private def blockChildrenAsts(ctx: ARLParser.BlockContext): List[Ast] =
    withBlockScope(ctx.statement().asScala.toList.flatMap(astsForStatement))

  // ------------------------------------------------------------------
  // .rfl scoping — disambiguate same-named tasks via ruleflow metadata
  // ------------------------------------------------------------------

  /** The task identifier of a (possibly `flow>`-qualified) task name: its last `>` segment. */
  private def taskIdOf(name: String): String = name.split('>').last.trim

  /** All `call task:` target names (`flow>task` when qualified) referenced anywhere inside a declaration subtree. */
  private def callTaskTargets(ctx: ParserRuleContext): Set[String] =
    childrenOf(ctx).flatMap {
      case callCtx: ARLParser.CallTaskContext => List(taskRefName(callCtx.taskRef()))
      case _: TerminalNode                    => List.empty
      case child: ParserRuleContext           => callTaskTargets(child).toList
      case _                                  => List.empty
    }.toSet

  /** Whether `meta` can contain a call to `target`: the target id is one of its tasks, it lives in a flow referenced by
    * `meta`'s subflow edges, or `target` is qualified `F>X` and a ruleflow named `F` declares `X`.
    */
  private def callAcceptable(meta: RuleflowMeta, target: String): Boolean = {
    val id = taskIdOf(target)
    meta.taskIds.contains(id) || meta.taskIds.contains(target) ||
    meta.subflowTargets.values.toList
      .flatMap(targetUuid => rflMeta.find(subMeta => subMeta.uuid == targetUuid))
      .exists(subMeta => subMeta.taskIds.contains(id) || subMeta.taskIds.contains(target)) ||
    qualifiedScope(target).isDefined
  }

  private def nameMatchedCallAcceptable(meta: RuleflowMeta, target: String): Boolean = {
    val sep = target.lastIndexOf('>')
    if (sep >= 0) {
      target.take(sep).trim == meta.name && meta.taskIds.contains(taskIdOf(target))
    } else {
      val id = taskIdOf(target)
      meta.taskIds.contains(id) || meta.subflowTargets.values.toList
        .flatMap(targetUuid => rflMeta.find(subMeta => subMeta.uuid == targetUuid))
        .exists(subMeta => subMeta.taskIds.contains(id) || subMeta.taskIds.contains(target))
    }
  }

  /** For a `F>X` target/declared name: the unique ruleflow named `F` (everything before the last `>`) that contains
    * `X`; None when `target` is unqualified or no unique match exists.
    */
  private def qualifiedScope(target: String): Option[RuleflowMeta] = {
    val sep = target.lastIndexOf('>')
    if (sep < 0) None
    else {
      val prefix  = target.take(sep).trim
      val id      = taskIdOf(target)
      val matches =
        rflMeta.filter(meta => meta.name == prefix && (meta.taskIds.contains(id) || meta.taskIds.contains(target)))
      if (matches.size == 1) Option(matches.head) else None
    }
  }

  /** Refined scope candidates for a task declaration. Task-list matches preserve the existing call-acceptance rule; a
    * name-only match requires every call target to be present in that named ruleflow.
    */
  private def refinedTaskScopeCandidates(name: String, calledTargets: Set[String]): List[RuleflowMeta] =
    if (name.contains('>')) {
      qualifiedScope(name).toList.filter(meta => calledTargets.forall(target => callAcceptable(meta, target)))
    } else {
      val id              = taskIdOf(name)
      val taskListMatches = rflMeta.filter(meta => meta.taskIds.contains(id) || meta.taskIds.contains(name))
      val nameOnlyMatches = rflMeta.filter(meta =>
        meta.name == name && !meta.taskIds.contains(id) && !meta.taskIds
          .contains(name)
      )
      taskListMatches.filter(meta => calledTargets.forall(target => callAcceptable(meta, target))) ++
        nameOnlyMatches.filter(meta => calledTargets.forall(target => nameMatchedCallAcceptable(meta, target)))
    }

  private def taskScopeFor(name: String, calledTargets: Set[String]): Option[RuleflowMeta] =
    refinedTaskScopeCandidates(name, calledTargets) match {
      case List(one) => Option(one)
      case _         => None
    }

  private def taskIdentityFor(ctx: ParserRuleContext, name: String): Option[TaskIdentityRecord] =
    taskIdentity.records.get(TaskIdentityKey(fileBasename, Option(ctx.getStart).map(_.getLine).getOrElse(-1), name))

  private def taskDeclarationTokenIndex(ctx: ParserRuleContext, name: String): Int =
    Option(ctx.getStart)
      .map(_.getTokenIndex)
      .filter(_ >= 0)
      .getOrElse(throw new IllegalStateException(s"Task declaration '$name' has no start token index"))

  protected def precomputeDuplicateTaskScopes(
    declarations: List[(ParserRuleContext, String)]
  ): Map[Int, Option[RuleflowMeta]] = {
    val scopesByTokenIndex = mutable.Map.empty[Int, Option[RuleflowMeta]]
    declarations
      .groupBy(_._2)
      .toList
      .filter { case (name, _) => duplicateTaskNames.contains(name) && !name.contains('>') }
      .sortBy(_._1)
      .foreach { case (_, sameNameDeclarations) =>
        val considered = sameNameDeclarations
          .sortBy { case (ctx, taskName) => taskDeclarationTokenIndex(ctx, taskName) }
          .filterNot { case (ctx, taskName) => taskIdentityFor(ctx, taskName).isDefined }
        val candidateSets = considered.map { case (ctx, taskName) =>
          refinedTaskScopeCandidates(taskName, callTaskTargets(ctx)).toSet
        }
        val scopes = resolveDuplicateTaskCandidates(candidateSets)
        considered.zip(scopes).foreach { case ((ctx, taskName), scope) =>
          scopesByTokenIndex(taskDeclarationTokenIndex(ctx, taskName)) = scope
        }
      }
    scopesByTokenIndex.toMap
  }

  private def resolveDuplicateTaskCandidates(candidateSets: List[Set[RuleflowMeta]]): List[Option[RuleflowMeta]] = {
    val unionSize = candidateSets.flatten.toSet.size
    if (candidateSets.size == unionSize) eliminateTaskScopeCandidates(candidateSets)
    else {
      val singletonUuids = candidateSets.zipWithIndex.collect {
        case (candidates, index) if candidates.size == 1 =>
          candidates.head.uuid -> index
      }
      val conflicting = singletonUuids
        .groupBy(_._1)
        .collect { case (uuid, declarations) if declarations.size > 1 => uuid }
        .toSet
      candidateSets.map {
        case candidates if candidates.size == 1 && !conflicting.contains(candidates.head.uuid) =>
          Option(candidates.head)
        case _ => None
      }
    }
  }

  private def eliminateTaskScopeCandidates(candidateSets: List[Set[RuleflowMeta]]): List[Option[RuleflowMeta]] = {
    val remaining   = candidateSets.toArray
    val assignments = Array.fill(candidateSets.size)(Option.empty[RuleflowMeta])
    val conflicted  = mutable.Set.empty[Int]
    var continue    = true
    while (continue) {
      val singletonIndices = remaining.indices.iterator
        .filter(index => assignments(index).isEmpty && !conflicted.contains(index) && remaining(index).size == 1)
        .toList
      if (singletonIndices.isEmpty) {
        continue = false
      } else {
        val singletonGroups = singletonIndices.groupBy(index => remaining(index).head.uuid)
        val singletonSet    = singletonIndices.toSet
        singletonGroups.foreach {
          case (_, List(index)) => assignments(index) = Option(remaining(index).head)
          case (_, indices)     => conflicted ++= indices
        }
        val consumedUuids = singletonGroups.keySet
        remaining.indices
          .filterNot(index =>
            singletonSet.contains(index) || assignments(index).isDefined || conflicted.contains(index)
          )
          .foreach { index =>
            remaining(index) = remaining(index).filterNot(meta => consumedUuids.contains(meta.uuid))
          }
      }
    }

    val duplicateAssignments = assignments.zipWithIndex
      .collect { case (Some(meta), index) => meta.uuid -> index }
      .groupBy(_._1)
      .collect { case (_, declarations) if declarations.size > 1 => declarations.map(_._2) }
      .flatten
      .toSet
    assignments.zipWithIndex.map {
      case (Some(meta), index) if !conflicted.contains(index) && !duplicateAssignments.contains(index) => Option(meta)
      case _                                                                                           => None
    }.toList
  }

  /** `@uuid` suffix applied to the fullName of a scoped duplicate task name; empty otherwise. */
  private def scopedSuffix(name: String, scope: Option[RuleflowMeta]): String =
    scope.filter(_ => duplicateTaskNames.contains(name)).map(meta => s"@${meta.uuid}").getOrElse("")

  private def scopeState(name: String, scope: Option[RuleflowMeta]): String =
    if (scope.isDefined) "resolved"
    else if (duplicateTaskNames.contains(name) && (rflMeta.nonEmpty || taskIdentity.records.nonEmpty)) "ambiguous"
    else "unknown"

  /** `ruleflowUuid`/`ruleflowName`/`ruleflowScope` annotations for a task or ruleflow method, plus the
    * `taskIdentity*`/`taskQualifiedName` provenance annotations when a sidecar record scoped the declaration.
    */
  private def scopeAnnotationAsts(
    ctx: ParserRuleContext,
    name: String,
    scope: Option[RuleflowMeta],
    identity: Option[TaskIdentityRecord] = None
  ): List[Ast] =
    scope.toList.flatMap(meta =>
      List(valueAnnotationAst(ctx, "ruleflowUuid", meta.uuid), valueAnnotationAst(ctx, "ruleflowName", meta.name))
    ) ++ List(valueAnnotationAst(ctx, "ruleflowScope", scopeState(name, scope))) ++
      identity.toList.flatMap(rec =>
        List(
          valueAnnotationAst(ctx, "taskIdentitySource", rec.source),
          valueAnnotationAst(ctx, "taskIdentityVerified", taskIdentity.verified.toString)
        ) ++ rec.qualifiedName.toList.map(qn => valueAnnotationAst(ctx, "taskQualifiedName", qn))
      )

  /** The basename of the file currently being lowered — the join key for task identity records. */
  private def fileBasename: String = Paths.get(parseResult.filename).getFileName.toString

  /** Scope for a task declaration: an exact (file, line, name) identity record wins over `.rfl` inference. */
  private def declScope(ctx: ParserRuleContext, name: String): (Option[RuleflowMeta], Option[TaskIdentityRecord]) = {
    val identity = taskIdentityFor(ctx, name)
    val scope    = identity.map(identityScope).orElse {
      Option(ctx.getStart)
        .map(_.getTokenIndex)
        .flatMap(duplicateTaskScopesByTokenIndex.get)
        .getOrElse(taskScopeFor(name, callTaskTargets(ctx)))
    }
    (scope, identity)
  }

  /** A sidecar record as scope: reuse the loaded `.rfl` meta of the same uuid when it exists (unioning the
    * compiler-derived task membership), else synthesize a meta from the record itself.
    */
  private def identityScope(rec: TaskIdentityRecord): RuleflowMeta = {
    val identityTasks = taskIdentity.tasksByUuid.getOrElse(rec.uuid, Set.empty)
    rflMeta.find(meta => meta.uuid == rec.uuid) match {
      case Some(meta) => meta.copy(taskIds = meta.taskIds ++ identityTasks)
      case None       =>
        RuleflowMeta(
          name = rec.qualifiedName.getOrElse(rec.uuid),
          uuid = rec.uuid,
          taskIds = identityTasks,
          subflowTargets = Map.empty,
          path = rec.path
        )
    }
  }

  /** `ruletask name(id) { initial? props* rules: sel; select? final? }`. */
  private def astForRuletask(ctx: ARLParser.RuletaskDeclContext): Ast = {
    val name = nameOf(ctx.flowId())
    valueScope.push(mutable.Map.empty)
    val (scope, identity) = declScope(ctx, name)
    val prevScope         = currentTaskScope
    currentTaskScope = scope
    val method    = flowMethodNode(ctx, name, uuidSuffix = scopedSuffix(name, scope))
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
    // ODM selects each rule at most once; the literal order is the first occurrence (as IBM's compiled
    // TaskDefinition.addRule lists show).
    val matchedRules = entries.flatMap(selectorMatchingRules).distinct

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
      annotations = valueAnnotationAst(ctx, "arlKind", "ruletask") ::
        ((propAnnotations :+ rulesAnnotation :+ selectionAnnotation) ++ scopeAnnotationAsts(ctx, name, scope, identity))
    )
    valueScope.pop()
    currentTaskScope = prevScope
    ast.withChildren(selectAsts.flatMap(_.nestedMethod).toList)
  }

  private case class SelectLowered(methodRef: Option[Ast], nestedMethod: Option[Ast])

  /** `select (T v) block` → nested METHOD `R.<task>$select:boolean(T)` referenced by a METHOD_REF. */
  private def astsForSelectBlock(ctx: ARLParser.SelectBlockContext, taskName: String): SelectLowered = {
    val varName = Option(ctx.Identifier())
      .map(_.getText)
      .orElse(Option(ctx.BacktickId()).map(backtick => stripBackticks(backtick.getText)))
      .getOrElse("selected")
    val varType   = typeFullName(ctx.`type`())
    val selName   = s"$taskName$$select"
    val selSig    = s"boolean($varType)"
    val selSuffix = scopedSuffix(taskName, currentTaskScope)
    val selFull   = s"$containerFullName.$selName$selSuffix:$selSig"

    val outerThis = thisParam
    valueScope.push(mutable.Map.empty)
    val varParam =
      parameterInNode(ctx, varName, varName, 1, isVariadic = false, EvaluationStrategies.BY_REFERENCE, varType)
    declareValue(varName, varType, varParam)
    val method = flowMethodNode(ctx, selName, signature = selSig, fullNameSuffix = selSig, uuidSuffix = selSuffix)
    val params = Seq(thisParamAst(ctx), Ast(varParam))
    val body   = blockAst(blockNode(ctx), blockChildrenAsts(ctx.block()))
    val selAst = methodAstWithAnnotations(
      method,
      params,
      body,
      methodReturnNode(ctx, "boolean"),
      annotations = List(valueAnnotationAst(ctx, "arlKind", "function"))
    )
    valueScope.pop()
    thisParam = outerThis
    val ref = Ast(methodRefNode(ctx, code(ctx), selFull, s"$selName:$selSig"))
    SelectLowered(Option(ref), Option(selAst))
  }

  /** Selector entries: `pkg.rule` exact, `pkg.*` the rules directly in package `pkg` (not in its sub-packages: a rule
    * task lists each sub-package separately, and IBM's compiled TaskDefinition members confirm it), `*` all. Whitespace
    * runs are normalized — compiled selectors may pad names differently than the rule declarations (`GBP D_01` ≡
    * `GBP D_01`).
    */
  private def selectorMatchingRules(entry: String): List[String] = {
    val normalized = entry.replaceAll("\\s+", " ").trim
    normalized match {
      case "*"                             => allRuleNames
      case prefix if prefix.endsWith(".*") =>
        val pkg = prefix.stripSuffix(".*")
        allRuleNames.filter(rule => packageOf(rule.replaceAll("\\s+", " ")) == pkg)
      case exact => allRuleNames.filter(rule => rule.replaceAll("\\s+", " ") == exact)
    }
  }

  /** `a.b.rule` -> `a.b`; a rule outside any package -> "". */
  private def packageOf(rule: String): String = {
    val dot = rule.lastIndexOf('.')
    if (dot < 0) "" else rule.substring(0, dot)
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
    val taskName = taskRefName(ctx.taskRef())
    val call     = callNode(
      ctx,
      code(ctx),
      taskName,
      resolvedCallFullName(taskName),
      DispatchTypes.STATIC_DISPATCH,
      Option("void()"),
      Option("void")
    )
    callAst(call, List.empty)
  }

  /** Full `flow>task` ref text; `taskNamePart` is `taskWord+`, so getText would collapse internal whitespace (`probe
    * subflow` → `probesubflow`) — slice the original text from the char stream.
    */
  private def taskRefName(taskRef: ARLParser.TaskRefContext): String = {
    val parts = taskRef.taskNamePart().asScala.toList.map { part =>
      Option(part.getStart)
        .flatMap(start => Option(start.getInputStream))
        .map(input =>
          input
            .getText(Interval.of(part.getStart.getStartIndex, part.getStop.getStopIndex))
            .trim
        )
        .getOrElse(part.getText.trim)
    }
    if (parts.nonEmpty) parts.mkString(">") else taskRef.getText.trim
  }

  /** `call task` target fullName: a duplicated target is suffixed with the caller's scope uuid when the scope's task
    * list contains it, or with a referenced subflow's uuid when the target lives there. Otherwise the plain unscoped
    * name (deliberately unresolved rather than guessed).
    */
  private def resolvedCallFullName(taskName: String): String = {
    val duplicated =
      duplicateTaskNames.contains(taskName) || duplicateTaskNames.contains(taskIdOf(taskName))
    val uuidSuffix = currentTaskScope match {
      case Some(scope) if duplicated =>
        val id = taskIdOf(taskName)
        // An explicit `flow>task` qualifier pins the target flow before scope-membership is consulted.
        qualifiedScope(taskName).map(meta => s"@${meta.uuid}").getOrElse {
          if (scope.taskIds.contains(id) || scope.taskIds.contains(taskName)) {
            s"@${scope.uuid}"
          } else {
            scope.subflowTargets.values.toList
              .flatMap(targetUuid => rflMeta.find(meta => meta.uuid == targetUuid))
              .find(meta => meta.taskIds.contains(id) || meta.taskIds.contains(taskName))
              .map(meta => s"@${meta.uuid}")
              .getOrElse("")
          }
        }
      case _ => ""
    }
    s"$containerFullName.$taskName$uuidSuffix:void()"
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
    val jumpTarget = NewJumpTarget()
      .name(label)
      .code(s"$label:")
      .lineNumber(line(ctx))
      .columnNumber(column(ctx))
    blockAst(block, Ast(jumpTarget) :: astForFlowSeqStatements(ctx.flowSeq()))
  }

  /** `goto L;` → CONTROL_STRUCTURE GOTO. */
  private def astForGoto(ctx: ARLParser.GotoStmtContext): Ast = {
    val label = Option(ctx.Identifier()).map(_.getText).getOrElse("<label>")
    gotoAst(ctx, s"goto $label", label)
  }
}
