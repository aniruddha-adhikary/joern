package io.joern.arl2cpg.astcreation

import io.joern.arl2cpg.parser.ARLParser
import io.joern.x2cpg.{Ast, AstCreatorBase, Defines}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EvaluationStrategies, NodeTypes, Operators}
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import org.antlr.v4.runtime.ParserRuleContext

import java.nio.file.Paths
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/** Lowering of the file skeleton: package/imports, signature, ruleset and rules. */
trait AstForRules {
  this: AstCreator =>

  /** FILE + global NAMESPACE_BLOCK; children are the signature TYPE_DECL (if any) and the container TYPE_DECL that
    * carries rules, ruleflow and tasks as METHODs.
    */
  protected def astForCompilationUnit(unit: ARLParser.CompilationUnitContext): Ast = {
    packageName = Option(unit.packageDecl()).map(pd => qualifiedNameText(pd.qualifiedName()))
    explicitImports = unit.importDecl().asScala.toList.map(decl => qualifiedNameText(decl.qualifiedName()))

    // Pre-pass over rule declarations so `rules:` selectors can match forward references.
    val ruleDecls = ruleDeclsOf(unit)
    allRuleNames = ruleDecls.map(rd => nameOf(rd.ruleName()))

    // Pre-pass over task declarations: names declared more than once need .rfl scoping to stay distinct.
    val taskDeclNames = unit.flowElement().asScala.toList.flatMap { element =>
      childrenOf(element).headOption match {
        case Some(f: ARLParser.FlowtaskDeclContext)     => List(nameOf(f.flowId()))
        case Some(r: ARLParser.RuletaskDeclContext)     => List(nameOf(r.flowId()))
        case Some(f: ARLParser.FunctiontaskDeclContext) => List(nameOf(f.flowId()))
        case _                                          => List.empty
      }
    }
    duplicateTaskNames =
      taskDeclNames.groupBy(identity).collect { case (name, occurrences) if occurrences.size > 1 => name }.toSet

    val signatureAst = Option(unit.signatureDecl()).map(astForSignatureDecl)

    // Determine container TYPE_DECL: the ruleset name, or the file basename for bare-rule Designer files.
    val (containerName, containerFull) = Option(unit.rulesetDecl()) match {
      case Some(ruleset) =>
        val name = ruleset.Identifier(0).getText
        (name, packageName.map(pkg => s"$pkg.$name").getOrElse(name))
      case None =>
        val base = Paths.get(parseResult.filename).getFileName.toString.stripSuffix(".arl")
        (base, packageName.map(pkg => s"$pkg.$base").getOrElse(base))
    }
    containerFullName = containerFull

    val namespaceBlockAst = packageName match {
      case Some(pkg) =>
        Ast(namespaceBlockNode(unit, pkg, pkg, parseResult.filename))
      case None =>
        Ast(globalNamespaceBlock())
    }

    val fileNode = NewFile().name(parseResult.filename).order(1)
    Ast(fileNode).withChild(
      Ast(namespaceBlockAst.root.get.asInstanceOf[NewNamespaceBlock])
        .withChildren(signatureAst.toList :+ astForContainerTypeDecl(unit, containerName, containerFull))
    )
  }

  /** All rule declarations of the unit: inside the ruleset, or bare at top level for Designer files. */
  private def ruleDeclsOf(unit: ARLParser.CompilationUnitContext): List[ARLParser.RuleDeclContext] = {
    Option(unit.rulesetDecl()) match {
      case Some(ruleset) => ruleset.ruleDecl().asScala.toList
      case None          => unit.ruleDecl().asScala.toList
    }
  }

  /** `public signature S extends X { members }` → TYPE_DECL S with a MEMBER per parameter. */
  private def astForSignatureDecl(ctx: ARLParser.SignatureDeclContext): Ast = {
    val name       = Option(ctx.Identifier()).map(_.getText).getOrElse("<signature>")
    val fullName   = packageName.map(pkg => s"$pkg.$name").getOrElse(name)
    val superType  = resolveTypeName(Option(ctx.qualifiedName()).map(_.getText).getOrElse(""))
    val headerCode = s"signature $name extends ${Option(ctx.qualifiedName()).map(_.getText).getOrElse("")}"
    signatureFullName = Option(fullName)

    val typeDecl =
      typeDeclNode(ctx, name, fullName, parseResult.filename, headerCode, NodeTypes.NAMESPACE_BLOCK, "", Seq(superType))

    val memberAsts = ctx.signatureMember().asScala.toList.map(astForSignatureMember)
    Ast(typeDecl).withChildren(memberAsts)
  }

  /** `public in Borrower borrower = null;` → MEMBER borrower carrying an ANNOTATION `direction`. */
  private def astForSignatureMember(ctx: ARLParser.SignatureMemberContext): Ast = {
    val name      = Option(ctx.Identifier()).map(_.getText).getOrElse("<member>")
    val typeName  = Option(ctx.`type`()).map(typeFullName).getOrElse(Defines.Any)
    val direction = Option(ctx.direction())
      .map(dir => childrenOf(dir).map(_.getText).mkString(" "))
      .getOrElse("")

    signatureMembers(name) = typeName

    val member = memberNode(ctx, name, code(ctx), typeName)
    if (direction.nonEmpty) {
      val literal    = Ast(annotationLiteralNode(ctx, direction))
      val assign     = annotationAssignmentAst("value", direction, literal)
      val annotation = annotationNode(ctx, direction, "direction", "direction")
      Ast(member).withChild(annotationAst(annotation, List(assign)))
    } else {
      Ast(member)
    }
  }

  /** The container TYPE_DECL (ruleset or file-basename shim) and its METHOD children. */
  private def astForContainerTypeDecl(
    unit: ARLParser.CompilationUnitContext,
    containerName: String,
    containerFull: String
  ): Ast = {
    val anchorNode = Option(unit.rulesetDecl()).getOrElse(unit: ARLParser.CompilationUnitContext)
    val typeDecl   =
      typeDeclNode(anchorNode, containerName, containerFull, parseResult.filename, s"ruleset $containerName")

    // overriding { a > b; } — collect overrides so they can annotate the *overriding* rule a.
    val overrides: Map[String, List[String]] =
      Option(unit.rulesetDecl())
        .flatMap(rs => Option(rs.overridingDecl()))
        .map { od =>
          od.overridingPair()
            .asScala
            .toList
            .flatMap { pair =>
              val names = pair.ruleName().asScala.toList.map(nameOf)
              if (names.size >= 2) Some(names(0) -> names(1)) else None
            }
            .groupBy(_._1)
            .view
            .mapValues(_.map(_._2))
            .toMap
        }
        .getOrElse(Map.empty)

    val ruleAsts =
      ruleDeclsOf(unit).map(rd => astForRule(rd, containerFull, overrides.getOrElse(nameOf(rd.ruleName()), Nil)))
    val flowAst  = Option(unit.ruleflowDecl()).map(astForRuleflow)
    val taskAsts = unit.flowElement().asScala.toList.flatMap(astForFlowElement)

    Ast(typeDecl).withChildren(ruleAsts ++ flowAst.toList ++ taskAsts)
  }

  /** `rule `pkg.name` { props when? actions }` → METHOD on the container TYPE_DECL. */
  protected def astForRule(
    ctx: ARLParser.RuleDeclContext,
    containerFull: String,
    overriddenRules: List[String]
  ): Ast = {
    val name       = nameOf(ctx.ruleName())
    val fullName   = s"$containerFull.$name:void()"
    val signature  = "void()"
    val methodCode = s"rule ${Option(ctx.ruleName()).map(_.getText).getOrElse("<rule>")}"

    fileRuleNames += name
    valueScope.push(mutable.Map.empty)
    implicitReceiver.push(None)
    syntheticBindingCount = 0

    // Create the `this` parameter before the body so `this` identifiers inside it can REF it.
    val thisAst = thisParamAst(ctx)
    val method  = methodNode(
      ctx,
      name,
      methodCode,
      fullName,
      Option(signature),
      parseResult.filename,
      astParentType = Option(NodeTypes.TYPE_DECL),
      astParentFullName = Option(containerFull)
    )

    // when-lowering: binding statements then the conjuncts ANDed into an IF.
    val (bindingAsts, conjunctAsts) = Option(ctx.ruleWhen()) match {
      case Some(whenCtx) => astsForWhen(whenCtx)
      case None          => (List.empty, List.empty)
    }

    val actionAsts = ctx.ruleBody().ruleAction().asScala.toList.flatMap(astForRuleAction)

    val bodyStatements =
      if (conjunctAsts.isEmpty) {
        bindingAsts ++ actionAsts
      } else {
        val condition =
          conjunctAsts.reduceLeftOption((left, right) => andAsts(ctx, left, right)).getOrElse(trueLiteralAst(ctx))
        val thenBlock = blockAst(blockNode(ctx), actionAsts)
        val ifAst     = ifThenElseAst(ctx, Option(condition), thenBlock, None)
        bindingAsts :+ ifAst
      }

    val body      = blockAst(blockNode(ctx), bodyStatements)
    val methodRet = methodReturnNode(ctx, "void")

    val propertyAnnotations =
      ctx.ruleProperty().asScala.toList.map(astForRuleProperty) ++
        overriddenRules.map(target => overridesAnnotationAst(ctx, target))

    val ast =
      methodAstWithAnnotations(method, Seq(thisAst), body, methodRet, annotations = propertyAnnotations)
    valueScope.pop()
    implicitReceiver.pop()
    ast
  }

  private def andAsts(ctx: ParserRuleContext, left: Ast, right: Ast): Ast = {
    val call =
      operatorCallNode(ctx, s"${ctxCode(left)} && ${ctxCode(right)}", Operators.logicalAnd, Option(Defines.Any))
    callAst(call, Seq(left, right))
  }

  private def ctxCode(ast: Ast): String =
    rootCode(ast)

  private def trueLiteralAst(ctx: ParserRuleContext): Ast =
    Ast(literalNode(ctx, "true", "boolean"))

  private def overridesAnnotationAst(ctx: ParserRuleContext, target: String): Ast = {
    val literal    = Ast(annotationLiteralNode(ctx, target))
    val assign     = annotationAssignmentAst("value", s""""$target"""", literal)
    val annotation = annotationNode(ctx, s"overrides $target", "overrides", "overrides")
    annotationAst(annotation, List(assign))
  }

  /** `property k = v;` / `ilog.rules.x = v` / `status = v` / `k = v` → ANNOTATION on the rule method. */
  private def astForRuleProperty(ctx: ARLParser.RulePropertyContext): Ast = {
    val texts = terminalTexts(ctx)
    val expr  = ctx.expression()
    val name  =
      if (texts.contains("property")) {
        Option(ctx.Identifier()).map(_.getText).getOrElse("<property>")
      } else if (texts.take(3) == List("ilog", ".", "rules")) {
        s"ilog.rules.${Option(ctx.Identifier()).map(_.getText).getOrElse("<property>")}"
      } else if (texts.headOption.contains("status")) {
        "status"
      } else {
        // id '=' expression — the id is the first child context.
        Option(ctx.id()).map(_.getText).getOrElse("<property>")
      }
    val valueAst = astForExpression(expr)
    val assign   = annotationAssignmentAst("value", code(ctx), valueAst)
    annotationAst(annotationNode(ctx, code(ctx), name, name), List(assign))
  }
}
