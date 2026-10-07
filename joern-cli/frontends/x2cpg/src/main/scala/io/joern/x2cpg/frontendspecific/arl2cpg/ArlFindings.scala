package io.joern.x2cpg.frontendspecific.arl2cpg

import io.shiftleft.codepropertygraph.generated.nodes.{AbstractNode, Finding, NewFinding, NewKeyValuePair, NewTag}
import io.shiftleft.codepropertygraph.generated.{Cpg, DiffGraphBuilder, EdgeTypes}
import io.shiftleft.semanticcpg.language.*

/** The machine-readable vocabulary arl2cpg uses to say what it could not do.
  *
  * Every gap in the graph is a FINDING node (see [[ArlFindings.finding]]) whose `evidence` is the node the gap belongs
  * to and whose key/value pairs carry a `code`, a `reason`, a centrally-derived severity, and location. Candidate and
  * skipped-BOM-file values are repeated key/value pairs and are declared list-valued in [[ListValuedKeys]]. Effects of
  * calls are TAG nodes on the CALL (see [[ArlTags]]). Neither ever replaces an AST node: the CPG stays a faithful
  * lowering, and the findings are the honest remainder — arlgraph's "never lose an edge, never guess" invariants
  * (DESIGN.md §0) in CPG terms.
  */
object ArlFindings {

  /** Finding key/value keys. */
  object Keys {
    val Code         = "code"
    val Reason       = "reason"
    val Message      = "message"
    val Filename     = "filename"
    val Line         = "line"
    val Author       = "author"
    val Severity     = "severity"
    val CallId       = "callId"
    val Candidates   = "candidates"
    val ReceiverType = "receiverType"
    val BomFiles     = "bomFiles"
  }

  val ListValuedKeys: Set[String] = Set(Keys.Candidates, Keys.BomFiles)

  val Author = "arl2cpg"

  /** Finding codes. */
  object Codes {

    /** A construct the parser accepted but the lowering has no rule for; evidence is the UNKNOWN node. */
    val UnknownConstruct = "unknown-construct"

    /** A file that did not parse cleanly; the AST under it is ANTLR's error recovery, not the author's program. */
    val SyntaxError = "syntax-error"

    /** A call whose effect on its receiver could not be read from any body in the artifact; evidence is the CALL. */
    val UnresolvedCallEffects = "unresolved-call-effects"

    /** A B2X element the reader does not model — an effect possibly missing from the graph. */
    val B2xUnmodelledElement = "b2x-unmodelled-element"
    val B2xMember            = "b2x-member"
    val B2xReturnTypeUnknown = "b2x-return-type-unknown"
    val B2xShadowsMethod     = "b2x-shadows-method"

    /** A Java call whose target could not be chosen from the receiver and static argument types. */
    val UnresolvedCallTarget = "unresolved-call-target"

    val BomMember         = "bom-member"
    val BomTypeUnresolved = "bom-type-unresolved"
    val BomDuplicateClass = "bom-duplicate-class"
    val BomIncludeMissing = "bom-include-missing"
    val BomFilesNotLoaded = "bom-files-not-loaded"
  }

  /** Reasons a call's effects stay unresolved (ported from arlgraph `Lowering.b2xEffects`). */
  object Reasons {
    val CalleeBodyNotInArtifact = "callee-body-not-in-artifact"
    val ReceiverTypeUnknown     = "receiver-type-unknown"
    val ClassNotInB2x           = "class-not-in-b2x"
    val NoB2xBodyForMethod      = "no-b2x-body-for-method"
    val B2xOverloadAmbiguous    = "b2x-overload-ambiguous"
    val B2xBodyUnreadable       = "b2x-body-unreadable"

    /** Prefix; the suffix lists the methods the body calls whose own body is nowhere in the artifact. */
    val B2xBodyCallsMethodWithoutBody = "b2x-body-calls-method-without-body"
  }

  /** Severity mapping: `unknown-construct` and `syntax-error` are `error`; `unresolved-call-target`,
    * `bom-type-unresolved`, `bom-duplicate-class`, `bom-include-missing`, `b2x-unmodelled-element`,
    * `b2x-return-type-unknown`, and `b2x-shadows-method` are `unresolved`; `unresolved-call-effects` is `info` only for
    * `callee-body-not-in-artifact` and `unresolved` otherwise; `b2x-member`, `bom-member`, and `bom-files-not-loaded`
    * are `info`. An unknown code fails instead of receiving a default.
    */
  private[arl2cpg] def severity(code: String, reason: String): String = (code, reason) match {
    case (Codes.UnknownConstruct | Codes.SyntaxError, _)                                  => "error"
    case (Codes.UnresolvedCallTarget, _)                                                  => "unresolved"
    case (Codes.UnresolvedCallEffects, Reasons.CalleeBodyNotInArtifact)                   => "info"
    case (Codes.UnresolvedCallEffects, _)                                                 => "unresolved"
    case (Codes.B2xUnmodelledElement, _)                                                  => "unresolved"
    case (Codes.B2xReturnTypeUnknown, _)                                                  => "unresolved"
    case (Codes.B2xShadowsMethod, _)                                                      => "unresolved"
    case (Codes.B2xMember, _)                                                             => "info"
    case (Codes.BomMember, _)                                                             => "info"
    case (Codes.BomFilesNotLoaded, _)                                                     => "info"
    case (Codes.BomTypeUnresolved | Codes.BomDuplicateClass | Codes.BomIncludeMissing, _) =>
      "unresolved"
    case (unknownCode, _) =>
      throw new IllegalArgumentException(s"Unknown ARL finding code '$unknownCode'")
  }

  def finding(
    builder: DiffGraphBuilder,
    evidence: Option[AbstractNode],
    code: String,
    reason: String,
    message: String,
    filename: String,
    line: Option[Int],
    additionalKeyValues: List[(String, String)] = Nil
  ): NewFinding = {
    val pairs = List(
      NewKeyValuePair().key(Keys.Author).value(Author),
      NewKeyValuePair().key(Keys.Code).value(code),
      NewKeyValuePair().key(Keys.Reason).value(reason),
      NewKeyValuePair().key(Keys.Severity).value(severity(code, reason)),
      NewKeyValuePair().key(Keys.Message).value(message),
      NewKeyValuePair().key(Keys.Filename).value(filename),
      NewKeyValuePair().key(Keys.Line).value(line.map(_.toString).getOrElse(""))
    ) ++ additionalKeyValues.map { case (key, value) => NewKeyValuePair().key(key).value(value) }
    val node = NewFinding().keyValuePairs(pairs).evidence(evidence.toList)
    builder.addNode(node)
    node
  }

  /** The value of `key` on a stored finding, empty when absent. */
  def value(finding: Finding, key: String): String =
    finding.keyValuePairs.find(_.key == key).map(_.value).getOrElse("")

  /** All values of `key` on a stored finding, in key/value-pair order. */
  def values(finding: Finding, key: String): List[String] =
    finding.keyValuePairs.filter(_.key == key).map(_.value).toList

  def code(finding: Finding): String   = value(finding, Keys.Code)
  def reason(finding: Finding): String = value(finding, Keys.Reason)

  def findings(cpg: Cpg, code: String): List[Finding] =
    cpg.finding.filter(f => ArlFindings.code(f) == code).l
}

/** TAG names attached to ARL CALL and B2X attribute METHOD nodes to record effects and literal metadata. */
object ArlTags {

  /** value: fullName of the B2X body METHOD the call resolves to. */
  val ResolvesTo = "ARL_RESOLVES_TO"

  /** value: `<receiver>.<field>` written by an assignment inside the resolved body (syntactic). */
  val Writes = "ARL_WRITES"

  /** value: `<receiver>.<field>` written by a setter-shaped call inside the resolved body (inferred). */
  val WritesInferred = "ARL_WRITES_INFERRED"

  /** value: `<receiver>.<field>` read inside the resolved body (syntactic). */
  val Reads = "ARL_READS"

  /** value: `<receiver>.<field>` read by a getter-shaped call inside the resolved body (inferred). */
  val ReadsInferred = "ARL_READS_INFERRED"

  /** value: the reason (see [[ArlFindings.Reasons]]) the call may affect its receiver in ways we cannot see. */
  val MayAffect = "ARL_MAY_AFFECT"

  /** value: `true` when an interval literal's lower bound is closed (`[`), otherwise `false`. */
  val IntervalLowerClosed = "ARL_INTERVAL_LOWER_CLOSED"

  /** value: `true` when an interval literal's upper bound is closed (`]`), otherwise `false`. */
  val IntervalUpperClosed = "ARL_INTERVAL_UPPER_CLOSED"

  /** Literal kind recovered from a B2X attribute getter body. */
  val LiteralKind = "ARL_LITERAL_KIND"

  /** Unescaped literal value recovered from a B2X attribute getter body, when the literal has a value. */
  val LiteralValue = "ARL_LITERAL_VALUE"

  def tag(builder: DiffGraphBuilder, node: AbstractNode, name: String, value: String): NewTag = {
    val tag = NewTag().name(name).value(value)
    builder.addNode(tag)
    builder.addEdge(node, tag, EdgeTypes.TAGGED_BY)
    tag
  }
}
