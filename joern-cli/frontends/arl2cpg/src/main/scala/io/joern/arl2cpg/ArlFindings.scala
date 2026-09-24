package io.joern.arl2cpg

import io.shiftleft.codepropertygraph.generated.nodes.{AbstractNode, Finding, NewFinding, NewKeyValuePair, NewTag}
import io.shiftleft.codepropertygraph.generated.{Cpg, DiffGraphBuilder, EdgeTypes}
import io.shiftleft.semanticcpg.language.*

/** The machine-readable vocabulary arl2cpg uses to say what it could not do.
  *
  * Every gap in the graph is a FINDING node (see [[ArlFindings.finding]]) whose `evidence` is the node the gap belongs
  * to and whose key/value pairs carry a `code` (below), a `reason`, and location. Effects of calls are TAG nodes on the
  * CALL (see [[ArlTags]]). Neither ever replaces an AST node: the CPG stays a faithful lowering, and the findings are
  * the honest remainder — arlgraph's "never lose an edge, never guess" invariants (DESIGN.md §0) in CPG terms.
  */
object ArlFindings {

  /** Finding key/value keys. */
  object Keys {
    val Code     = "code"
    val Reason   = "reason"
    val Message  = "message"
    val Filename = "filename"
    val Line     = "line"
    val Author   = "author"
  }

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

  def finding(
    builder: DiffGraphBuilder,
    evidence: Option[AbstractNode],
    code: String,
    reason: String,
    message: String,
    filename: String,
    line: Option[Int]
  ): NewFinding = {
    val pairs = List(
      NewKeyValuePair().key(Keys.Author).value(Author),
      NewKeyValuePair().key(Keys.Code).value(code),
      NewKeyValuePair().key(Keys.Reason).value(reason),
      NewKeyValuePair().key(Keys.Message).value(message),
      NewKeyValuePair().key(Keys.Filename).value(filename),
      NewKeyValuePair().key(Keys.Line).value(line.map(_.toString).getOrElse(""))
    )
    val node = NewFinding().keyValuePairs(pairs).evidence(evidence.toList)
    builder.addNode(node)
    node
  }

  /** The value of `key` on a stored finding, empty when absent. */
  def value(finding: Finding, key: String): String =
    finding.keyValuePairs.find(_.key == key).map(_.value).getOrElse("")

  def code(finding: Finding): String   = value(finding, Keys.Code)
  def reason(finding: Finding): String = value(finding, Keys.Reason)

  def findings(cpg: Cpg, code: String): List[Finding] =
    cpg.finding.filter(f => ArlFindings.code(f) == code).l
}

/** TAG names attached to ARL CALL nodes to record what a call does to its receiver, and how we know. */
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

  def tag(builder: DiffGraphBuilder, node: AbstractNode, name: String, value: String): NewTag = {
    val tag = NewTag().name(name).value(value)
    builder.addNode(tag)
    builder.addEdge(node, tag, EdgeTypes.TAGGED_BY)
    tag
  }
}
