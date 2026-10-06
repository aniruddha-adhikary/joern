package io.joern.arl2cpg

/** Custom operators for the ODM ARL-specific constructs that have no counterpart in
  * [[io.shiftleft.codepropertygraph.generated.Operators]].
  */
object ArlOperators {

  /** `b: T(test);` — a when-pattern with no `from`/`in` source: the binding is drawn from working memory. */
  val workingMemory = "<operator>.workingMemory"

  /** `b: T(test) in e;` — pattern matching over the collection `e`. */
  val matchIn = "<operator>.matchIn"

  /** `label: aggregate { ... } do { proj; }` — the aggregate projection over its collected bindings. */
  val aggregate = "<operator>.aggregate"

  /** `exists { classPatterns }` — working-memory existence check over patterns. */
  val exists = "<operator>.exists"

  /** ARL interval literal: argument 1 is the lower bound and argument 2 the upper bound. The CALL carries boolean
    * `ARL_INTERVAL_LOWER_CLOSED` and `ARL_INTERVAL_UPPER_CLOSED` TAGs; `code` preserves the full bracketed interval
    * source text, including internal whitespace and excluding trailing selectors.
    */
  val interval = "<operator>.interval"

  /** `select (T r) { ... }` on a ruletask — dynamic rule filter over the candidate rules (its arguments). */
  val dynamicSelect = "<operator>.dynamicSelect"
}
