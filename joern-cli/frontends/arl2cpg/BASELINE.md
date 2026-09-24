# arl2cpg completeness baseline

Measured with `sbt "arl2cpg/Test/runMain io.joern.arl2cpg.BaselineRunner <corpusRoot> <unknowns.tsv>"`,
which builds each corpus directory with `--allow-unknown`, lists every `UNKNOWN` node the `unknownAst`
fallbacks emit (AstCreator / AstForExpressions / AstForStatements / AstForFlow / AstForWhen) and records
which ANTLR parser rules the corpus exercises.

## Corpus

`ARL.g4` names `LoanValidation/ruleset.arl` and `dcs/dcs_rules.arl` as the corpora it was written against.
Neither file is present in the repositories reachable from this build: the DCS ruleset is an IBM build
product that was never checked in, and `LoanValidation` exists only as the arlgraph fixture `loan.arl`
(copied to `src/test/resources/b2x/loan.arl`). The baseline therefore uses every ARL file that *is*
available; the numbers below are the measured gap over that corpus, not over the two named files.

| group                    | source                                                       | files | lines  |
|--------------------------|--------------------------------------------------------------|------:|-------:|
| arl2cpg-test-resources   | `src/test/resources/arl`                                      |    10 |  9 983 |
| arl96                    | odm-for-ai `arl96.tar.xz` (ODM 9.6 decompiled archives)      |    18 | 24 894 |
| odm96-corpus             | odm-for-ai `odm96-corpus.tar.xz` (coverage rulesets)         |    77 |  5 167 |
| stress-permit-decide     | odm-for-ai `stress-permit-decide.tar.xz`                     |     1 |  2 748 |
| odm-for-ai-probes        | `arlgraph/probes/*.arl`                                      |     9 |  9 939 |
| odm-for-ai-fixtures      | `arlgraph/tests/fixtures/*.arl` (incl. LoanValidation)       |     9 |    711 |
| odm-for-ai-samples       | `arlgraph/samples/*.arl`                                     |     3 |  1 586 |
| odm-for-ai-evidence      | `arlgraph/evidence/*.arl`                                    |     2 |  5 496 |
| **total**                |                                                              | **129** | **60 524** |

## Result

| metric                                  | value |
|-----------------------------------------|------:|
| `UNKNOWN` nodes (all `unknownAst` sites) |     0 |
| files with syntax errors                |     0 |
| parser rules in `ARL.g4`                |    90 |
| parser rules exercised by the corpus    |    85 |

The first run found 2 files in `odm96-corpus` with syntax errors (`person.initial == 'A'`): the grammar had
no character literal. `CharLit` was added to `ARL.g4` and `astForLiteral`; the rerun is clean.

### Grammar alternatives that never lower to a real node

None on this corpus: every parse-tree node of the 129 files lowers to a typed CPG node.

### Alternatives the corpus does not exercise (lowering exists but is unmeasured)

| rule             | lowering                                        |
|------------------|-------------------------------------------------|
| `arrayAccess`    | `AstForExpressions.astForPostfix` (`<operator>.indexAccess`) |
| `overridingDecl` / `overridingPair` | `AstForRules.astForContainerTypeDecl` (rule `overrides` annotation) |
| `packageDecl`    | `AstForRules` namespace block (covered by unit tests, absent from the archives) |
| `thenNamedBlock` | `AstForStatements.astForThenNamedBlock`          |

These are the residual risk once the DCS ruleset becomes available; rerun the runner over it before retiring
`tools/arlgraph`.

## Gate 1

Since this change any `UNKNOWN`, any file with syntax errors and any unmodelled `b2x` element is recorded as a
`FINDING` (`code=unknown-construct | syntax-error | b2x-unmodelled-element`, with `filename`/`line`) and the
build fails with `Gate1Violation` unless `--allow-unknown` is given. Every dynamic call on ruleset data that
the frontend cannot follow into a body carries an `ARL_MAY_AFFECT:<reason>` tag and an
`unresolved-call-effects` FINDING; the reasons are arlgraph's: `callee-body-not-in-artifact`,
`receiver-type-unknown`, `class-not-in-b2x`, `no-b2x-body-for-method`, `b2x-overload-ambiguous`,
`b2x-body-unreadable`, `b2x-body-calls-method-without-body:<names>`.
