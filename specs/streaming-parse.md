# okay-parse — streaming, error-tolerant parsing

## Overview
Parsing as a TOTAL function from token streams to trees: any input —
including a truncated or damaged one — yields a tree; what did not
parse is IN the tree as error nodes with spans, and diagnostics are a
data channel, not faults. Throws never appears. Built on
stage-pipeline.md and okay-lex; the pipeline is
`chars →(lex)→ tokens →(parse)→ instructions →(build: Fold)→ CST`.
Totality is what makes this the substrate for LLM streaming (a partial
JSON prefix is a valid tree with holes, refined as tokens arrive) and
for editors (see incremental reparse below).

## The two surfaces, meeting in the middle
Both are in scope from the start, converging (the user's direction:
"сразу в двух направлениях, снизу и сверху"):

- **Bottom-up: the VM.** The uniml instruction model — a dialect is a
  `Stage[Token[K], Instr, S]` emitting `Open(kind) / Close / Emit /
  Reframe(...)`; the builder is a `Fold[Instr, CST]` producing a
  lossless concrete syntax tree (punctuation, trivia, comments,
  ordering, spans, error nodes all preserved). Total by construction:
  unmatched Close, unclosed Open, unexpected tokens become error
  nodes/reframes.
- **Top-down: combinators.** `P[A]` — parser combinators as programs
  over `Take % Token[K]`, each combinator TOTAL: it always yields an
  A plus error-as-data (its A carries holes/defaults, diagnostics go
  to the Diag channel). Recovery (sync-token panic mode,
  insertion/deletion repair) are combinators, not exceptions.
- **The meeting point:** combinators COMPILE to instruction streams —
  a combinator run emits the same `Instr` language the VM speaks, so
  both surfaces produce the same CST, interoperate in one pipeline,
  and share the builder, the diagnostics channel, and the incremental
  machinery. This contract (one Instr language underneath) is the
  invariant that keeps the two directions converging instead of
  forking.

## Interface (sketch)
```scala
enum Instr:
  case Open(kind: String, role: Option[String] = None)
  case Close(expected: Option[String] = None)
  case Emit(role: Option[String] = None)          // attach current token
  case Reframe(...)                               // recovery restructuring
final case class Diag(severity: ..., span: Span, message: String)

type Driver[K, S] = Stage[Token[K], Instr, S]     // a dialect, bottom-up
def build: Fold[Instr, CST]                        // the total builder
// combinators: P[A] over Take % Token[K], with run-to-Instr
```
Diagnostics: one `Writer` channel of `Diag` beside `Instr` (a union of
class-distinct Writers, or one Writer of `Instr | Diag` — measured and
decided at implementation; see stage-pipeline.md).

## Incremental reparse (in the contract from day one)
- CST nodes carry their token ranges; parser state snapshots are
  values taken at node boundaries (tree-sitter's discipline, our
  pure-state version — a snapshot is just the driver's S plus the
  builder's stack summary).
- Reparse after an edit: relex the damaged region (okay-lex
  re-convergence), resume the driver from the nearest snapshot before
  the damage, run until the instruction stream re-converges; unchanged
  subtrees are REUSED by reference, not rebuilt.
- Same caller policy as lexing: an editor retains snapshots, a batch
  run retains none, one code path.

## Behavior
- [x] totality: every token stream yields a CST; truncated input
      yields a tree with holes (the LLM-streaming case), never a fault
- [x] lossless: concatenated lexemes of the CST == the input
      (round-trip through lex + parse + build, trivia included)
- [x] error nodes carry exact spans (their tokens do); Cst.errors
      collects the diagnostics from the tree
- [x] the VM driver and the combinator surface produce the same CST
      for the proving dialect (JSON), damaged inputs included
- [x] recovery: a bad token damages one leaf, not the tree; a close
      with nothing open is an error leaf (sibling recovery tests)
- [x] streaming: a prefix of the input yields a prefix-consistent
      tree that the remainder refines
- [ ] incremental: after an edit, unchanged subtrees are reused by
      reference and the reparse touches O(damage) — deferred: rides
      okay-lex reconvergence plus node-boundary driver snapshots; the
      relex layer is in place, the parse layer is the follow-up

## Out of scope
- semantic projections (JSON values, documents) — okay-codec
- grammars-as-data / parser generators (a later possibility over the
  same Instr language)

## Decisions
- **Two small modules, not one** (okay-lex / okay-parse), codecs a
  third — the module-minimalism policy.
- **Both surfaces from the start, one Instr language underneath** —
  the convergence contract above.
- **Errors as data everywhere** — Error tokens (lex), error nodes and
  Diag channel (parse); Throws is banned from the pipeline by design,
  which is also what makes every stage resumable and streamable.
