# okay-parse

> Total, error-tolerant, lossless parsing: any token stream yields a
> tree; what did not parse is IN the tree as error nodes; a truncated
> stream is a tree with holes. Incremental reparse reuses unchanged
> subtrees by reference.

Depends on: `okay-lex`. Pure Scala — cross-built for JVM and JS.

## Guide

**One instruction language, two surfaces.** Both ways of writing a
parser — a hand-written driver (bottom-up, the uniml style) and
combinators (top-down) — emit the same four instructions:
`Open(kind, tok) / Emit(tok) / Close(tok) / Bad(tok, msg)`. Whoever
emits them, the same TOTAL builder folds the stream into the same
lossless CST. That convergence is the module's contract: drivers and
combinators are interchangeable and testable against each other.

**The builder absorbs damage.** `Parse.build` is a `Fold`: a `Close`
with nothing open becomes an error leaf ("nothing to close"); nodes
still open at the end are closed by `present` with an "unclosed"
marker. So a damaged input degrades one leaf, a truncated input (the
LLM streaming case) is a tree with holes — and `Throws` never
appears anywhere in the stack, by design.

**Lossless.** Trivia and punctuation are emitted, never skipped:
`Cst.lexemes(tree) == input`, byte for byte, damage included.
`Cst.errors` collects the diagnostics FROM the tree — errors are
data, not a side channel.

**Incremental reparse.** The driver contract that makes it sound: a
token maps to its instructions with NO cross-token state (all parser
state lives in the builder). Then:

- `Parse.full` folds token by token, snapshotting `(tokenIndex,
  Building)` at root-level node boundaries — the builder is a
  persistent structure, so a snapshot is a pointer, not a copy;
- `Parse.reparse` relexes (okay-lex reconvergence), resumes the
  builder from the nearest boundary before the damage, drives
  forward, and SPLICES once the token stream is the old one again at
  a matching boundary. Unchanged subtrees come back BY REFERENCE
  (`eq`) for a length-preserving edit; a length-changing edit rebases
  their spans (`Cst.rebase` — the absolute-span tax; relative spans
  are the stated future refinement). Driver and builder work is
  O(damage); no convergence found means a full, still-correct
  reparse.

## Tutorial

The full streaming pipeline — chars through the scanner stage through
the driver stage into the builder:

```scala
import okay.parse.{Cst, JsonParse, Parse}
import okay.lex.{Scan, Json as JsonLex}

val cst = Parse.toCst(
  through(through(chars(text))(Scan.stage(JsonLex.scan)))(JsonParse.driver)
    .toLazyList)

Cst.lexemes(cst) == text     // lossless
Cst.errors(cst)              // diagnostics, with exact spans
```

Truncation is not a fault:

```scala
val holes = parse("{\"a\": [1, 2")      // a tree; the array node is
Cst.errors(holes).nonEmpty              // closed with an "unclosed" marker
```

An editor session — parse once, then pay only for the damage:

```scala
val session = Parse.full(JsonLex.scan, JsonParse.instrs)(doc)
val re = Parse.reparse(JsonLex.scan, JsonParse.instrs)(
  session, doc, edited, editStart, editEndOld, editEndNew)

re.tree == Parse.full(JsonLex.scan, JsonParse.instrs)(edited).tree
// and for a same-length edit, untouched subtrees are the SAME objects
```

Write a dialect: give tokens kinds (a `Scan`), then either write a
driver (`Token => Vector[Instr]` if it can be per-token — that also
buys you reparse — or a `Stage` if it needs lookahead state), or
assemble combinators over `Stage.await`; the builder is already
written.

## API reference

| member | signature | meaning |
|---|---|---|
| `Instr[K]` | `Open(kind, tok?) / Emit(tok) / Close(tok?) / Bad(tok?, msg)` | the one instruction language |
| `Cst[K]` | `Node(kind, children) / Leaf(tok) / Err(tok?, msg)` | the lossless concrete syntax tree |
| `Cst.lexemes` | `Cst[K] => String` | the lossless law as a function |
| `Cst.errors` | `Cst[K] => Vector[(Option[Token], String)]` | diagnostics from the tree |
| `Cst.rebase` | `(c, offΔ, lineΔ) => Cst[K]` | shift every span (no-op when both deltas are 0) |
| `Parse.Driver[K, A]` | `Stage[Token[K], Instr[K], A]` | the parser side of a pipeline |
| `Parse.build` | `Fold[Instr[K], Building[K]]` | the total builder |
| `Parse.present` | `Building[K] => Cst[K]` | close the leftovers, produce the root |
| `Parse.toCst` | `IterableOnce[Instr[K]] => Cst[K]` | fold a finished stream |
| `Parse.full` | `(scan, step)(input, snapshotEvery) => Parsed[K, S]` | parse + lex snapshots + builder snapshots |
| `Parse.reparse` | `(scan, step)(old, oldInput, newInput, editStart, editEndOld, editEndNew, snapshotEvery)` | the incremental path |
| `JsonParse.driver` | `Driver[K, Unit]` | the streaming JSON driver |
| `JsonParse.instrs` | `Token[K] => Vector[Instr[K]]` | the per-token core (shared by driver and reparse) |
| `JsonParse.combinators` | `Driver[K, Unit]` | the same grammar, top-down |

## Gotchas

- The per-token driver contract is what reparse rests on: if your
  driver needs cross-token state, it still parses (as a Stage) but
  cannot reconverge token-wise.
- The reused suffix of a length-CHANGING edit is rebuilt leaf-by-leaf
  by `Cst.rebase` (spans are absolute) — reference reuse holds only
  for length-preserving edits.
- `Building` is public because `Parsed` carries it; treat it as
  opaque.

Measured (see [benchmarks](../benchmarks.md)): full parse vs
incremental reparse on a 50-member document — the O(damage) claim as
a number.
