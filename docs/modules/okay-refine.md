# okay-refine

> Reading anything out of anything, one pattern at a time: a typed
> hierarchy of patterns that recognise a document level by level —
> bytes, format, document, instrument — where a pattern is a prism
> whose read may decline, a path reads and writes back, and a choice
> runs every alternative and says `Unclear` rather than first-wins.

Depends on: `okay` (core), `okay-codec`, `okay-optics`. Pure Scala —
cross-built for JVM, JS and Native. Spec: `specs/refine.md`.

## Guide

**The problem it is for.** A file arrives and nothing is known about
it: not its format, not what it describes, not which of the forty
shapes of "an interest-rate swap" it is. Every question about it is
the same question at a different level — *is this one of these?* — and
every answer makes the next question askable. A parser for a
programming language does this over a grammar fixed in advance; here
the grammar is the sum of every pattern anyone has registered, and it
grows without touching what was there. The design bet, from a bank
project that failed at this twice for cognitive load: **the patterns
are the components.** Each is a value; the hierarchy is their
composition; the document model is what the composition produces.

**A pattern is a prism.** `Refine.step(name)(read)(write)`: `read`
is partial and says why it declines, `write` is total. Composing along
a path (`andThen`) reads *and* writes back, so a document read through
one branch and written through another is a conversion — lawful, for
free. `<|>` is a choice at one level.

**The answer is a `Verdict`, never a bare value.** `Took(value, path,
declined)` names the path of patterns that produced it and every
sibling that declined with its own reason; `Unclear(candidates, …)`
when two branches took the input; `Declined(tried)` with every path
and why. A hierarchy's whole worth over a hand-written `match` is
that the reader sees what was considered — for a risk system, a
silent first-wins is the defect this module exists to remove.

## Tutorial

Three steps, a path and a choice:

```scala
val int   = Refine.step[String, Int]("int")(s => s.toIntOption.toRight(s"'$s' is not an integer"))(_.toString)
val even  = Refine.step[Int, Int]("even")(n => if n % 2 == 0 then Right(n) else Left(s"$n is odd"))(identity)
val small = Refine.step[Int, Int]("small")(n => if n < 10 then Right(n) else Left(s"$n is not small"))(identity)
val evenOrSmall = int andThen (even <|> small)
val seven = evenOrSmall.run("7")    // Took(7, int/small, Vector(int/even: 7 is odd))
val four  = evenOrSmall.run("4")    // Unclear(Vector((int/even, 4), (int/small, 4)), Vector())
val bad   = evenOrSmall.run("x")    // Declined(Vector(int: 'x' is not an integer))
val seven2 = evenOrSmall.write(7)   // Right("7")
```

The first level shipped here is FORMAT — cbor, json, xml, yaml over
the codecs' own lossless trees, so a detector cannot drift from the
parser it stands for:

```scala
val verdict = Format.detect.run("""{"a": [1, 2]}""".getBytes(UTF_8))
val path = verdict match
  case Verdict.Took(_, by, _) => by.toString               // "text/json"
  case other => other.toString
val tried = verdict.reasons.map(_.at.toString)              // Vector("cbor", "text/xml", "text/yaml")
val back = verdict.toOption.flatMap(doc => Format.detect.write(doc).toOption)
val text = back.map(new String(_, UTF_8))                   // Some("""{"a": [1, 2]}""")
```

Into a sum: each alternative learns one case, `write` takes that case
only, and an `Or` asks its alternatives in order:

```scala
val num: Refine[String, AnyVal] = int.widen[AnyVal] <|> decimal.widen[AnyVal]
val half = num.run("4.5").toOption     // Some(4.5)
val i42  = num.write(42)               // Right("42")
val no   = num.write(true)             // Left("int|decimal: no alternative writes this value")
```

## API reference

| | |
|---|---|
| `Refine.step(name)(read)(write)` | a pattern: `A => Either[String, B]` and `B => A` |
| `r andThen s` | a path; the verdict's path is both names |
| `r <|> s`, `Refine.first(a, b, …)` | a choice; every alternative runs |
| `r.map(name)(to, from)` | an iso on what is learnt |
| `r.widen[C]` | into a sum; `write` accepts this branch's case only |
| `r.run(a): Verdict[B]` | `Took` / `Unclear` / `Declined`, each with its `Refusal`s |
| `r.write(b): Either[String, A]` | the way back along the path |
| `Refine.Step(…).prism` | the step as an optics `Prism`, for the laws |
| `Format.detect` | `Refine[Array[Byte], Doc]`: `cbor <|> (text andThen (json <|> xml <|> yaml))` |
| `Doc.Json / Xml / Yaml / Cbor` | the detected document, as the dialect's own tree (or the bytes) |

## Gotchas

- **A bare scalar is no document.** `hello` and `42` are declined by
  every text format: a level whose answer is "a string" has learnt
  nothing about what to ask next. Write the step if you want scalars.
- **YAML's block dialect and JSON.** `{"a": 1}` is a YAML flow mapping
  by the spec, but okay-codec's dialect is block-only and reads `{` as
  a scalar beside a mapping; `Format.yaml` declines a root-level scalar
  for exactly that reason. When flow style lands, the same input
  becomes `Unclear` naming both — by design.
- **The XML declaration** was read by the XML dialect as an unclosed
  tag until `xml-processing-instruction` (2026-09-29): every real FpML
  file was declined with `unclosed`. `<?…?>` and `<!DOCTYPE …>` are one
  token each now, and `TestFormat` reads a declared document as
  `text/xml`.
- **`write` is `Either` on a tree.** A step's prism review is total; an
  `Or` into a sum cannot know which alternative a case belongs to
  without asking, so the tree's `write` is partial and says so.

## Literature

- Pickering, Gibbons, Wu, *Profunctor Optics: Modular Data Accessors*
  (Programming 2017) — the prism as partial getter + total constructor;
  composition along a path is what makes a read-and-write-back pair.
- Kiselyov, Shan, Friedman, Sabry, *Backtracking, Interleaving, and
  Terminating Monad Transformers* (ICFP 2005) — the soft cut a later
  stage's "if this is a swap then … else …" is spelled with.
- Wirth, *Program Development by Stepwise Refinement* (CACM 1971) — the
  word, and the discipline: what is known narrows one decision at a time.
- Hutton, Meijer, *Monadic Parser Combinators* (1996) — parsers as
  values composed by choice and sequence, the shape this borrows for a
  grammar that is not fixed in advance.
- ISDA, *FpML* (Financial products Markup Language) and *Common Domain
  Model* — the two public corpora the private domain modules read; this
  module's format level is what they stand on.
