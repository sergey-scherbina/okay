## atm-beyond-state-docs - answer-type modification, illustrated without state

The operator's own question ("это нужно и можно только для state?")
exposed a gap: theory chapter 3's answer-type-modification section
had only state-shaped worked examples. Added: Asai's typed `printf`,
built directly on `Cont`'s `shift` — `lit`/`hole` — with `hole`'s
answer type growing an arrow (`T => S`) instead of staying `S`,
exactly Danvy & Filinski's original point about `shift`, no state
cell anywhere.

- docs/theory/03-parameterised.md: the printf example under "Instance
  one", `Prog` (freer-base stage 2) added to "Instance two" as the
  paramonad's third instance, two new references (Asai 2009, Danvy &
  Filinski 1990 "Abstracting control" — DOIs checked against
  Crossref, not assumed).
- src/test/scala/TestPrintfAtm.scala (3 tests): the one-hole case
  correct on more than one input; a literal alone needs `/` (its `A`
  is not its `S`, so `reset` does not apply); and a REFUTED attempt,
  kept as a `compileErrors` pin — two holes do not compose by nesting
  a further `flatMap`, because `bind` requires the next step to
  answer exactly the outer shift's `S`, which a second hole never
  does. The first draft assumed otherwise and was corrected against
  the compiler.
- specs/atm-beyond-state-docs.md carries the scope: one hole
  illustrates the mechanism completely; a general multi-argument
  printf is Asai's own paper's "three new solutions," not rebuilt
  here.
