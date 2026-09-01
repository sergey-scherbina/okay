# The theory textbook — where okay comes from, with the papers

## Overview
okay's code already cites its sources in passing — `Cont.scala` names
Danvy and Filinski in its opening comment, `Logic.scala` names LogicT,
`Cont.scala` names Carette–Kiselyov–Shan for its staging trick. What
does not exist is the connected account: which theories the library is
built from, who established them, where to read them, and — for each
design decision — why THIS point in the design space, argued from the
cited work and from the library's own measurements.

That is what this adds: `docs/theory/`, a page per chapter, okay code
as the running examples. It is a textbook in the sense that it teaches
the theory through one concrete library, not a survey; a reader who
finishes it can read okay's core files and know why each is shaped as
it is.

## Interface
The deliverable is documentation. Its "interface" is the contract each
chapter keeps:

- every claim about okay carries a `file:line` or a quoted signature;
- every theoretical claim carries a citation with author, title, venue
  and year;
- every "why" is argued, not asserted — from the papers, or from a
  measurement recorded in `src/jmh/history.tsv` / `docs/benchmarks.md`;
- refuted designs are shown WITH their refutations (the house already
  writes this way: `docs/existentials.md`, the benchmarks page).

Chapters:

| file | subject | anchor citations |
|---|---|---|
| `README.md` | the map, the notation (`A ! F`, `%`, `+`, `/>`), how to read | — |
| `01-monads.md` | functors, monads, why effects need them | Moggi 1989/1991; Wadler 1992/1995 |
| `02-continuations.md` | continuations; delimited control, shift/reset, prompts; Cont as the foundation | Felleisen 1988; Danvy & Filinski 1990; Filinski 1994; Bjarnason 2012 |
| `03-parameterised.md` | parameterised (indexed) monads; answer-type modification; typestate | Atkey 2009 |
| `04-free-freer.md` | free monads, freer monads, the left-nested-bind problem and its answers | Swierstra 2008; Kiselyov & Ishii 2015; Voigtländer 2008; van der Ploeg & Kiselyov 2014 |
| `05-effects-handlers.md` | algebraic effects, handlers, extensible rows; okay's unions and TypeableK; the Writer story | Plotkin & Power 2003; Plotkin & Pretnar 2009; Kiselyov, Sabry & Swords 2013 |
| `06-tagless-staging.md` | final tagless; staging; okay's two stagings (inline/summonFrom and the reified Pipeline) | Carette, Kiselyov & Shan 2009; Taha & Sheard 1997/2000 |
| `07-logic-streams.md` | nondeterminism and msplit; streams as codata; folds as algebra; the sketches | Kiselyov, Shan, Friedman & Sabry 2005; Wadler 1985; Flajolet et al. 2007; Cormode & Muthukrishnan 2005; Dunning & Ertl 2019 |

## Behavior
- [ ] every chapter compiles its claims: each okay example is quoted
      from the tree as it stands, with `file:line`, and a spot-check of
      the quotes against the sources passes
- [ ] the citations are verifiable: author, title, venue, year for
      every paper, checked against the published record
- [ ] the decisions are argued: at minimum — why freer over free, why
      Cont founds the tower, why rows are unions rather than
      transformer stacks, why the Writer operation became a GADT, why
      staging is done twice in two different ways, why msplit is the
      one Logic primitive — each grounded in a citation or a recorded
      measurement
- [ ] the book is linked from `docs/README.md` and cross-linked with
      the typepedia and `docs/existentials.md`; no chapter duplicates
      them — it cites them
- [ ] refutations are kept: the six Writer encodings, the benchmark
      reversals (fold attribution, cluster flush policy) appear as
      worked examples of the method, not as embarrassments edited out

## Out of scope
- Category theory beyond what the code exercises. Monads are presented
  as Moggi and Wadler present them — a programming structure with laws
  — not via adjunctions; a pointer to further reading suffices.
- A Haskell-vs-Scala comparison. The sources are mostly Haskell-shaped;
  the book translates them into okay's Scala 3 without litigating the
  languages.
- API reference material. The typepedia owns that; the book explains
  why the API is what it is.

## Decisions
- **A page per chapter under `docs/theory/`, not one long file** —
  chosen because the chapters are independently linkable from code
  comments and specs, which is how the house documentation is actually
  read. Rejected: one `THEORY.md`, which would be the book nobody
  opens.
- **okay as the single running example** — chosen because the request
  is a textbook of THIS library's foundations, and because a worked
  example the reader can run beats a survey. Rejected: neutral
  pseudocode, which would sever the file:line contract.
- **Citations inline, verified, with a references section per
  chapter** — the years and venues checked, because a textbook that
  misattributes its sources teaches the wrong lesson twice.
