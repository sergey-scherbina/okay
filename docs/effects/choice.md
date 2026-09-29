# Choice and Logic

Nondeterminism: a program that can take several branches, run by a
handler that explores them. `choose(a, b, c)` forks the program; the
handler decides whether to collect every answer, the first, or a fair
mix of infinite branches. `Logic` adds backtracking search on top.

## Operations and handlers

| | |
|---|---|
| `choose(as*)` | one of these; no alternatives means failure |
| `runChoice(p)` | every answer, in order |
| `Logic.observe(n)(p)` | the first `n` answers, of a possibly infinite search |
| `Logic.interleave(a, b)` | a FAIR or: two infinite branches take turns |
| `Logic.fairBind(p)(f)` | a bind that cannot starve its siblings |
| `Logic.cut(p)` / `Logic.ifte(c)(th)(el)` | keep the first answer / the soft cut: `el` only when `c` has none |

The handler is multi-shot: it resumes the same continuation once per
alternative, which a one-shot effect runtime cannot do at all.

## Example

```scala
val sums: Int ! Choose =
  for
    a <- choose(1, 2)
    b <- choose(10, 20)
  yield a + b

val all = !.run(runChoice(sums))   // every branch: 11, 21, 12, 22

val firstTwo = !.run(Logic.observe(2)(sums))   // the first two: 11, 21
```

## Notes

- Alternatives are a `Seq`, and a `LazyList` is a `Seq`, so infinite
  choice points cost nothing to build; fairness is what makes them
  searchable.
- Other effects under a search follow handler order: state per branch or
  shared ([State](state.md), [Once](once.md)).
- Weighted choice with conditioning is [Prob](prob.md).

See also: `src/main/scala/Choice.scala`, `src/main/scala/Logic.scala`,
`specs/backtracking.md`; LogicT (Kiselyov, Shan, Friedman, Sabry 2005).
