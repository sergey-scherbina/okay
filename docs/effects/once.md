# Once

Call-by-need for programs: a shared sub-program runs at most once, and
every later demand gets the same answer. A plain shared program runs each
time it is reached, like a by-name parameter; `Once` is the `lazy val`
for programs, and because "runs at most once" is observable, it is an
effect rather than a hidden cache.

## Operations and handlers

| | |
|---|---|
| `Once.once(p)` | `p`, to run at most once; the first demand runs it |
| `p.handle(Once.memo)` | handle it |
| `Once.run(p)` | handle it: the memo cells live in the handler |

Once is counted per VALUE: `Once.once(p)` evaluated twice makes two
cells, so share the value to share the cell. In a `direct` block, a
`lazy val` with a mark does this for you.

## Example

```scala
var runs = 0
val expensive: Int ! Once = Once.once[Int, Pure] { runs += 1; pure(21) }
val both: Int ! Once = expensive.flatMap(a => expensive.map(b => a + b))

val answer = both.handle(Once.memo).run   // 42, and runs is 1
```

## Under a search

Handler order decides what a branch sees. `runChoice(Once.run(p))` gives
each branch its own cells, so nothing leaks between branches.
`Once.run(runChoice(p))` keeps one store for the whole search, so a later
branch sees what an earlier one computed. A cell demanded while its own
program is still running is a loud error, not a second run.

See also: `src/main/scala/Once.scala`, [Choice](choice.md),
[Prob](prob.md), where sharing a sub-model is this.
