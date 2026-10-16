## warnings-keyed-root — the keyed-root landing clean: no warnings, Native linking, the recursion inventory true

A check of master at c13b12605 (okay-core pinned at da567f0, with shift-here) found the keyed-root landing left
three things. 85 compile warnings over the full build ("no warnings, ever"): unused imports in okay-std, okay-java,
okay-stm, scala2 and the root; `KeysOf.NoneK`/`AnyK` private to `okay` and reached from inline givens (an unstable
accessor each) — public now; erased evidence parameters (`Within.pair`, `Join.within`, `Join.contains`) `@unused`;
`Walk.placed`'s fallback unreachable but for null — `case null`. Scala Native did not link okay, okay-std or
okay-stm: `KeyOf.of` cached by a `java.lang.ClassValue`, which Native lacks — the cache is `KeyClasses` now, a
`ClassValue` on the JVM and JS (`src/main/scala-jvm-js`), a concurrent map on Native (`src/main/scala-native`). And
the stack-recursion inventory: three rows gone (`samePaths`, `walk`, `fitAt`) deleted, `runAll`'s `install` and
`relayed`'s `go` added, BOUNDED per `+` of the sum written in the program's type. Gated `affected master … staged`
on the three platforms: JVM 6545 green, no warnings; JS 1713 and Native 1318 with one failure, TestCatsEffectLaws'
onCancel law (backlog cats-js-oncancel-law; it passed on the JVM this run).
