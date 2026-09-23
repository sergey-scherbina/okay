## frege-typed-operations - okay-frege compiles clean, and operations are typed

The Frege compiler's "application of fromObj will diverge" warning is
gone, and Frege warnings now fail the build. They were logged at debug,
so the gate never saw them; `fregeCompile` now fails on any `W` line.

The fix is a better type, not a silenced one. `Operation a` is an okay
operation typed by its answer (a phantom over `java.lang.Object {}`),
and `perform :: Operation a -> Prog a`. The native that makes an
operation now fixes what `perform` answers, as `effect[F, A](op: F[A])`
does in okay. Using an `Operation Long`'s answer as a `String` is a
Frege type error; with the old `perform :: Obj -> Prog a` the caller
picked the type and it failed at run time.

The single cast from okay's `Object` answer stays `fromObj :: Obj -> a`,
now `private`. That is the only native shape for which Frege emits a
generic `(A)` cast: an `Answer a -> a` detour produced `(Object)x`,
which javac refused. It carries Frege's own
`--- nowarn: application of fromObj will diverge`, as the Prelude does
for `error`. Frege's `Typecheck.checkReturn` was read for the exact
condition. Docs and specs/frege.md updated.
