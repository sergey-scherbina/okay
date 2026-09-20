## direct-compiler-phases - the direct compiler is a class of phase traits

`Direct.compileAll` was one 1460-line method of ~70 nested defs sharing
a closure over the Quotes, the monad and the two mode flags — correct,
and unnameable: no phase could be tested apart from the whole macro's
behaviour. It is now `private[okay] final class DirectCompiler[F]`,
mixed from one trait per phase in its own file — DirectMarks, DirectRow,
DirectEmit, DirectDefer, DirectVals, DirectLoops, DirectParallel — over
a base DirectPhase that owns the Quotes given (at `q.type`, so a
dependent method a phase calls binds to THIS q), the `Out` enum and the
abstract recursion knot `compile`/`compileBlock`. Terms cross Quotes
paths as `Expr`. Direct.scala keeps the API at 608 lines
(01993eb4; spec section and board entry b2ff4fe7).

Every line moved and none rewritten, so emission is unchanged by
construction — 136 tests across every TestDirect* suite, green cold.
One name changed meaning on the way out of `object Direct`: `Effect`,
the auto-colouring marker `Direct.Effect` inside the object and the
narrower `okay.Effect` in the package, so `discardedMonadic` searched
for the wrong instance and the compiler said nothing. Qualified, and
pinned by a phase test that is red with the bare name.

Test-side macros `DirectProbe` run ONE phase over a block and answer
with data; `TestDirectPhases` asserts six phase decisions on their own
(specs/direct-macro.md, "Structure"): the defer pre-pass under both
modes and with no enclosing def, the mark count, the runnable element,
the drop predicate, the independent run, a spine's value slots.

Found on the way, recorded in memory rather than the boards: a WARM
`testOnly` after editing a macro's implementation ran the previous
compile's classes in the expansions — two suites red on a fix that was
in the source and in the .class. `rm -rf .jvm/target`, cold, was the
verdict. Edit a macro, gate cold.
