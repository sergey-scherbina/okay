## arrows-guide - the tutorial for arrows, with its examples executed

`docs/arrows.md`, asked for by the operator, linked from the docs
index, the root README and the optics page. Every code block is run by
`TestArrowsGuide` in okay-lex — the discipline `docs/optics.md` set the
same morning, for the same reason: a tutorial whose examples are not
executed rots at the first rename.

WHAT IT COVERS, in the order a reader meets it: the glyphs on a plain
function (which is an arrow, so they can be tried immediately); why
Kleisli composition is `>=>` and not `>>>`; `Mealy` as the place
arrows and STREAMS genuinely meet — two machines over one input in a
single pass with `&&&`, a machine then a plain step with `>>>`; why
optics and streams do NOT meet and that being a decision rather than a
gap; the applicative slot as the one piece of real magic; and direct
style, where optics work and a block with an optic in it stays
applicative.

TWO THINGS THE TESTS TAUGHT THE PAGE rather than the other way round.
`>>>` composes within ONE carrier: `counting >>> A.arr(f)` does not
typecheck when `A` is `Function1`'s arrow and `counting` is a `Mealy`
— the function has to be lifted by the machine's own `arr`, and the
page says so because the compiler said it first. And a machine that
names itself needs a `lazy val`: a stateless `Mealy` answers with
ITSELF as the next machine, which a strict `val` cannot express.
`Mealy.arr` is written the same way for the same reason.

WHAT THE PAGE REFUSES TO CLAIM: that arrows replace monads. The
opening states the trade in one line — a monad hides its shape because
`flatMap` takes a function, which is right when the shape depends on
the answer and a loss when it does not — and leaves it there.
