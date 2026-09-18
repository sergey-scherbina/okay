## optic-law-rewrites - two modifies through one optic are one, and the 2.7x is collected

`Fuse` knows the functor law now: `modify(g) . modify(f)` through ONE
optic becomes `modify(f andThen g)`. The compiler is licensed to use
that law and the JIT is not, which is why the prize was sitting there.

MEASURED, 8 iterations, 3 forks, 24 measurements a lane:

| lane | ns/op |
|---|---|
| `fuseTwiceByLaw` - two nested `Fuse.modify`, rewritten | **1524 +- 116** |
| `fuseTwiceHand` - the same fused by hand (the control) | 1421 +- 35 |
| `traversalTwice` - the same work, extension form | 4063 +- 142 |

2.7x, within 7% of the hand-written answer, bars nearly touching.

THE SHAPE WAS LEARNED FROM A PROBE, and three theories died first.
`Fuse.modify(o)(g)(Fuse.modify(o)(f)(v))` does NOT reach the outer
macro as a nested macro call: by the time the outer runs, the inner
has already emitted the INTERPRETATION, because the planner cannot
read a traversal -

    o.apply[Function1](g)(using fn).apply(s0)

so the rewrite matches that, not a macro call. Nothing here was
guessable, and the first probe LIED: `report.info` did not print for
the outer expansion while a file written from the same macro showed it
running perfectly. A diagnostic can be swallowed, and absence then
proves nothing.

WHAT IT REFUSES, each forced by the law rather than chosen: a
different optic (composing two is not the functor law) and a
type-changing optic (the law needs `S =:= T` and `A =:= B`). Both fall
through to the path that existed before.

AND WHERE IT CANNOT REACH, stated so nobody re-tries it: the EXTENSION
form. `o.modify(f)(s)` parses as `(o.modify(f))(s)` - the macro
answers a function and the application happens outside it, so the
outer never receives the inner. `traversalTwice` stays at 4063 in the
table above for exactly that reason, and it is in the table on
purpose.

THE TEST THAT MAKES THE LANE MEAN ANYTHING is not the one that checks
the answer - that passes with or without fusion. It is the one that
watches the two functions INTERLEAVE: fused, they alternate per
element (1,2,1,2); two passes run all the firsts and then all the
seconds. That is the intermediate Vector not being built, observed
rather than timed.

Not taken: the `set . set` half on products. Its prize is ~2.5 ns and
NOTHING in bytes on the JVM, because escape analysis already
scalar-replaces the intermediate; the entry keeps it with the caveat
that Native and JS have no such analysis and were never measured.

AND, ASKED THE SAME DAY: do optics and arrows work inside a `direct`
block? `TestDirectOpticsArrows` answers it as seven tests rather than
from memory — a lens read, a lens write, a composed lens, a traversal,
two `reflect`s with an optic between them (the block stays APPLICATIVE:
both errors, not the first), `fanout` on the `Function1` arrow, and
`compose` on the Kleisli. All green.

The one refusal is the one already recorded, and it is a test now: a
`.reflect` INSIDE the focus function does not compile — a mark under a
lambda is the corner `direct`'s v1 refuses by design. The CHANGELOG
sentence it came from, "an optic and a direct block meet at the call,
not inside it", reads much wider than it is. It is about a mark under
the focus lambda and about nothing else.
