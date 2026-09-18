## arrow-glyphs - the literature's glyphs, and the rename that made room for them

`import okay.Optic.arrows.*` gives the operators every language with
arrows spells the same way: `>>>` and `<<<` for composition, `***`
for split, `&&&` for fanout, `+++` for a sum, `|||` for fanin.

BEHIND AN IMPORT, not in package scope, and that is the one design
decision here. These are extension methods on a bare `P[A, B]` -
which is every two-parameter type in the library - and
`universal-extension-apply-breaks-features` records what a universal
extension in package scope did last time: `import okay.*` disabled
named tuples for everyone.

TAKING `>>>` COST A RENAME, and finding that out was the lane.
`Monad.scala` held `>>>` for KLEISLI composition. An arrow `>>>` does
not sit beside that one, it COLLIDES: an effectful function
`Int => Option[Int]` is also a `P[A, B]` with `B = Option[Int]`, so
the arrow extension wins resolution and then cannot typecheck, the
next arrow having to start at `Option[Int]`. The first cut of this
object had that bug; `TestArrowGlyphs` is where it showed, and my
reasoning that there could be no collision had checked one direction
of two.

The fix was not to give up the glyph but to give each one the name
its own literature uses: `Control.Arrow.>>>` composes arrows,
`Control.Monad.>=>` composes Kleisli arrows. THE RENAME WAS FREE AND
OVERDUE - the Kleisli `>>>` had ZERO call sites (every `>>>` in the
tree was a `Long` bit shift or a local definition), and it was
BLOCKING: `TestProc` and `TestWorkflowProc` were each hand-rolling
their own `>>>` for `Proc` because the name was taken. Both use the
shared one now and their 30 tests still pass.

`Choice.left` EXISTS NOW TOO, asked for by the operator and missing
for no good reason. Hughes' `ArrowChoice` takes `left` as the
primitive and derives `right`; this library takes `right`, because
`Choice` is here for PRISMS and a prism's requirement is `right`. But
`Strong` has `first` and a derived `second`, and `Choice` had no
mirror at all. It does now, derived by swapping and overridable, so a
reader from `Control.Arrow` finds the name they expect - and `+++`
became the literature's definition verbatim: `left(p)` then
`right(q)`.

NO GLYPH FOR OPTIC COMPOSITION, and none for plain functions. An
optic composes with `andThen` and a second spelling is what
specs/unwrap-glyph.md refuses by name; a plain function IS an arrow,
so `f >>> g` already works on one.

Every glyph is tested against the named method it spells, on the same
inputs. A glyph that quietly means something else than the method it
claims is worse than no glyph.
