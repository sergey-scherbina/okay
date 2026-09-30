## freer-paramonad-row — an indexed effect in a row beside State, State's handler forwarding the index: where Freer's parameters live in the effect system

The operator's follow-up to freer-paramonad ("где и как в системе
эффектов?"), answered in TestFreerPara by compiling, green at the first
typing: a row `[S, R, X] =>> PSt[S, R, X] | At[State[Int, *], S, R,
X]` — the indexed typestate effect beside an ordinary `State % Int`
lifted ON THE DIAGONAL (`At.Op[F, R, X](e) extends At[F, R, R, X]`,
what `Lift`'s phantom index cannot say and a handler's loop over a
mixed row needs: the existential middle index of a matched `Bind` is
then `T <: R`) — and State's handler in its library shape over it: own
operations answered from the threaded Int, `PSt` forwarded with the
index it came with, the result run through the indexed natural
transformation into shift bodies. A counter ticked around an `Int ->
List[String]` move answers `(List("x", "x"), (3, 3))`. specs/
freer-base.md "The indexes INSIDE the effect system" names what
production would add (three-ary `+`, `split` over three-ary
constructors, `!`'s doors off `Unit`, `At` vs a one-cast diagonal
extractor — a measurement). Gate: additive — TestFreerPara 7/7 +
`affected master Test/compile`.
