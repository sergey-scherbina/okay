## book-delivery-identical - chapter 16b: one for, three ways, two shops everywhere

The delivery example is now the SAME `for` in all three versions — for
each shop, each variety and its price, then the fee, free when there is
none: `yield (tea, price + fee.getOrElse(0))`. Only the lifting differs:
`EitherT.liftF` and a hand `fromB` in cats, `reflect` for layers (each
team's helper reflects its own monads through the capabilities its
`reify` blocks hand out), `.at` for effects. The refused cats `for` is
the working one without `fromB`, and both of its errors are pinned: the
stacks (`EitherT[Option, …]` vs `EitherT[List, …]`), and `getOrElse` on
an `Int` — team B's "no fee" is an Option LAYER there, not a value. All
three agree (TestBookTwoMonadsCats 14).
