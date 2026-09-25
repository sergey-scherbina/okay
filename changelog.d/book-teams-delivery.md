## book-teams-delivery - chapter 16b: two teams with different monads, three ways

The "stacks do not compose" subsection now uses two teams whose stacks
share only the error and differ in the other monad: team A `EitherT[List]`
(choose a shop, price it), team B `OptionT[Either]` (a delivery fee a
shop may not offer; an unknown shop is an error).

- The two helpers in one for-comprehension do not compile; the refusal
  is quoted and pinned (`compileErrors`).
- The union stack `OptionT[EitherT[List]]` with one hand conversion per
  team; layered reflection with three shipped layers (List, Either,
  Option) and plain-value helpers; algebraic effects with the user's own
  Option-like effect (`absent` on `Stop`, `runOption` in three lines) and
  helpers on their own rows widened into `Choose + Stop + Throws % String`.
- All three asserted to give `List(Right(Some(850)), Right(None))`.
- TestBookTwoMonadsCats 11 -> 13.
