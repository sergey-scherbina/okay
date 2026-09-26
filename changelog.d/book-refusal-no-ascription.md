## book-refusal-no-ascription - chapter 16b: the refused for, without a type ascription

The one-expression call of team A's and team B's helpers is shown, and
pinned by `compileErrors`, as a bare `for` — no `val p: Choices[…]` in
front — so the only refusal is the one that matters: `Found: EitherT[List,
String, (String, Int)]`, `Required: OptionT[Checked, B]` (the `for`'s
`flatMap` belongs to `deliveryFee`'s `OptionT`).
