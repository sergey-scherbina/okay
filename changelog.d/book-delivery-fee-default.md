## book-delivery-fee-default - chapter 16b: price first, and a missing fee counts as zero

The delivery example's order no longer drops a shop that does not
deliver: every `for` looks the price up first, then the fee, and adds
`fee.getOrElse(0)`. South's tea now answers `("green tea", 250)`.

- cats: team B's `OptionT` is converted by hand into an `Option` value in
  team A's `EitherT[List]` (`fromB`); the union `OptionT` stack and
  `fromA` are gone. The refused one-expression `for` now starts with the
  price, and its recaptured refusal is pinned: `Found:
  OptionT[Checked, (String, Int)]`, `Required: EitherT[List, AA, D]`.
- layered reflection: two `reify` blocks (List, Either); the fee's
  `Option` is a value, not a layer.
- effects: price, then fee, `getOrElse(0)`; the paragraph on the order
  of lookups is gone, since all three now look the price up first.
- All three asserted equal on the new answer; TestBookTwoMonadsCats 14.
