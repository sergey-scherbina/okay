## book-fee-option-value - chapter 16b: "no delivery" is a plain Option value, not an effect

The algebraic-effects version of team B's `deliveryFee` is now
`Option[Int] ! Throws % String`: the absent fee is data, the unknown
shop the one effect. The `Opt` effect (and its handler) is gone; the
program matches on the `Option` before pricing, so south answers
`Right(None)` as in the other two versions. Row `Choose + Throws %
String`; TestBookTwoMonadsCats 13, the three versions still agree.
