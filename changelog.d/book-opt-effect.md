## book-opt-effect - chapter 16b: the delivery example's "absent" is the user's own `Opt` effect

The algebraic-effects version of team B's `deliveryFee` used the core's
`Stop` (the end of a generator) for "no delivery". It now declares its
own effect, `enum Opt[+A] derives okay.Effect` with one operation
(`absent`) and a handler (`runOpt`) that turns it into `None` — the name
says what it means, and the example shows an effect written from
scratch. The row is `Choose + Opt + Throws % String`; the three versions
still agree (TestBookTwoMonadsCats 13).
