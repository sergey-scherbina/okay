## book-effects-fold - chapter 16b: the effects delivery program maps the Option

`EffectsDelivery.order` no longer matches on the delivery fee: it binds
the variety and maps the `Option` in the `yield`
(`fee.map(f => (tea, price + f))`), the same shape as the cats and the
layered versions and three lines shorter. The chapter says the one
difference this makes (the varieties are looked up in a shop without
delivery too) and how to skip it (fold the fee). The three versions
still agree on the chapter's data (TestBookTwoMonadsCats).
