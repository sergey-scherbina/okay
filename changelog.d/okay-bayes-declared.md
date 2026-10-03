## okay-bayes-declared - models whose structure is known before they run

okay-bayes stage 8 (specs/okay-bayes.md; operator, after the typeclass
survey: "do models already have all the typeclasses?" — formally yes,
through `Free`'s Monad, but derived from flatMap). `Declared`: the
parameters as the core's free Selective, `Static[Model, P]` (`sample`,
`pure`, `both`, `all`, `sampleN`, branches by `ifS`/`select`), the
likelihood a pure `P => Double`. `Declared.sites` lists every site, with
its distribution and whether it sits under a branch, without drawing;
`Declared.model` is the ordinary model every sampler takes;
`Declared.nuts` refuses a discrete site, a site under a branch or a
repeated name before sampling. TestDeclared (JVM, Scala.js, Native): sites
listed past a distribution that throws when drawn; the TestBayes coin
declared with `ifS`, P(coin) 0.4316 (exact 0.4295); eight schools
non-centered by `Declared.nuts` against the exact posterior.
