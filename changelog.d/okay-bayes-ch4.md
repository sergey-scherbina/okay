## okay-bayes-ch4 - Bayesian Methods for Hackers ch.4: ranking by evidence

okay-bayes stage 5a (specs/okay-bayes.md): `Distribution.incompleteBeta`
(Lentz's continued fraction), `Beta.cdf` and `Beta.quantile` (Newton
inside a bracket), and `Rank` — items ordered by the 5% quantile of their
Beta posterior (`lowerBound`, `sort`), with the book's normal approximation
beside it (`approxLowerBound`). TestRank on JVM, Scala.js and Native: the
cdf against closed forms, the quantile its inverse to 1e-10; the book's
bound measured against the exact one (optimistic by 0.054 at one vote,
3e-4 at 700); 999 of 1000 ranks first and one of one last; the spread of a
mean falls as 1/√n.
