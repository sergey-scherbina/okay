## okay-bayes-ch5 - Bayesian Methods for Hackers ch.5: loss functions and the Bayes action

okay-bayes stage 5b (specs/okay-bayes.md): `Decision.action(draws, lo,
hi)(loss)` — the decision of least posterior expected loss, by a grid and
golden-section refinement — with `Decision.expectedLoss` and
`Loss.squared / absolute / pinball(τ)`. TestDecision: the three losses'
actions equal the mean, median and τ-quantile of the same draws; The Price
is Right on the book's numbers against its closed-form posterior (19 876 ±
3 767 against 19 899 ± 3 712); the showdown's best bid falls from 14 549 to
10 703 as the risk of overbidding grows from 30 000 to 150 000.
