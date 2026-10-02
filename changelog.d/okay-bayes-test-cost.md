## okay-bayes-test-cost - TestDecision's showdown under munit's 30 s

The ci-runner's whole build went red on `okay.bayes.TestDecision` ("the
showdown loss"): a timeout, not a wrong answer — 34–44 s against munit's
30 s on a box at load 11–32, reproduced alone. The test priced five risks
over all 40 000 posterior draws on a 400-point grid, about 10^8 loss
evaluations: 0.9 s on the JVM, 2.6 s on Scala.js, 10 s on Native when
quiet. It now prices them over every tenth draw on a 200-point grid: 0.07
/ 0.15 / 0.6 s, the bids still falling with the risk, the same on all
three platforms. The runner's bisect blamed shift-merge; the red was this
lane's. `TestBandit`'s regret test, in the same red list, passed alone
(0.1–0.5 s quiet) and is unchanged.
