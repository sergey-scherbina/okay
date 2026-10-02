## okay-bayes-darkworlds - Dark Worlds on Kaggle's skies

okay-bayes 5d (specs/okay-bayes.md): the book's ch.5 data-heavy example,
Kaggle's "Observing Dark Worlds" — ten training skies and their TRUE halo
positions as test resources. TestDarkWorlds: Sky 3 (578 galaxies) by
`adaptive` and by AD NUTS against an exact 3-D grid posterior (x 2324.7 /
2325.1 against 2324.1), the true halo 1.04 posterior sds away; on ten skies
the truth inside the 95% region 10 of 10, median distance 39.
Found: the posterior has local modes and NUTS from a random start stays on
one (Sky 3 at x 3229, ESS 3; four skies of ten off by 2000+).
`Smooth.nuts(..., init = Map(name -> value))` starts the chains at a point,
by parameter name in its own units; from a coarse search's best point
NUTS is right on every sky.
