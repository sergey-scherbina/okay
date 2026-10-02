## darkworlds-oracle-bulk - the Dark Worlds oracle through a Bulk too

Operator: "def sky(n): Vector[Galaxy] — make it a Bulk or a stream". The
grid oracle now reads each sky through the Bulk in scope as well
(`DarkWorlds.sky[D]`), keeping its independence from the model by its own
parser — lines split by hand, columns by position — where the model takes
`Bulk.csv`'s rows by name. Every sum over galaxies is one aggregate whose
accumulator holds a sum per point (`logLikAt`): the coarse start search
and the oracle's 129 600-point fine grid alike. No Vector of galaxies is
left; the grid's numbers are unchanged to the last digit.
