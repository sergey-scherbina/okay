## refine-laws - any pattern's laws, checked in one line

`RefineLaws.check(pattern, inputs, values, expect)` and `checkNamed`
answer a `Report`: read→write→read to the same value, write→read for
sample values, nothing throws, determinism; `Expect.Corpus` also reports
every input not taken (declined, unclear). Framework-free main code, so
okay-fin's and okay-insure's patterns can be held to the laws by their own
tests. TestRefineLaws: a lawful pattern and ISDA's two FpML examples pass;
a lossy write, a refused write, throws, a drifting read and not-taken
inputs are each found. In okay and okay2.
