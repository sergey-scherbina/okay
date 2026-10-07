## okay-cont: a handler takes its effect off wherever it is in the row

Lane row-removed (specs/freer-min.md, stage 39). `Free.handle[E](h)(p)` on
a program over any row that has `E`: `Removed[E, R]`, built by the
compiler by position, names the row without `E` (`Out`) and puts the
handler's capability at `E`'s place among the rest's lifted one level in.
The order of effects in a type says nothing; the order of handlers says
everything. `widen` stays for a row with fewer effects or another order
by `Sub`.
