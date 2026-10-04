## state-foreign-shape — why `push` as one boundary read stateForeign 1.31x: C2 inlined the benchmark into the loop

After the operator's "Продолжай со stateForeign".

- Reproduced in one build behind a property (`push` vs `dollar(p)(pure)`):
  40.5-41.9 us against 32.1-32.4, per fork.
- `-XX:+LogCompilation` (the PrintInlining stream interleaves across compiler
  threads and lies): with `push` the continuation call `f(a)` in `Run.go` sees
  only the benchmark's two lambdas, so C2 inlines them — with boxing and
  State's constructors — into the loop (3832 B of code against 2568);
  `dollar`'s `ret` frame passes the same site and keeps it megamorphic.
  `dontinline` on the benchmark's methods: 33.4 us. An artifact of a program
  with two continuation lambdas, not the machine's work (history.d,
  backlog delimited-simplify-costs (1) answered).
- `scripts/hsdis-link.sh`: the HotSpot disassembler kept in every sdkman JDK
  (one copy in `~/Library/hsdis`), `--check` lists any JDK without it.
