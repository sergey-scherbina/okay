- [x] wroclaw-table-refresh — DONE (2026-09-11): §20's table is one
      whole `scripts/wroclaw-bench.sh 8 3 1` at load 1.7-3.1, every
      engine, after okay's lanes changed underneath the old one. It
      also corrected a claim made from mixed runs: okay's one-core row
      is a TIE with `java.util.stream` (563 vs 561), not a win. And it
      measured the table's own noise floor — two rows that are the
      same code since wroclaw-flat-by-default read 9% apart.
