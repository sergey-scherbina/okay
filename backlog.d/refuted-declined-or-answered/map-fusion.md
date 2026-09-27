- map-fusion — MEASURED AND REFUTED 2026-09-27 (specs/map-fusion.md):
  fusing `map` into the next bind in Free. The direct form gave 1.17-1.23x
  but was stack-unsafe (Delim's composed continuations chained 20 000
  direct calls). The trampolined form was 21% slower on nestedSW. The
  2.27x step gap stays real: write a hot step as one flatMap.
