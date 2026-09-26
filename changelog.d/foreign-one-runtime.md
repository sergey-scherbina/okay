## foreign-one-runtime — one body per capability over a `Language` (2026-09-26)

Stage 4 of specs/foreign-one.md, narrowed by Decision 16. After one engine,
value tree, protocol and pool, what differed between Python and R above the
engine was four facts — the pool, the address separator, the value rules,
the interpreter's name — so `Language[M]` holds them and every cluster stage
kind (`ForeignStage`, `ForeignReducer`, `ForeignStreamer`, `ForeignModel`)
and every facade capability (`Calls`, `Frames`, `Programs`, `Holds`,
`Speaks`) is one body over it. The Python/R twins are deleted
(okay-foreign-cluster +330/−480); the givens and `py(path)`/`r(path)`
factories stay, so no caller changed. The facade's R table road converted
twice and now takes Python's (the R half of facade-frame-seam). Live:
cluster 30, workflow 12. Mutant: R addressed with `:` fails 8 R tests. Filed
rather than built: foreign-more-languages, foreign-jvm-programs,
foreign-package-name.
