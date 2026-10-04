- cont-stack-exact-first — DONE 2026-10-04 (specs/cont-stack.md, road 3).
  Where the stack can be read (JDK 22+, native access), the end of every
  room reads it and grants more of the same stack. The count is the
  fallback, and `-Dokay.cont.read=false` turns reading off. Cost: 1.04x
  at 1 000 levels, 1.03x at 100 000, statePara 1.00x. The 2.31x at a
  million levels is backlog cont-stack-exact-million.
