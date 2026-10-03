## cont-strict-k-next-carrier - the Next carrier off the loop, measured and refuted; no code change

cont-strict-k, step 2 (operator: "Продолжай 2", i.e. continue with option
2). Recorded so that nobody tries it again blind.

**The idea.** A capture caught by a frame hands the machine's loop a
`Next` carrier: 24 B a capture, about 7% of statePara's CPU in its
constructor. Written as the loop's own inline arm (`foundHere`, tail
calls), it allocates nothing.

**Measured,** alternated against master, history.d
`cont-strict-k-next-carrier`:
- **A resumption's `Next` as an inline arm:** no change. C2 already
  scalar-replaces it.
- **The capture as the loop's own arm, both near roads:**
  - statePara **0.83x** (-48 KB);
  - contAnswer **1.16-1.19x**;
  - fib100 1.04-1.07x.
- **The direct-delimiter arm only:** the same picture, because
  contAnswer's captures take that arm too (contAnswer 1.20-1.21x, fib100
  1.10-1.11x).

**Why it fails.** The clause call `f(k)` moves into `loop$1`, and
contAnswer's clause is the macro's continuation, which C2 then inlines
into the loop. That is cont-frames-register-pressure, in its fourth
place. statePara's clause is small, so there it only gains.

The code is unchanged. backlog cont-strict-k keeps the open question: a
road with no `Next` that keeps the clause call out of the loop.
