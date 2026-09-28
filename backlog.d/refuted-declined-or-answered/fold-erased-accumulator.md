- fold-erased-accumulator — MEASURED 2026-09-28 (operator ask), NOT
  landed (specs/map-fusion.md "The inline road, one rung each"). The
  road — `inline def foldEach`/`foldM` with `inline` step functions,
  the local `go` expanded at the call site with the caller's `B` —
  reads exactly its ceiling on `foldEach`: `stateFoldEachInline` 12.27
  µs against the library's 15.16 (**1.24x**, −24 B a step: the 16 B
  box and 8 B off the closure), 1.27x still over the hand loop. On
  `foldM` it reads 1.11x (16.81 → 15.15) at the SAME −24 B a step: the
  `Bind` + `Mapped` a `.map`-written step builds are NOT
  scalar-replaced even when built and matched inside one expanded `go`
  — the escape-analysis hypothesis for the map node is REFUTED by the
  bytes. The price of the road is unchanged (a copy of `go` per call
  site, inline budget on every caller of a library fold) and 1.24x on
  a primitive fold did not buy it. RETAKE only on the original trigger:
  a consumer whose hot fold carries a primitive accumulator and shows
  it in a profile — then `inline` that ONE consumer's fold, or land
  `foldEach` as `inline` beside a non-inline twin. The map node's
  answer stays `foldEach` (free-map-node).
