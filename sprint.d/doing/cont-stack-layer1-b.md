- [ ] cont-stack-layer1-b — specs/cont-stack.md stage 4, after
      cont-stack-switch lands: the rest of Layer 1. (B) answer-using
      bodies (`k(1) + k(10)`, `a :: k(x)`, interpolation) CPS-transformed
      selectively as okay-direct does for `Free`, an explicit
      type-aligned stack of pending body parts (the `Wrap`/`Args`
      technique of stage 1a), multi-shot intact, one allocation per call
      of `k`; 1M such shifts on a 128 KB stack with zero switches.
      Then the KNOWN higher-order functions (`map`/`foreach`/`flatMap`/
      `fold` on the standard collections, `Option`, `Either`) as
      trampolined traversals; VISIBLE user functions (an `inline def`,
      a same-compilation `def` through `Symbol.tree`, TASTy with
      `-Yretain-trees`) rewritten along the path `k` flows, cached; and
      `direct { !k(…) }` inside a body through okay-direct's machinery.
      Opaque bodies stay correct through Layer 2 — a test per shape.
      Literature: Rompf, Maier & Odersky, ICFP 2009 (the selective CPS
      transform and its wall at code it cannot read).
