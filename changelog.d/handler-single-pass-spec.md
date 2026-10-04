## handler-single-pass-spec: the design written — `handle` registers, `run` walks once

- specs/handler-single-pass.md (operator's design, 2026-10-04). `handle(h)` pushes `h` onto the stack of a
  handled node. One walk, at the end, matches each operation against the whole stack, innermost first. The
  handlers meet the machine through one abstraction, the step (`init`, `step`, `ret`), which the built-ins
  already have (handler-one-step). The spec covers the control boundary (handlers that need `k` split the
  stack), the dispatch rule (a handler's own operations go only outward), the laws by the equivalence oracle,
  and five stages starting with a re-measure.
