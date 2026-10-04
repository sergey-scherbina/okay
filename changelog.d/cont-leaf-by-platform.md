## cont-leaf-by-platform: measured — on the JVM the strict leaf is 4x the lazy one at depth

- `ContDepthBenchmark` (new): contAnswer's body at 1e3/1e5/1e6 levels. Strict against lazy: 0.85x, 0.29x,
  0.25x (28.8 ms against 115 ms at a million). The strict leaf is linear, a StackSwitch included; the lazy
  one grows from 28 to 115 ns a level with linear bytes (history.d cont-leaf-depth).
- Not acted on yet: choosing the leaf per platform, with Native and the thread-switch caveat to settle
  first (sprint item).
