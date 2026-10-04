## cont-leaf-native: per-platform leaf declined — Native does not share the JVM's win, and the library gains nothing

- `ProbeContDepthNative` (runs only with OKAY_PROBE_CONT_DEPTH, built releaseFast): strict against lazy on
  Native is 1.25x at 1 000 levels, 0.77–0.89x at 100 000 and 0.74–0.96x at 1 000 000. On the JVM the same
  body reads 0.85x, 0.29x and 0.25x (history.d cont-leaf-native).
- In main code, none of the 10 answer-using `Cont` bodies gets the lazy leaf: each calls `k` inside a lambda
  or a by-name argument, so it is strict already. Choosing the leaf by platform would only change user
  code. Declined; the reasons are in backlog refuted-declined-or-answered/cont-leaf-by-platform.
