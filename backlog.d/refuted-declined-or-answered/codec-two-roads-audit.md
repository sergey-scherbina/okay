- codec-two-roads-audit — CHECKED 2026-09-19, neither named suspect
  reproduces json-parse-fast-road's shape. The CBOR pair is not
  abandoned: `Cbor.get` (fold), `Staged.scala` (compile-time) and
  `RuntimeStaged.scala` (run-time) meet in one shared helper
  (`Staged.cborProduct`; see [[three-cbor-decoders-one-helper]]) and
  every scoped test run exercises all three. `Codecs.current`
  defaulting to the interpreter is a stated, justified policy, not an
  oversight: only `okay-script` depends on `okay-staging` at all
  (build.sbt), and it calls `Staging.autoInstall()` at boot — every
  other module simply never carries the compiler on its classpath, so
  there is no silent "fast road nobody takes" to find, only doors that
  never asked for the fast road in the first place.
