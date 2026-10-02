# okay-cats-cross — okay-cats on JVM, Scala.js and Scala Native

Status: done, 2026-10-02. Owner lane: `okay-cats-cross`. From the
cats-depth audit (backlog okay-cats).

## Decisions

- **A crossProject with ONE source set.** Nothing in the bridges is
  JVM-only except the doors that park (`toIO` = `IO.blocking(runWith)`,
  `asIO`, `scheduler`). Those now ask `Answers[Async]` — the evidence
  `runWith` needs, given on JVM and Native, absent on JS — so on JS they
  are a compile error at the call (okay's rule: a platform contributes
  evidence, not API) and no file had to move to a platform directory.
  Callers on the JVM bring it with `import okay.given`.
- **cats-effect 3.7.1, cats 2.13.0.** cats-effect publishes Scala Native
  0.5 artifacts from 3.7.0 on (3.5.7 has 0.4 only). CE3 keeps binary
  compatibility across minors: okay-fs2 (fs2 3.10.2, built on 3.5) and
  okay-kyo's tests, which see okay-cats in test scope, run green on it.
- **Tests:** `src/test/scala-cross` (laws, bridges, kernel, cats-effect
  laws) on all three platforms; `src/test/scala` on the JVM (they block:
  `unsafeRunSync`, latches, sleeps).

## Results

- JVM 323, Scala.js 252, Scala Native 252, green; okay-fs2 10 and
  okay-kyo 18 green on cats-effect 3.7.1. The cats-effect `AsyncTests`
  (110 properties) hold on JS and Native too.
- FOUND: the derived `tailRecM` through an EAGER carrier overflowed
  Scala.js ("a million through an eager okay monad"); measured, JS
  overflows between 300 and 1 000 iterations and a 128 KB JVM thread at
  1 000. The claims of effects-foldmap/monad-tailrecm are wrong; filed
  as sprint eager-carrier-depth, and that one test is JVM-only
  (TestToCatsDepth) until it lands.
