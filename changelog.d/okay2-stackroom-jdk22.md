## okay2-stackroom-jdk22 - okay2 reads its own stack on JDK 22+: the FFM reader as a Multi-Release variant

The last open part of cont-stack-okay2-macro. Operator: "Делай то что
нужно для okay2 только сразу всё" (do what okay2 needs, all of it at
once).

**What it does.** okay2's `StackRoom` answered -1 on every JDK, so
`StackSwitch.more` never granted levels and okay2 always counted. Its
JDK 22+ twin, ported from the Scala 3 core's `jdk22/StackRoom.scala`,
now reads two things through FFM:
- the stack pointer, from `getcontext` and the measured `ucontext`
  layouts (macOS aarch64, glibc Linux aarch64 and x86_64);
- the bounds, from the pthread stack attributes, minus HotSpot's guard
  zones.

It answers -1 wherever the layout is unknown, a symbol is missing (musl)
or native access is off.

**Build.** okay2/build.sbt gains the root build's `versioned` and
`multiRelease`:
- `okay2Jdk22` compiles `okay2/jdk22` with `-release 22`;
- okay2's jar carries it under `META-INF/versions/22/` with
  `Multi-Release: true`;
- the forked tests run against that jar, with
  `--enable-native-access=ALL-UNNAMED`.

The root-path `StackRoom` gained `readable` and `readableWithout`, so
both classes have one public shape. Scala 2.13 handles `invokeExact`'s
signature polymorphism from the result ascription, as Scala 3 does.

**Tests.** TestStackRoom has three tests. On this JDK and layout it reads
a pointer between the bounds, 200 frames deeper reads lower, and a hidden
symbol declines. The class loaded is the jar's, so the JVM made the
version choice. TestContStack and TestContMacro are unchanged and green.

**cont-stack-okay2-macro is closed:** Layer 1 A, Layer 1 B and this reader.
