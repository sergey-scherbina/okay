## cont-stack-ucontext-layouts - the exact stack reader on Linux (glibc aarch64 and x86_64)

The JVM's exact road (JDK 22+ with `--enable-native-access`) read the
stack only on macOS arm64; every Linux server counted. It now reads on
glibc aarch64 and x86_64 too (specs/cont-stack.md Decision 13).

- Measured, not taken from a header: `scripts/ucontext-probe.java`
  scans the whole `ucontext_t` for the words inside the thread's bounds
  that fall as the stack deepens. glibc aarch64: `sp` at 432 (native,
  Docker linux/arm64). glibc x86_64: `sp` at 160 = `gregs[REG_RSP]`
  (Docker linux/amd64, under emulation). The frame pointer moved with
  it on both.
- A glibc layout was more than the one constant Open question 1
  expected: `mcontext` is inline, not behind a pointer; the bounds are
  `pthread_getattr_np` + `pthread_attr_getstack` with the guard off the
  bottom, as HotSpot computes them; and the `ucontext_t` buffer is 8 KB,
  because glibc aarch64's struct is 4560 bytes and `getcontext` wrote to
  byte 1004 of the old 1024.
- The real suites, from the packaged Multi-Release jar, in Docker:
  TestStackRoom and TestContStack 11 of 11 on glibc aarch64, glibc
  x86_64 and musl (which has no `getcontext`, answers −1 and counts).
  The reader's floor lies above the real StackOverflowError on all
  three platforms (3–4 KB on glibc, 256 KB on macOS).
- `StackRoom.readableWithout(symbol)` builds the handles with a symbol
  hidden, so the musl fall-through is a test on every machine.
- Left: macOS x86_64 and a native x86_64 run —
  backlog `cont-stack-ucontext-x86-native`.

Commits: 8e3a73b61 (spec), 80ab4ff79 (reader, tests, probe, results).
