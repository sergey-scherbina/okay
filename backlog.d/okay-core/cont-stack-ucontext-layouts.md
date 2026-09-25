- [ ] cont-stack-ucontext-layouts — specs/cont-stack.md Open question
      1, after cont-stack-switch lands with the macOS arm64 reader
      (`sp` at offset 264 into the `mcontext`, measured 2026-09-25): the
      three `(os, arch)` layouts the `jdk22/StackRoom` FFM reader does
      not know yet — macOS x86_64, glibc x86_64 (`uc_mcontext.gregs
      [REG_RSP]`), glibc aarch64 (`uc_mcontext.sp`). Each is ONE
      constant and one probe run ON THAT OS (the scratchpad probe
      `StackProbe.java` of 2026-09-25: read `sp` at depth 0 and at
      1000 frames of a trivial method, expect ~112 B a frame and the
      pthread bounds to contain both); until a layout is measured that
      platform answers −1 and counts. Also here, only if an exhaustion
      read ever shows in a profile: `_setjmp` as a cheaper pointer
      read than `getcontext` (326 ns, a `sigprocmask` syscall inside)
      — it stores `sp` at a fixed slot on macOS arm64 with no syscall;
      glibc mangles the slot, so macOS-only. And musl (Alpine) has no
      `getcontext` at all: the missing symbol must fall through to the
      count silently — a test with the symbol lookup stubbed absent.
