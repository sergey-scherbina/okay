- [ ] cont-stack-ucontext-x86-native — what cont-stack-ucontext-layouts
      (2026-09-26, specs/cont-stack.md Decision 13) left: (1) macOS
      x86_64, the one layout never measured — it answers −1 and counts;
      needs an x86_64 macOS JDK (Rosetta) and the macOS bounds, then
      `scripts/ucontext-probe.java`'s scan behind the `uc_mcontext`
      POINTER (macOS keeps it at 48, not inline). (2) glibc x86_64 was
      measured under EMULATION (Docker linux/amd64 on Apple silicon): sp
      at 160 is the libc's layout and should carry, but one native
      x86_64 run of `scripts/ucontext-probe.java` and TestStackRoom is
      owed. (3) Only if an exhaustion read ever shows in a profile:
      `_setjmp` as a cheaper pointer read than `getcontext` (326 ns, a
      `sigprocmask` syscall inside) — macOS-only, glibc mangles the slot.
      PRIORITY: LOW (a trigger: a user on x86 macOS, or an x86 Linux box
      in reach).
