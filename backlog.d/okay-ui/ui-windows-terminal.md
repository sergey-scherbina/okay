- [ ] ui-windows-terminal — raw mode beyond stty. WRITTEN OUT
      2026-09-18, because a one-line stub told the next reader
      nothing, and because ui-terminal-keys made the remaining job
      smaller than it looks.
      WHAT BREAKS: `Terminal.raw` shells out to `stty raw -echo`, and
      Windows has no stty outside a POSIX shell. Nothing else in the
      host is POSIX — painting is ANSI, which Windows 10+ draws once
      `ENABLE_VIRTUAL_TERMINAL_PROCESSING` is on.
      WHAT NO LONGER NEEDS DOING: the KEYS. With
      `ENABLE_VIRTUAL_TERMINAL_INPUT` set, a Windows console sends the
      same `ESC [ A` sequences a POSIX terminal does, and
      `Frame.feed` already decodes those — so this is now only about
      the MODE, not about a second input vocabulary.
      THE SHAPE, dependency-free (okay-ui takes none): the same trick
      `stty` is — a CHILD PROCESS configuring the console its parent
      shares. `powershell -c` with an `Add-Type` P/Invoke of
      `GetStdHandle`/`GetConsoleMode`/`SetConsoleMode` does it without
      JNA and without a native image step; the bracket shape of
      `Terminal.raw` (on, run, off) is unchanged.
      TRIGGER, and it is a hard one: a Windows box to VERIFY on. This
      is platform code whose whole content is a side effect on a
      terminal nobody here has — landing it unverified would put a
      claim in the repository that no test and no person has ever
      seen hold, which is worse than the honest gap.
