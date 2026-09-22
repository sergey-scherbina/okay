- [ ] dottydoc-matcherror-under-publishlocal — RECURRENCE LEDGER. A full
      `scripts/gate.sh publishLocal` failed once in `okayJVM / Compile / doc`
      with `scala.MatchError: val <none> (of class
      dotty.tools.dotc.core.Symbols$NoSymbol$)` from
      `xsbt.DottydocRunner.run` (2026-09-23, pom-jmh-and-chat-version, a
      cold worktree, 124 modules documented in parallel). Rerun alone,
      `okayJVM/clean; okayJVM/doc` passed. Two further full `publishLocal`
      runs the same hour were green. So it is not reproducible yet; the
      transient dotty crashes in memory (`dotty-classfile-crash-transient`)
      are the same family. The next sighting should record whether the
      run was cold and whether another doc task for the same sources
      (JS/Native) was running beside it.
