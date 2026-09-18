- [x] tag-distinct-keys — DONE (2026-09-11) as `Distinct[R]`, and
      the entry's own plan did not survive the first measurement.
      "Collect the singleton keys and refuse duplicates" would refuse
      `Of["k", Ping] + Of["k", Peng]`, which tag-test-the-signature-too
      had made legal that same morning. Comparing ERASURES instead
      would refuse `Writer % String + Writer % Int`, which
      `TestRowIdentity` runs and which routes correctly. Neither the
      key nor the type decides it: the TEST does, so the instance
      declares it — `TypeableK.ByValue`, carried by `writerK` alone,
      with unmarked meaning "tests by erasure". That default is the
      safe direction: an unmarked fine instance is refused and fixed
      by one word, the reverse would pass a row that misroutes.
      The check is therefore not Tag-specific at all. It catches the
      UNTAGGED `Reader % Int + Reader % String` — the defect the whole
      Tag/Refs/Delim machinery exists to work around — which is what
      it was asked for and more.
      WHERE: `Handler.union`, in a SECOND using clause (`(using
      TypeableK, Handler, Handler)(using Distinct[F + G])`), not
      `RowLift.at`/`plus`. That is where `split` claims the excluded
      middle, it is 15 call sites against 147, and a second clause
      leaves the sites that pass the first one explicitly alone.
      MEASURED, both disqualifiers: zero breakage (whole tree
      compiles, gate GREEN at 4247 results, all 10 okay-agent union
      sites included) and no compile-time cost worth naming (full
      Test/compile 44 s; cold core 70+105 sources 34 s, agent 16+22
      6 s, zero warnings).
      THREE TRAPS, all in `Distinct.scala`'s comments: `TypeRepr.of[R]`
      for a higher-kinded parameter arrives as an HKTypeLambda, never
      as the applied `+` the call site wrote; a `Tag.Of[K, F][A]`
      member is an applied ALIAS, so `baseType(tagSym)` is the only
      thing that sees it; and `F | F` is `F`, so a row CANNOT repeat a
      member — the check only ever fires on two different types with
      one runtime identity.
