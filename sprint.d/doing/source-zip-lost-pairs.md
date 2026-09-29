- [ ] source-zip-lost-pairs — PRIORITY: HIGH (a wrong answer, not a
      hang). The ci-runner's whole build of 2026-09-29 18:02Z (range
      3deb319f..bceceb8c, gate log okay-gate.rVkvz3QjRL) failed
      `TestSourceZip` "each side keeps its own order": 1600 pairs of
      2000, in step, no error — the zip ENDED EARLY and dropped the
      rest silently. Green alone, so the runner recorded it in
      .work/ci/flakes and pushed; it is not a flake, a zip that ends
      before both sides do is lost data. Suspect first: the zip's
      cancel scope released mid-run (`Merge.closing` closes both
      channels, each drains what it buffered and answers the end).
      THE LANE: reproduce (the law in a loop, three schedulers,
      `mergeReleases` read per round, then under load), name the
      path, red first, fix, the loop green. (2026-09-29)
