- [ ] zip-lost-pairs — TestSourceZip "each side keeps its own order"
      obtained 1600 pairs of 2000 in ci-runner's whole build
      (2026-09-29T18:02, range 3deb319f..bceceb8c); passed alone on
      re-run. A stream ending early is LOST DATA, not a flake. The range
      holds today's scheduler lanes (forkLong 851d59dd5, the slice-hook
      handshake f88aeca89, resumeLate f50932cbe). Reproduce in a loop,
      find the revision, fix with a law. (2026-09-29)
