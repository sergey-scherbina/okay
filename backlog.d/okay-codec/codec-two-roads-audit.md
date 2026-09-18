- [ ] codec-two-roads-audit — the json-parse-fast-road shape as a
      QUESTION rather than a fix: a module had two roads to the same
      value, 37x apart, and the DEFAULT was the slow one for long
      enough that a separate feature (py-arrow) got filed to work
      around the symptom. Where else does this repository have a fast
      path that nothing takes by default? Named suspects: the CBOR
      pair beside `Json`/`JsonValue`, and the staging seam's
      interpreter-vs-installed choice (`Codecs.current`), which is a
      runtime switch rather than a road but has the same failure mode
      — measured once, then assumed. Cheap to check, and the last
      check of this kind was worth 37x.
