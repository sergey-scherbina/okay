- [ ] stack-bytes-warm-flake — okay-codec's `TestStackBytes` "both
      threshold lanes: every door is flat past the threshold" failed in a
      whole `affected` gate (2026-09-29, sentinel-end-placed-wakes):
      Cbor.read[Tree] wanted 256 KB at 100 levels and 16 at 400, so
      `assertEquals(deeper, deep)` failed. 256 KB is the COLD reading of
      that door; `needsWarm`'s 2000 rounds had not made it warm on a
      loaded box. Green alone (16/16/16). THE LANE: make "warm" a
      condition rather than a count (warm until two successive readings
      agree, bounded), or compare `deeper <= deep` with the message it
      already has. Priority: MEDIUM (a flake in the default gate).
