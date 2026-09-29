- [ ] xml-processing-instruction — the XML dialect reads `<?xml
      version="1.0"?>` (and any `<?…?>`) as an OPEN tag: `Xml.kindOf`
      tests `<!--`, `<![CDATA[`, `</`, `/>` and then `<`, so a
      processing instruction opens a frame nobody closes and the tree
      ends with an `unclosed` error node. Found by okay-refine's format
      level (2026-09-29): every FpML document begins with the XML
      declaration, so `Format.detect` declines every real FpML file with
      "unclosed" — TestFormat pins it as "KNOWN GAP" so the fix flips the
      test. THE ASK: a `K.Pi` token kind (`<?` … `?>`, scanned like a
      comment: one token, never a frame; Channel.Syntax, since the
      declaration is not trivia), `kindOf` testing `<?` before `<`,
      lossless as everything else; the projection ignores it. A change
      to an existing scanner, so the full `affected master staged`.
      Related: [[okay-refine]] stage 2 needs it before the FpML prover.
      (2026-09-29, found by refine stage 1)
