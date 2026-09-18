- [x] optics-typepedia — DONE 2026-09-18. The reference nobody could
      grep: `docs/typepedia.md` is 911 lines of "every core type and
      typeclass" and contained neither `Lens` nor `Optic`. It has an
      Optics section now — the constraint lattice, the seven families,
      the five interpretations, the two roads through `Fuse`, the two
      field constructors, and the gotchas that were only in source
      comments (`.compiled` is slower and why; a traversal cannot be
      compiled at all; `Aggregating` is deliberately not `Strong`;
      `idApplicative`/`zipLazy` are not givens; a bottom-up rewrite is
      a catamorphism, not a traversal). Also: docs/optics.md gained
      the program-zooming section, the tutorial's "where to go next"
      links the page, and both theory indexes carry the sharper
      chapter-10 result instead of the old one-line summary.
