- [ ] okay-refine — the FOUNDATION for reading anything out of anything
      (operator ask, 2026-09-29): a typed hierarchy of PATTERNS that
      recognise a document step by step — bytes → format → document →
      instrument → …, every level open. The primitive is a `Refine[A, B]`:
      a PRISM whose `read` may decline (Choose: this branch dies, the
      search goes on, `pattern-binds`), whose `write` is total, so
      composition along a path reads AND writes back, and a `<|>` of
      alternatives is the sum. A result is a `Verdict` with its
      SUPPORT (which patterns took it, which declined and where, the
      runner-up), `Unclear(candidates)` when two branches took it —
      never "the first won" silently. The domain (FpML, ISDA CDM) is
      NOT here: it is a private repository's, and this module ships one
      public prover only. Stage 1 (this lane): `specs/refine.md`,
      module okay-refine (cross, on okay-codec + okay-optics + core
      Logic), `Refine`, `Verdict`, the FORMAT level — cbor / json / xml /
      yaml over the existing dialects, a format detected with its
      reason, an unknown one declined with what was tried; laws:
      write-then-read is identity, read-then-write is the input where
      the read was lossless. Stage 2: the DOCUMENT level over a
      `Schema` (a `Refine[Json, A]` from any derived Schema), the open
      registry, the `Judge` seam (a DLM-shaped ordering hint over the
      alternatives, never a source of structure). Stage 3: lessons as a
      fold over a journal (order of trial, not structure). Additive lane.
      Spec: specs/refine.md. (2026-09-29, operator ask)
