- [x] optics-outside-route-of-labels — DONE (2026-09-11, a450ff85). The
      operator settled the open question ("it will be needed"), and
      the entry's own doubt was half wrong: the check catches more
      than a documentation typo, since `describe` publishes those
      names to OpenAPI and to MCP tool schemas. The cost the entry
      worried about is real and stands — a field deliberately named
      differently from the url is now refused, and must be renamed on
      one side. `of[C]` is inline, reads `MirroredElemLabels`, and
      compares path parameters then query parameters against the
      fields; the refusal was verified by removing it and watching
      both tests fail.
