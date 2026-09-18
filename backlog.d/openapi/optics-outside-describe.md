- [x] optics-outside-describe — CLOSED 2026-09-11. The consumer
      arrived and was not built by this arc: a sibling wrote
      `okay-openapi`, quoting this entry's own rule back at it. The
      description then had to be made worth reading — see
      openapi-queries above — and the shape of that work is the
      lesson: the renderer had ALL the names it needed and none of the
      kinds, because the router flattened `Routed` to a String at its
      door. A DESCRIBE interpreter is only as good as what survives
      the boundary it is read across.
      What remains undeclared is HEADERS, and that is a route
      declaration, not a renderer feature: a handler reads a header
      off the `Request` with nothing declared anywhere. Not started —
      no consumer has asked.
