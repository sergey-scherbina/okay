- [x] route-arity-one-tuple — DONE 2026-09-11. The entry asked for a
      second sighting; there were four, three by authors other than
      the one who wrote the entry: the sibling who wrote `TestOpenApi`
      reached for `t.head`, `TestRouterOut` did the same, okay-demo's
      `/events/{email}` carried a comment saying `.head` is "the
      spelling until it has a better one", and I wrote `Tuple1(id)` to
      build a url. That settled it.
      Neither remedy on paper was taken. An `on1` overload doubles the
      surface, and an `Extract[A]` MATCH TYPE says what the parameter
      is and leaves the router to cast into it — which AGENTS.md
      forbids. `Route.Arity[A] { type Out }` is a witness that CARRIES
      the conversion, the same reason `Split` is a witness and not
      `Tuple.Concat`. It collapses at the HANDLER only: `unapply` and
      `url` still speak in tuples, because the optic's laws are stated
      over `A`.
      Measured: 8 signatures on `Router`, 4 on its companion, and the
      whole repository needed FOUR call-site edits. Arity 2 still
      untuples as `(id, slug) => ...` and arity 0 is still `_ => ...`.
