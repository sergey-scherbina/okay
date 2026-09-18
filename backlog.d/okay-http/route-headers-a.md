- [x] route-headers-a — DONE 2026-09-11. A request header is a
      `Named[T]` in a third place: `:@`, the same spellings
      (`as`/`opt`/`all`), rendered `in: header`. The design fact worth
      keeping: a header CANNOT join the route's `A` without breaking
      `unapply(url(a)) == Some(a)`, so `Routed[A]` stays a prism on the
      url and `Headed[A, H]` is the request-shaped declaration. One
      builder serves both query and header because a header block IS a
      `Map[String, Vector[String]]`.
