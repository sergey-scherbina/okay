- [ ] router-trie — `Router.routes` is first-match: `entries.iterator
      .map(answer).collectFirst`, one `matches` closure per entry per
      request (Route.scala). A trie by method and path segment built
      once at `Router` construction answers in O(segments) and keeps
      first-match order among the entries a leaf holds. NOT worth a lane
      today: every router in this repository declares 1–3 routes per
      file (grep `Router.on|at`), where a scan beats a trie. TRIGGER: a
      router with dozens of routes (okay-chat's, an admin surface) — not
      "a new number". NUMBER: requests/µs on that router, scan vs trie,
      with the route count in the row. Filed from the staging survey
      (2026-09-22, ranked 4 of 4).
