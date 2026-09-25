- [ ] okay2-http-routes — okay-http's typed routes on okay2 (spec stage
      44, part B): `Route`, `Query`, `Headed`, `Router` (on/at/json/out/
      html/bytes/events/media, `routes`, `describe`, `markdown`,
      `enforcing`, the trie index), `Urls`, `Acceptance`, and the Scala.js
      client transports. The Scala 3 core types a route's captures as a
      TUPLE and joins them with `Route.Split`/`Route.Arity`; in Scala 2
      that is an HList-shaped encoding or nested pairs with a match-free
      split instance. Settle that first, with the arity laws as tests
      (TestArity). Part A (the transports) landed. (2026-09-25)
