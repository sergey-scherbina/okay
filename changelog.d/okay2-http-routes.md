## okay2-http-routes - okay-http's typed routes for okay2

Part B of okay2-http (spec stage 44, docs §35). `Route`, `Queried`,
`Headed`, `Query`, the terse `syntax`, the verb extractors, `Router`
(both route shapes) with its trie index and `enforcing`, `Urls` and
`Acceptance` all run on JVM, Scala.js and Scala Native. `JsonSchema` is
ported into okay2-codec because the router uses it to describe bodies.

Scala 3 types a route's captures as a tuple. In okay2 a route captures
its OUTSIDE form: `Unit`, one value, or a tuple of up to 8. That form is
what the handler, `url` and `unapply` use. `Captures`, `Tupled` and
`Concat` give each form a list shape the compiler builds by induction,
and `Split` joins two captures through them. `route.of[C]` is a blackbox
macro: it checks the case class's field types against the capture at
compile time and the names at construction.

Tests:
- TestRoute, TestArity (the join/split law), TestRouteHeaders,
  TestRouteSecured, TestRouterIndex (the trie agrees with a first-match
  scan on generated tables), TestRouteBody, TestRouterOut,
  TestRouterMedia and TestRouterSummary: 107 results on each platform,
  all pure;
- TestJsonSchema;
- a `Live` Acceptance run through the JDK `Server` and `Transports`.

Found along the way:
- `implicitly` drops the `Out` refinement in Scala 2, so `Split.apply`
  keeps it.
- A handler over several captures is written `{ case (a, b) => }`.
- The list cons is `:*:`, so it does not shadow `List`'s `::`.

Not ported:
- TestRouteFacts, which needs okay-di;
- the Scala.js transports, filed as `okay2-http-js-transports`.
