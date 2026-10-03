- handle-overloads-chain — ANSWERED 2026-10-03: keep the overloads
  (cont-list-combinators-one-walk's lane, by survey). Free.scala's
  `p.handle(h1, h2)` and `p.handle(h1, h2, h3)` are documented as
  exactly `p.handle(h1).handle(h2)…`, and in okay only the test that
  pins that equation calls them (TestHandler.scala:43-47); every
  other site chains. But they are PUBLIC API, and the okay2 twin
  has the same forms, uses them (okay2 TestHandler.scala:41, 42,
  85, 89, 118) and documents them (docs/okay2.md:359). Removing
  them from okay saves ~20 lines of evidence plumbing and costs a
  breaking change plus a divergence between the twins, which this
  repository keeps parallel on purpose. Reopen only if okay2 drops
  its forms too, or if the overloads ever get in inference's way.
