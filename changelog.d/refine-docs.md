## refine-docs - okay-refine's page catches up with the arc

- docs/modules/okay-refine.md: "Writing a document level" (a pattern per
  document element named after it, the smallest value, `req`/`opt`, the
  registry as `Refine.first`, disjoint alternatives, events beside
  instruments), "The corpus method" (vendor the standard's own examples,
  one test that prints the table and asserts declined == 0 — 801 FpML
  documents went 123 → 801 in a day and found two dialect defects), "The
  cross-format law" (one value from two formats), "What lives where";
  two gotchas (strict XML; `field` on a repeated element).

- Re-landed 2026-09-29 after ci-runner reverted it (63c5ffe87) on a
  TestFleet timing flake bisected to a docs-only commit; see backlog
  `ci-runner-docs-only-culprit`.
