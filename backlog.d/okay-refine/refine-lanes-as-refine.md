- [ ] refine-lanes-as-refine — operator 2026-10-01 (asked whether lanes
      can be one design with refine). A lane IS a prism into the tagged
      sum of all lanes (`lane(x)` its review, `take` its preview), so
      routing is the LAST refine level: document → instrument → lane, one
      `Refine[A, Lane-sum]`, `split` over any `Routable`. Folds `Routes`,
      `Dispatch` and `Router` into one mechanism with two syntaxes (a
      `match` table for exhaustiveness, `orElse` of lane patterns for
      verdicts that explain a route, `or` to find overlapping rules), and
      RefineLaws then checks routing tables too. A lane routes, it does not
      transform: projections move to the output (`out(lane).map(f)`).
      The existing routing tests are the oracle for the rewrite.
