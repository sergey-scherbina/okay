## dlm-explain-missing - an audit that could not say a lesson decided the turn

Found 2026-09-28 by okay-watch's check bot: `/why` on a sentence the
person's own LESSON routed to `check`, whose address slot was missing,
answered «layer: none, rule: none, lesson: none». `Route.Missing`
carries no `Support` — the route says a value is absent and what to ask
is the caller's — and `Explanation.of` read support out of `Fires`
alone. An explanation that cannot name the layer for a turn learning
decided is the one explanation learning most needs.

- `Explanation.of` takes the support of a `Missing` route from
  `noticed`, which already carries every layer's reading of every
  intent it saw. No type changed and no consumer's pattern match moved;
  `noticed` is computed once instead of twice.
- specs/dlm-learning.md gains the behaviour line; two tests — a Missing
  decided by a rule, and one decided by a lesson.
