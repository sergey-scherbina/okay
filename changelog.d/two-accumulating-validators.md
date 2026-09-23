## two-accumulating-validators - `Validate.validated`: a schema walk as a `Validated`

The bridge the backlog asked for, and nothing more: `Validate.gather`
stays the hand-written `Either` accumulator (the schema hot path, with
paths), and `Validate.validated(schema)(json)` reads its answer as the
core's `Validated[Validate.Errors, A]` — one `fromEither`. `Errors` is
a `Vector`, so the `Semigroup` is concatenation and the instance
resolves from the companions with no import. TestValidate: equal to
`decode` as an `Either`; two walks under one `app` keep both sides'
errors with their paths, in order; two good walks are a `Valid` pair.
Comments in `Validate.scala` and `Validated.scala` now point at the
bridge instead of at the backlog; specs/validated.md box and result;
docs/guide.md has the example.
