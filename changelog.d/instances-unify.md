## instances-unify - one instance mechanism, not two; Distinct names Lexical; one decision table

The operator asked whether the protections against mixing repeated
effects are still needed, or should be done differently. They are
still needed: `Distinct` guards the row, which stays the default path.
But one of this session's additions duplicated an older mechanism.

- `Lexical.walk` now runs on `Instances`. Its own `Local.Op(owner,
  op: Any)` signature was `Instances[F, A](handle, op)` again, but untyped
  (it needed a cast) and unknown to `Distinct`. `walk` performs typed
  `Instances(handle, e)`, and the existing `Instances.exhausted` closes
  it. `Local`, the cast, `runLocal` and `LocalEscaped` are removed. Same
  bytes (286 272 against 286 192) and 1.44x the row's time.
- `Instances.exhausted` throws a named `Instances.Survived`, whose
  message covers both roads (`only` never stripped, or a walk instance
  used outside its installation or inside a delimiter's body).
- `Distinct`'s refusal now names `Lexical` next to `Tag`, `Instances` and
  `Refs`: an instance can also be taken OFF the row.
- docs/many-instances.md: the "Choosing" table is the one place the
  choice is made, with a row for every route and strategy, what each puts
  in the row, and the measured cost.
