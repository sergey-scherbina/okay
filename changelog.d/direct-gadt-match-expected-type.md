## direct-gadt-match-expected-type - refuted: the `: X` on a tail GADT match stays, and why is pinned

The backlog asked the `direct` macro to ascribe the block's answer type
to a tail `match`, so that `(e match …): X` in TestDirectOnce could
lose its `: X`. It cannot be done that way, and the reason is now a test.

- The macro never sees the unascribed block: an inline method's
  arguments are TYPED BEFORE expansion, and the later arms have
  already failed (`Found: String, Required: Long`).
- Two signature variants of `DirectApply.apply` were tried to push the
  expected type in first — a leading `using Pin[A]` with `Pin`
  contravariant, and a prefix object whose `apply` is typed after `A`
  is fixed. Both fail exactly as before; reverted.
- The rule is the compiler's, reproduced with no macro and no library:
  `def mk[A](block: => A): CovBox[A]` with a covariant box, a GADT
  match in the block, and dotty 3.9 fixes the answer variable at the
  FIRST arm's singleton (`Required: (1L : Long)`); the same with an
  invariant box types every arm at `X`. That is why the covariance of
  `Free[F, +A]` (free-answer-variance) introduced the ascription.
- `TestDirectGadtTail` (4): the ascribed form runs all three arms; the
  bare `direct` form, the covariant probe and the invariant control
  are pinned, so a Scala release that changes the rule turns them
  red and the ascription can go then. TestDirectOnce's comment points
  at it. The backlog entry moves to refuted-declined-or-answered.
