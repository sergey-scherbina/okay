## okay2 - the okay core written a second time, in Scala 2.13

Not the facade (okay-scala2) but the core itself, again, in Scala 2.13
with nothing of Scala 3 on the classpath: the freer tree and its
rotation, rows, handlers, `Cont`, State/PState, Writer, Throws, Reader.
Module `okay2`, package `okay2`, directory `okay2/` — a SEPARATE sbt
build (`okay2/build.sbt`, operator's call), not a project of the root
build; gated with `cd okay2 && ../scripts/gate.sh test`. specs/okay2.md.

- Stage 0 measured the mechanism by hand first: the Scala 3 core's
  runtime never depended on the union (dispatch is by class, widening
  is one cast), so only the row's SPELLING needed a new encoding. A
  higher-kinded phantom alias was refuted (scalac 2 cannot give an
  alias the kind `* -> *` by partial application); a row is a kind-`*`
  type with a member `type Op[+A]`, `F + G` leaves it abstract, and an
  operation of a union row is held raw in the tree.
- Handlers find their signature ANYWHERE in a row (`Remove[F, R]`, "R
  without F", resolved by implicits) because a Scala 2 union neither
  commutes nor associates; `at` takes a whole-row witness (`Sub`).
- Nine suites mirror the Scala 3 core's: the rotation law over twelve
  bind-tree shapes, 1M-deep chains everywhere, abort, multi-shot,
  relay, translate, `Handler.union`, membership refused at compile
  time. 59 test results under `-Xlint -Werror`, in okay2's own gate.
- Six scalac-2 traps recorded with the fix for each (infix type
  precedence, newline before `(`, `@tailrec` on a polymorphic member,
  the `Nothing` checkcast, no top-level aliases, method type parameters
  not refined by a constructor pattern).

Docs: docs/okay2.md (every snippet a line of okay2's tests), linked from
docs/README.md. Backlog section `okay2`: bench (first), distinct,
stage 2 (the rest of the core), cross-build.
