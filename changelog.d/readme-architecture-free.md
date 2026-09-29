## readme-architecture-free - the README's Architecture starts at Free, and its Effects run from common to rare

The operator: Free is the base of everything and belongs first; Cont
should say it is itself an effect inside Free; Delim was missing from the
effects; the list wanted an order.

- Architecture opens with `Free[F, A]`: the freer monad, its four cases,
  `A ! F` as `Free[F, A]`, rotations for stack safety. `Cont` follows as
  `Free[Shift, A]`, one effect whose operation is a function of the
  continuation, so running a `Cont` is handling that effect.
- Effects in order: Reader, State, Writer, Throws, Resource, Async,
  Choice/Logic, Delim (new: multi-prompt delimited control, Dybvig,
  Peyton Jones and Sabry's shape), then the two general ones: several
  instances of one effect, and your own effect.
- The Async entry named Loom as the JVM scheduler; it says adaptive by
  default, Loom a `given` away (scheduler-default-flip).
