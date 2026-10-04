## cont-stack-okay2-layer1b - okay2's Cont reads answer-using bodies: k(x + 1) + 1 a million deep, no frame

Layer 1 B of specs/cont-stack.md in okay2. Operator: "Окей" (okay), to
taking it on.

**The road is the Scala 3 core's cont-stack-layer1-b, on okay2's own
runner.** It is not a port of the newer frame machine. The core took this
road on a runner of the same shape (`step` plus `Reentry`) before the
frame machine existed.

- **`ContMacro`:** a body that USES `k`'s answer is transformed
  selectively (Rompf, Maier & Odersky, ICFP 2009) into a `ContCps.Cps`
  whose body is data: `Call(k, a, rest)` and `Done(r)`. It reads:
  - calls of `k`;
  - applications, function part and arguments in order. A `k`-free part
    evaluated ahead of a call is bound to a `val` first, so the order of
    effects is kept;
  - blocks and `val`s;
  - a tail `if`/`match`, whose condition or scrutinee may call `k`.

  Anything else that mentions `k` leaves the whole body the leaf, as
  before: `k` as a value, a by-name argument, a lambda, `try`, `while`, a
  `var`, an assignment, a non-tail `if`. A Scala 2 trap was met and
  handled: `c.untypecheck` resets only the symbols a fragment defines, so
  references to body-local vals are rebound by name after the transform.
- **The runner:** okay2's `step` gains the pending stack and the walked
  body as parameters. A `Call` continues the program through the
  `Reentry`'s fields in-loop. Every exit that returned an answer feeds
  the part on top instead.

**Tests.** A million `k(x + 1) + 1` and a million `val`-bound bodies run
on a 128 KB stack with zero switches; red first: StackOverflowError. The
Scala 3 core's meaning tests pass too: order of effects, an exception
after `k`, multi-shot, and the opaque shapes.

**Measured:** contAnswer 1.04x and 1.03x against the direct leaf, +48 B
a level (history.d okay2-cont-layer1b). The Scala 3 core paid 1.24x for
this road.

**Left on cont-stack-okay2-macro:** only the JDK 22+ FFM stack reader.
