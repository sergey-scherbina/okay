package okay.freer


import scala.annotation.tailrec

/**
 * THE TYPED DELIM MACHINE, PROBED (indexed-effects stage 4): the
 * shapes the design rests on, compiled before the machine is written,
 * as ProbeFreerStep did for the base. Kept compiling, like the other
 * probes, so a future Scala says by turning red whether they still
 * type. Four questions:
 *
 *  1. `Has[S, P, B]` — "p is on the stack S, and B is what lies below
 *     it" — as an INDUCTIVE VALUE (`Here`/`There`), still derived by
 *     the two givens `Shift.Stacked.Has` derived it with until
 *     shift-prompt-key (2026-10-03, the stack moved into the row), so a
 *     capture's door asks for the same evidence and gets a path.
 *  2. `Segs`, the machine's continuation stack, INDEXED BY THE PROMPT
 *     STACK: a `Mark` frame is at `P0 *: St` for the prompt's own
 *     singleton `P0`, so the type of the stack and the value of the
 *     stack are one thing.
 *  3. A cut that WALKS the witness instead of searching by identity:
 *     `Here` says the next mark is p's, `There` says skip one — no
 *     `===`, no `NotFound`, typed by the GADT at every frame. TYPES,
 *     and is REFUTED for the machine anyway (2026-09-30), by `shift`'s
 *     semantics rather than by the compiler: `reset(E[shift f]) =
 *     reset(f(x => reset E[x]))` keeps the OUTER reset while `k`
 *     installs an inner one, so when `E` runs again the stack is
 *     `… *: p *: p *: B` where it was `… *: p *: B` when `E`'s own
 *     witnesses were built — every path from inside `E` down past `p`
 *     is off by one. A positional witness is a de Bruijn index, and
 *     re-installation shifts it. So the machine keeps the identity
 *     search (`===` with `Same`) as its cut, TYPED by the stack it
 *     walks, and `Has` stays what it is today: compile-time evidence
 *     that the prompt is present, not a path.
 *  4. Whether the found mark's answer type is the prompt's BY THE
 *     TYPES: `P0 <: Prompt[X]` on the frame and `P0 <: Prompt[R]` at
 *     the capture — or whether the identity witness (`Same`, today's
 *     `===`) still has to say `X =:= R`. ANSWERED NO by the compiler
 *     (2026-09-30): with `cut` written to answer `R`, the `Here` arm
 *     read "Found: A => Cur, Required: A => R" — `x` is `Cur` by the
 *     GADT, but `P0 <: Prompt[x]` and `P0 <: Prompt[R]` together do
 *     not make `x = R` (Prompt is invariant, and two upper bounds do
 *     not meet). So the cut answers the mark's own `x`, existential,
 *     and the machine relates it to the capture's `R` with the identity
 *     witness it has today — as an ASSERTION at the mark the path
 *     reached, no longer as a search.
 */
class ProbeDelimTyped extends munit.FunSuite:

  final class Prompt[R](val label: String)

  /** the witness as a VALUE: the path to the prompt's mark */
  enum Has[S <: Tuple, P, B <: Tuple]:
    case Here[P, B <: Tuple]() extends Has[P *: B, P, B]
    case There[P, Q, S <: Tuple, B <: Tuple](below: Has[S, P, B]) extends Has[Q *: S, P, B]
  object Has:
    given here[P, S <: Tuple]: Has[P *: S, P, S] = Has.Here()
    given there[P, Q, S <: Tuple, B <: Tuple](using b: Has[S, P, B]): Has[Q *: S, P, B] = Has.There(b)

  /** the machine's continuation, indexed by the stack of installed
   * prompts; `K` is a plain function here, a program in the machine */
  enum Segs[St <: Tuple, A, Z]:
    case Done[Z]() extends Segs[EmptyTuple, Z, Z]
    case K[St <: Tuple, X, Y, Z](f: X => Y, rest: Segs[St, Y, Z]) extends Segs[St, X, Z]
    /** a delimiter: `P0` the prompt's singleton, `X` its answer, the frame at the stack it made */
    case Mark[X, P0 <: Prompt[X], St <: Tuple, Y, Z](p: P0, up: X <:< Y, rest: Segs[St, Y, Z]) extends Segs[P0 *: St, X, Z]

  /** the frames up to the mark, composed (a function here, `Frames` in
   * the machine): they answer the prompt's `R`; `up` is the mark's own
   * step to what lies outside, at the stack BELOW the prompt */
  final case class Cut[A, R, Y, B <: Tuple, Z](captured: A => R, up: R <:< Y, outer: Segs[B, Y, Z])

  /**
   * Question 3: walk `has` and `kont` as one. At `Here` the stack is
   * `P0 *: B` and the mark reached is at `Q0 *: St'`; the GADT gives
   * `Q0 = P0`, `St' = B`, and the mark's answer `x` is `Cur`, what the
   * captured frames answer — all of it typed, no test, no cast. The
   * answer is the mark's own `x` (question 4).
   */
  @tailrec final def cut[St <: Tuple, P0, B <: Tuple, A, Cur, Z](has: Has[St, P0, B], kont: Segs[St, Cur, Z], acc: A => Cur): Cut[A, ?, ?, B, Z] =
    kont match
      case k: Segs.K[St, Cur, y, Z] => cut(has, k.rest, acc.andThen(k.f))
      case m: Segs.Mark[x, ?, ?, y, Z] @unchecked => has match
        case Has.Here() => Cut[A, x, y, B, Z](acc, m.up, m.rest)
        case Has.There(below) => cut(below, m.rest, acc.andThen(m.up))
      case Segs.Done() => throw IllegalStateException("a witness reached the bottom: cannot happen at a root machine")

  test("questions 1-3: the witness is a path, the stack is typed, the cut walks it with no identity test") {
    val p1 = new Prompt[Int]("outer")
    val p2 = new Prompt[Int]("inner")
    // the stack, typed: p2 on p1 on nothing; a K between them
    val inner: Segs[p1.type *: EmptyTuple, Int, Int] =
      Segs.K(_ + 1, Segs.Mark[Int, p1.type, EmptyTuple, Int, Int](p1, <:<.refl, Segs.Done()))
    val kont: Segs[p2.type *: p1.type *: EmptyTuple, Int, Int] =
      Segs.Mark[Int, p2.type, p1.type *: EmptyTuple, Int, Int](p2, <:<.refl, inner)
    // the witnesses from the givens: Here for the top, There(Here) for the one below
    val has2 = summon[Has[p2.type *: p1.type *: EmptyTuple, p2.type, p1.type *: EmptyTuple]]
    assert(has2.isInstanceOf[Has.Here[?, ?]])
    val has1 = summon[Has[p2.type *: p1.type *: EmptyTuple, p1.type, EmptyTuple]]
    assert(has1.isInstanceOf[Has.There[?, ?, ?, ?]])
    // a cut to the inner prompt keeps the K and the outer mark outside; a cut to the outer keeps nothing
    val c2 = cut(has2, kont, identity[Int])
    assert(c2.outer.isInstanceOf[Segs.K[?, ?, ?, ?]])
    assertEquals(c2.captured(41): Any, 41)
    val c1 = cut(has1, kont, identity[Int])
    assert(c1.outer.isInstanceOf[Segs.Done[?]])
    assertEquals(c1.captured(41): Any, 42, "the K between the marks is part of the captured segment")
  }
