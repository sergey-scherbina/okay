package okay.freer

import okay.{guard}


import okay.freer.Freer.{Return, Inject, Bind, Delay}
import scala.annotation.tailrec

/**
 * THE ABSTRACT MACHINE FOR `Freer`, effect-independent (specs/cont-atm.md; the operator: "Freer/Delimited должны
 * быть полностью независимы от того какой именно эффект в нем работает — их задача предоставить все
 * необходимые примитивы абстрактной машины для реализации эффектов").
 *
 * An interpreter of `Freer` written directly is correct and typed by `Freer`'s own indexes, but grows the host
 * stack in two places: the continuations of `Bind` (opaque lambdas) and the interpreter's own nested runs. The
 * machine is that host stack made DATA (the functional correspondence: Ager, Biernacki, Danvy & Midtgaard 2003;
 * Biernacka, Biernacki & Danvy 2005), segmented as Dybvig, Peyton Jones & Sabry's (JFP 2007) is:
 *
 *  - `Frames`, a SEGMENT, the operator's `A => F[B, S, R]`: `Bind`'s continuations as data, joined as `Bind`
 *    joins — nothing else;
 *  - `Stack`: segments joined by BOUNDARIES, of two kinds, each typed by its installation — a VALUE boundary
 *    (`Delim`: a value passes, the answer types chain through: a prompt, a nested run's barrier, a handler's
 *    frame, a MARK an effect finds again — an environment, which a capture carries) and an ANSWER boundary
 *    (`Bound`: the level closes and its answer flows out as a value — Danvy & Filinski's `reset`, answer-type
 *    modification typed with no claim).
 *
 * Everything an effect looks for is a boundary, so every search walks boundaries, never frames, and a capture to
 * the nearest takes the segment as it is. `Delimited` is the interface — the machine's primitives, which an
 * effect's `Step` answers its operations with; `Run` is the one loop. Nothing here names an effect.
 */
trait Delimited[G[_, _, +_]]:
  import Delimited.{Mark, Tag, Next, Piece, Found, Closed, Kont}

  /** a captured continuation run from `x` to its answer, NOW: a nested run */
  def force[A, T](k: Kont[G, A, T], x: A): T

  /** the next state: a program, its segment, the stack under it */
  def next[A, B, S, T, R, Z](c: Freer[G, T, R, A], k: Frames[G, A, B, S, T], m: Stack[G, B, S, R, Z]): Next[G, Z]

  /** the empty segment */
  def end[A, S]: Frames[G, A, A, S, S]

  /** a frame on a segment: `f`'s value flows into `rest` */
  def frame[A, X, B, S, T, R](f: A => Freer[G, T, R, X], rest: Frames[G, X, B, S, T]): Frames[G, A, B, S, R]

  /** a VALUE boundary over `rest`, marked `tag`: the level's value goes on into `out`, its answers chain through.
   * With a tag and nothing else to it, it is a MARK: transparent, found by `holds`, carried by a capture */
  def delim[B, S, R, B2, S2, Z](tag: Mark | Null, out: Frames[G, B, B2, S2, S], rest: Stack[G, B2, S2, R, Z]): Stack[G, B, S, R, Z]

  /** an ANSWER boundary over `rest`: the level inside closes and answers `R`; `out` takes that on */
  def bound[S, R, B2, S2, X, Z](tag: Tag[R] | Null, out: Frames[G, R, B2, S2, X], rest: Stack[G, B2, S2, X, Z]): Stack[G, S, S, R, Z]

  /** the nearest boundary whose mark `is` holds for; null when none */
  def holds[B, S, R, Z](m: Stack[G, B, S, R, Z], is: Mark => Boolean): Mark | Null

  /** walk out over value boundaries to the nearest whose mark `is` holds for: the piece up to and including it,
   * and what lies under it; null when there is none, or a boundary `stop` holds for comes first */
  def cut[A, B, S, T, R, Z](k: Frames[G, A, B, S, T], m: Stack[G, B, S, R, Z], is: Mark => Boolean,
                            stop: Mark => Boolean): Found[G, A, T, R, Z] | Null

  /** a captured piece put back on top of a segment and its stack, and `c` run inside it: its value into the
   * piece's innermost frames, its captures reaching the piece's boundaries (DPJS's `pushSubCont`) */
  def reinstall[A0, Y, I, B, S, R, Z](piece: Piece[G, A0, R, Y, I], c: Freer[G, R, R, A0], k: Frames[G, Y, B, S, I], m: Stack[G, B, S, R, Z]): Next[G, Z]

  /** walk out over value boundaries to the nearest ANSWER boundary: the continuation it closes, value boundaries
   * (marks) and all; null when there is none */
  def closed[A, B, S, T, R, Z](k: Frames[G, A, B, S, T], m: Stack[G, B, S, R, Z]): Closed[G, A, T, R, Z] | Null

  /** from now on this run calls user code under a `try`, and a throw goes to the step's `thrown` (a catch frame
   * was installed): until then a run pays nothing for exceptions */
  def guarding(): Unit

  /**
   * an OPAQUE body given a STRICT `k` — `k(x)` a nested run of `c` from `x`, its answer waited for — and the body's
   * answer into `c`. The one place a run nests the host stack (the bridge); with `ContReplay.on`, at the end of the
   * room the innermost such `k` SUSPENDS and the body is re-executed later, its `k`'s answers remembered
   * (cont-js-depth stage 4)
   */
  def strict[A, T, R, Z](c: Closed[G, A, T, R, Z], body: (A => T) => R, replayable: Boolean): Next[G, Z]

/** a SEGMENT, `A => Freer[G, S, R, B]` as data: frames joined as `Bind` joins them. Contravariant in `A`: it
 * consumes a value. */
enum Frames[G[_, _, +_], -A, B, S, R]:
  case End[G[_, _, +_], A, S]() extends Frames[G, A, A, S, S]
  case Frame[G[_, _, +_], A, X, B, S, T, R](f: A => Freer[G, T, R, X], rest: Frames[G, X, B, S, T])
    extends Frames[G, A, B, S, R]

object Frames:
  private val theEnd: End[[S, R, X] =>> Nothing, Any, Any] = End()

  /**
   * The empty segment, one instance for every index (shift-capture-objects). THE CAST: `End` holds nothing, so no
   * value in it can be read at the wrong type. Its indexes say only that its input is its output, which holds
   * for every instance alike.
   */
  def end[G[_, _, +_], A, S]: Frames[G, A, A, S, S] = theEnd.asInstanceOf[Frames[G, A, A, S, S]]

/** THE STACK: what closes a level computing `Freer[G, S, R, B]` into the run's result `Z` */
enum Stack[G[_, _, +_], B, S, R, Z]:
  /** the run's end by VALUE: the level's value is the result, its answers diagonal */
  case Done[G[_, _, +_], B, X]() extends Stack[G, B, X, X, B]
  /** the run's end by ANSWER: the level closes (its value is its answer index) and its answer is the result */
  case Answered[G[_, _, +_], S, R]() extends Stack[G, S, S, R, R]
  /** a VALUE boundary: the level's value flows on into `out`; its answers chain through, as `Bind`'s do */
  case Delim[G[_, _, +_], B, S, R, B2, S2, Z](tag: Delimited.Mark | Null, out: Frames[G, B, B2, S2, S],
                                              rest: Stack[G, B2, S2, R, Z]) extends Stack[G, B, S, R, Z]
  /** an ANSWER boundary: the level closes and answers `R`; outside it, `out` takes that on as its value */
  case Bound[G[_, _, +_], S, R, B2, S2, X, Z](tag: Delimited.Tag[R] | Null, out: Frames[G, R, B2, S2, X],
                                               rest: Stack[G, B2, S2, X, Z]) extends Stack[G, S, S, R, Z]

object Delimited:

  /**
   * A STEP: what the machine does with one of effect `G`'s operations in a program over the row `H` — given the
   * segment up to the nearest boundary and the stack under it, the next state, built with the machine's
   * primitives. `G[T, R, A]` is the operation at the program's indexes; the equations its own GADT match supplies
   * type the state it answers.
   */
  trait Step[G[_, _, +_], H[_, _, +_]]:
    def step[A, B, S, T, R, Z](op: G[T, R, A], k: Frames[H, A, B, S, T], m: Stack[H, B, S, R, Z],
                               machine: Delimited[H]): Next[H, Z]

    /** a throw from user code at segment `k` and stack `m`, in a run that is `guarding`: the next state, or null —
     * nobody here takes it, and it is thrown on */
    def thrown[A, B, S, T, R, Z](t: Throwable, k: Frames[H, A, B, S, T], m: Stack[H, B, S, R, Z],
                                 machine: Delimited[H]): Next[H, Z] | Null = null

  /**
   * WHAT LEAVES A RUN, told by the effect running it: an operation of the effects outside `F` goes out as a node
   * of the program the run answers; a deferred run of the same row is stepped into rather than forced
   */
  trait Outer[G[_, _, +_], F[+_]]:
    /** `op` as an operation of the effects outside, or null when this run answers it — which may depend on the
     * boundaries on its stack `m` (`machine.holds`) */
    def apply[T, R, A, B, S, Z](op: G[T, R, A], m: Stack[G, B, S, R, Z], machine: Delimited[G]): F[A] | Null
    /** an operation sent out stands at one index */
    def diagonal[T, R, A](op: G[T, R, A]): T =:= R
    /** a deferred run of this row the run steps into rather than forces (a nested run): its program, or null */
    def enter[T, R, A](t: () => Freer[G, T, R, A]): Freer[G, T, R, A] | Null = null
    /** the mark of the value boundary stepping into `t` installs (a barrier), or null for none */
    def barrier(t: () => Any): Mark | Null = null

  /**
   * HOW AN OPERATION LEAVES: as a node of the program a run answers, at the tree signature `O` the effects
   * outside are written at, every index `Unit` — the core's `Unary[F]`, which this machine never names: the
   * caller says how an `F[X]` is an `O[Unit, Unit, X]` (the identity, on the diagonal)
   */
  trait Leaving[F[+_], O[_, _, +_]]:
    def apply[X](op: F[X]): O[Unit, Unit, X]

  /** nothing outside */
  type None[+X] = Nothing
  /** nothing leaves: the signature of a run alone */
  type Nowhere[S, R, +X] = Nothing
  object Leaving:
    /** nothing leaves a run alone */
    val none: Leaving[None, Nowhere] = new Leaving[None, Nowhere]:
      def apply[X](op: Nothing): Nothing = op

  /** a run where every operation is the effect's own */
  private final class Alone[G[_, _, +_]] extends Outer[G, None]:
    def apply[T, R, A, B, S, Z](op: G[T, R, A], m: Stack[G, B, S, R, Z], machine: Delimited[G]): None[A] | Null = null
    def diagonal[T, R, A](op: G[T, R, A]): T =:= R = throw IllegalStateException("nothing leaves a run alone")

  /** a machine for one effect, alone */
  def apply[G[_, _, +_]](steps: Step[G, G]): Machine[G] = Machine(Run(steps, Alone[G](), Leaving.none))

  /**
   * an operation of a program over effect `G` under the effects `F`: one of `G`'s, at any indexes (`Own`), or one
   * of `F`'s, which passes through and so is DIAGONAL — it changes no answer type of `G`'s levels (`Fwd`)
   */
  enum Sum[G[_, _, +_], F[+_], S, R, +A]:
    case Own[G[_, _, +_], F[+_], S, R, A](op: G[S, R, A]) extends Sum[G, F, S, R, A]
    case Fwd[G[_, _, +_], F[+_], S, A](op: F[A]) extends Sum[G, F, S, S, A]

  /** the row: `G`'s operations and `F`'s */
  type Row[G[_, _, +_], F[+_]] = [S, R, A] =>> Sum[G, F, S, R, A]

  /** a machine for effect `G` under the effects `F` (nested machines): it answers `G`'s operations and leaves
   * `F`'s in the program it answers, for the machine outside */
  def under[G[_, _, +_], F[+_], O[_, _, +_]](steps: Step[G, Row[G, F]])(using l: Leaving[F, O]): Run[Row[G, F], F, O] =
    Run(Own(steps), Fwd[G, F](), l)

  /** `Own`'s operations to the effect's steps */
  private final class Own[G[_, _, +_], F[+_]](steps: Step[G, Row[G, F]]) extends Step[Row[G, F], Row[G, F]]:
    def step[A, B, S, T, R, Z](op: Sum[G, F, T, R, A], k: Frames[Row[G, F], A, B, S, T], m: Stack[Row[G, F], B, S, R, Z],
                               machine: Delimited[Row[G, F]]): Next[Row[G, F], Z] = op match
      case Sum.Own(o) => steps.step(o, k, m, machine)
      case Sum.Fwd(_) => throw IllegalStateException("a forwarded operation reached the steps")
    override def thrown[A, B, S, T, R, Z](t: Throwable, k: Frames[Row[G, F], A, B, S, T], m: Stack[Row[G, F], B, S, R, Z],
                                          machine: Delimited[Row[G, F]]): Next[Row[G, F], Z] | Null = steps.thrown(t, k, m, machine)

  /** `Fwd`'s operations out, diagonal by their constructor */
  private final class Fwd[G[_, _, +_], F[+_]] extends Outer[Row[G, F], F]:
    def apply[T, R, A, B, S, Z](op: Sum[G, F, T, R, A], m: Stack[Row[G, F], B, S, R, Z], machine: Delimited[Row[G, F]]): F[A] | Null =
      op match
        case Sum.Fwd(o) => o
        case _ => null
    def diagonal[T, R, A](op: Sum[G, F, T, R, A]): T =:= R = op match
      case Sum.Fwd(_) => summon[T =:= R]
      case _ => throw IllegalStateException("an own operation is not sent out")

  /** a machine for an effect `G` whose effects outside are `F`: it answers every operation of `G` but those
   * `outer` sends out, which leave in the program it answers, at the signature `O` (`Unary[F]` for a `Free` row) */
  def over[G[_, _, +_], F[+_], O[_, _, +_]](steps: Step[G, G], outer: Outer[G, F])(using l: Leaving[F, O]): Run[G, F, O] =
    Run(steps, outer, l)

  // ---- what the primitives speak of ----

  /** what an effect hangs on the stack to find again; opaque to the machine */
  trait Mark

  /** an answer boundary's mark, with `T` the answer of its level */
  trait Tag[T] extends Mark

  /**
   * a throw from user code, as the program a guarded call answers instead (`guarding`): a `Delay` of this. The
   * machine hands it to its step's `thrown`; anything else that forces it throws it again, the same object — so
   * it is a correct program everywhere, and typed at any answer with no claim (`() => Nothing`)
   */
  /**
   * A STRICT `k` OUT OF ROOM (cont-js-depth stage 4): thrown by the innermost strict body's `k` instead of nesting
   * further, it unwinds the host stack to the run's driver; every strict body it passes records how to run it again
   * (`again`, outermost last). The driver runs `k` from `x` on the shallow stack and those bodies again, innermost
   * first. A control throwable: no stack trace, and `NonFatal` lets it pass — a body that catches `Throwable` around
   * its `k` swallows it (the contract, docs/cont-stack.md).
   */
  final class Suspend private[Delimited] (private[Delimited] val run: AnyRef, private[Delimited] val k: Kont[?, ?, ?],
                                          private[Delimited] val x: Any) extends scala.util.control.ControlThrowable:
    /** what to run again, each given the value its pending `k` call answers: the innermost first */
    private[Delimited] var again: List[Any => Any] = Nil

  final class Thrown(val t: Throwable) extends (() => Nothing):
    def apply(): Nothing = throw t

  /** the machine's state, which a step answers: a program, its segment, the stack under it */
  sealed abstract class Next[G[_, _, +_], Z]:
    type A
    type B
    type S
    type T
    type R
    def c: Freer[G, T, R, A]
    def k: Frames[G, A, B, S, T]
    def m: Stack[G, B, S, R, Z]

  /** a captured piece of the stack, built outward: from the hole `A0` (at index `T0`) through segments and the
   * value boundaries between them to `A` (at index `T`) — at least one segment and its boundary, so a capture
   * allocates no empty piece to start from */
  enum Piece[G[_, _, +_], A0, T0, A, T]:
    case One[G[_, _, +_], A0, T0, B, S](k: Frames[G, A0, B, S, T0], tag: Mark | Null) extends Piece[G, A0, T0, B, S]
    case Snoc[G[_, _, +_], A0, T0, A, T, B, S](prev: Piece[G, A0, T0, A, T], k: Frames[G, A, B, S, T], tag: Mark | Null)
      extends Piece[G, A0, T0, B, S]

  /** what a capture found: the piece up to the marked boundary, and what lies under it */
  sealed abstract class Found[G[_, _, +_], A0, T0, R, Z]:
    type Y
    type I
    type B2
    type S2
    def piece: Piece[G, A0, T0, Y, I]
    def tag: Mark
    def out: Frames[G, Y, B2, S2, I]
    def rest: Stack[G, B2, S2, R, Z]

  /** a CAPTURED continuation: segments and the value boundaries between them, the last closed by an answer
   * boundary — from `A` (at index `T`) to the answer `T` */
  sealed abstract class Kont[G[_, _, +_], -A, T]:
    /** the run of it from `a`, its answer the run's result */
    def from(a: A): Next[G, T]
    /** run it from `a` NOW, a nested run of the machine that captured it — a BARRIER to re-execution: a suspension
     * inside it goes no further out (its caller may be no strict body) */
    private[Delimited] def forced(a: A): T
    /** the same with no barrier: a strict body's own call, which a suspension may cross (cont-js-depth stage 4) */
    private[Delimited] def forcedIn(a: A): T
    /** put back with `a`, its answer delivered through a fresh answer boundary into `out` over `rest` */
    def resume[B2, S2, X, Z](a: A, out: Frames[G, T, B2, S2, X], rest: Stack[G, B2, S2, X, Z]): Next[G, Z]

  /** the nearest answer boundary: the continuation it closes, and its two ways on */
  sealed abstract class Closed[G[_, _, +_], A, T, R, Z] extends Kont[G, A, T]:
    /** the level answers `r`: it goes to the boundary, and on outside it */
    def answer(r: R): Next[G, Z]
    /** `c` runs in the level's place, closed by the same boundary */
    def instead(c: Freer[G, R, R, R]): Next[G, Z]

  /** a run for one effect alone: its answer, not a program */
  final class Machine[G[_, _, +_]] private[Delimited] (val loop: Run[G, None, Nowhere]):
    /** run `c` with `k` as the last frame of its continuation; the result is its answer */
    def run[A, S, R](c: Freer[G, S, R, A], k: A => S): R = loop.runAlone(c, k)
    /** run `c` to its value */
    def value[A, X](c: Freer[G, X, X, A]): A = loop.valueAlone(c)
    /** a continuation run now */
    def force[A, T](k: Kont[G, A, T], x: A): T = loop.force(k, x)

  // ---- THE LOOP ----

  /**
   * THE ONE LOOP, for every effect: `G`'s operations go to `steps`, but those `outer` sends out, which leave as
   * nodes of the program the run answers, the run's state behind them in a `Delay` — so the interpreter outside,
   * forcing it, re-enters this loop with no host frame per nesting. A nested run of the same row (`outer.enter`)
   * is stepped into, under a value boundary, not forced: its depth is this run's stack, not the host's.
   */
  final class Run[G[_, _, +_], F[+_], O[_, _, +_]](steps: Step[G, G], outer: Outer[G, F], leaving: Leaving[F, O]) extends Delimited[G]:
    // @publicInBinary: `within` is inline and sets it from every caller — no unstable `inline$room` accessor (E192)
    @scala.annotation.publicInBinary private[Delimited] var room: Int = StackSwitch.firstRoom
    /** levels this stack may still be granted, read or not: past them a fresh stack (`StackSwitch.levelsPerStack`).
     * Changed only at the end of a room, so the per-level path does not touch it */
    private var budget: Int = StackSwitch.levelsPerStack - StackSwitch.firstRoom

    /** user code runs under a `try` (`guarding`) */
    private var guarded: Boolean = false
    /** nothing leaves: no question to ask an operation (a machine for one effect) */
    private val alone: Boolean = outer.isInstanceOf[Alone[?]]
    def guarding(): Unit = guarded = true

    /** run `c` with `k` as the last frame of its continuation: the program, over `F`, that answers its answer */
    def run[A, S, R](c: Freer[G, S, R, A], k: A => S): Freer[O, Unit, Unit, R] =
      go(c, frame((a: A) => Return[G, S, S](k(a)), end[S, S]), Stack.Answered[G, S, R]())

    /** run `c` to its value: the program, over `F`, that answers it */
    def value[A, X](c: Freer[G, X, X, A]): Freer[O, Unit, Unit, A] = go(c, end[A, X], Stack.Done[G, A, X]())

    /** a strict `k` cannot wait for an effect outside: one met inside it is refused, by name */
    def force[A, T](k: Kont[G, A, T], x: A): T = k.forced(x)

    /** a closed segment run from `x` now: no `Next` between (a strict `k` is called once an operation) — on a
     * machine alone through its own loop, which answers `T` itself, no `Return` built to be taken apart */
    private def forceAt[A, S, T](k: Frames[G, A, S, S, T], x: A): T =
      if alone then deeper(goAlone(Return[G, T, A](x), k, Stack.Answered[G, S, T]()))
      else answerOf(deeper(go(Return[G, T, A](x), k, Stack.Answered[G, S, T]())))

    /** a machine alone run with `k` as the last frame: its answer (`Delimited.Machine`) */
    private[Delimited] def runAlone[A, S, R](c: Freer[G, S, R, A], k: A => S): R =
      drive(goAlone(c, frame((a: A) => Return[G, S, S](k(a)), end[S, S]), Stack.Answered[G, S, R]()))

    /** a machine alone run to its value */
    private[Delimited] def valueAlone[A, X](c: Freer[G, X, X, A]): A = drive(goAlone(c, end[A, X], Stack.Done[G, A, X]()))

    // ---- THE STRICT `k` BY RE-EXECUTION (cont-js-depth stage 4, specs/cont-js-depth.md)

    /** a driver is running this run: a suspension has somewhere to go */
    private var driving: Boolean = false
    /** the strict `k` of the body being run — the only one that may suspend (a `k` called from a lambda of the run
     * is not replayable); null inside the nested run a `k` call starts, until a strict body there sets its own */
    private var current: AnyRef | Null = null
    /** strict `k` calls nested on the host stack now */
    private var levels: Int = 0

    def strict[A, T, R, Z](c: Closed[G, A, T, R, Z], body: (A => T) => R, replayable: Boolean): Next[G, Z] =
      if !ContReplay.on then c.answer(body(x => c.forcedIn(x)))
      // never re-executed (cont-safe-mode): its `k` a barrier, so no suspension from below crosses this body
      else if !replayable then c.answer(body(x => c.forced(x)))
      else c.answer(strictRun(c, body, Nil))

    /** `body` run with a recording `k`, `known` its first answers (a re-run); a suspension crossing it records how to
     * run it again: with the answers it had, and the pending call's from the driver */
    private def strictRun[A, T, R, Z](c: Closed[G, A, T, R, Z], body: (A => T) => R, known: List[Any]): R =
      val k = StrictK[A, T](c, known)
      val saved = current
      current = k
      try body(k)
      catch case s: Suspend if s.run eq this =>
        val had = k.answered
        s.again = ((v: Any) => { val n = c.answer(strictRun(c, body, had :+ v)); goAlone(n.c, n.k, n.m) }) :: s.again
        throw s
      finally current = saved

    /** a strict body's `k`: a nested run a call, the answers kept; a re-run's first calls answered from `known` */
    private final class StrictK[A, T](c: Kont[G, A, T], private var known: List[Any]) extends (A => T):
      private var got: List[Any] = Nil
      def answered: List[Any] = got.reverse
      def apply(x: A): T =
        // a call from the continuation itself (re-entrant, inside a call of this `k`) is no call of the body's:
        // neither recorded nor replayed, and it never suspends
        if !(current eq this) then c.forced(x)
        else
          val v: T = known match
            // THE CLAIM: a re-run makes the same calls in the same order (the contract), so the n-th call's answer is
            // the n-th answer recorded, of this `k`'s type
            case g :: more => known = more; g.asInstanceOf[T]
            case Nil =>
              if driving && levels >= ContReplay.room then throw Suspend(Run.this, c, x)
              nested(x)
          got = v :: got
          v

      private def nested(x: A): T =
        val saved = current
        current = null
        levels += 1
        try c.forcedIn(x) finally { levels -= 1; current = saved }

    /**
     * THE DRIVER: `start` run; a suspension it throws unwinds to here, where its `k` runs from its `x` on this
     * shallow stack and the bodies it crossed run again, innermost first — each given the value the one before
     * answered. A driver already running takes them (one per run).
     */
    private def drive[Z](start: => Z): Z =
      if driving || !ContReplay.on then start
      else
        driving = true
        // THE CLAIM: the last value is the top's — `start`'s, or what the outermost re-run body's continuation answered
        try turn(() => start, Nil).asInstanceOf[Z]
        finally driving = false

    /**
     * a nested run whose caller is no strict body — a `k` called from a lambda of the run, or from outside it: its
     * own driver and room, so no suspension crosses the caller, which could not be run again. Its depth on the host
     * stack is that caller's (specs/cont-js-depth.md, stage 4, out of scope: a run nested in user code)
     */
    private def barrier[X](body: => X): X =
      if !ContReplay.on then body
      else
        val wasDriving = driving
        val wasLevels = levels
        val wasCurrent = current
        driving = false
        levels = 0
        current = null
        try drive(body) finally { driving = wasDriving; levels = wasLevels; current = wasCurrent }

    @tailrec private def turn(act: () => Any, work: List[Any => Any]): Any =
      (try act() catch case s: Suspend if s.run eq this => s) match
        case s: Suspend if s.run eq this =>
          val k = s.k.asInstanceOf[Kont[G, Any, Any]]   // THE CLAIM: a Suspend of this run carries one of its own `k`s
          turn(() => k.forcedIn(s.x), s.again.reverse ::: work)
        case v => work match
          case Nil => v
          case again :: rest => turn(() => again(v), rest)

    /** a run's answer, which a strict `k` cannot wait for an effect outside to give */
    private def answerOf[T](p: Freer[O, Unit, Unit, T]): T = p match
      case Return(t) => t
      case _ => throw IllegalStateException("a strict k performed an operation of an outer effect; give its body the lazy k")

    /** `body` one level deeper: at the end of a room, more of this stack where it is READ to have it
     * (`StackSwitch.more`), else a fresh stack */
    private def deeper[X](body: => X): X =
      val here = room - 1
      if here > 0 then within(here, body) else roomEnd(() => body)

    /** the end of a room, out of `deeper`: where the stack is read this branch is taken, and kept inline it grew
     * the per-level path past what C2 inlines — a capture's `Segment` escaped, 2.2x at a million levels
     * (cont-stack-exact-first) */
    private def roomEnd[X](body: () => X): X =
      val granted = math.min(StackSwitch.more(), budget)
      if granted > 0 then
        val left = budget
        budget = left - granted
        try within(granted, body()) finally budget = left
      else
        StackSwitch.fresh: first =>
          val left = budget
          budget = StackSwitch.levelsPerStack - first
          try within(first, body()) finally budget = left

    private def within[X](left: Int, body: => X): X =
      val saved = room
      room = left
      try body finally room = saved

    @tailrec private def go[A, B, S, T, R, Z](c: Freer[G, T, R, A], k: Frames[G, A, B, S, T], m: Stack[G, B, S, R, Z]): Freer[O, Unit, Unit, Z] =
      c match
        case Return(a) => k match
          case Frames.Frame(f, k2) => go(if guarded then guard(f(a)) else f(a), k2, m)
          case Frames.End() => m match
            case Stack.Done() => Return(a)
            case Stack.Answered() => Return(a)
            case Stack.Delim(_, out, rest) => go(Return(a), out, rest)
            case Stack.Bound(_, out, rest) => go(Return(a), out, rest)
        case Bind(c0, f) => go(c0, Frames.Frame(f, k), m)
        case Delay(t) => t match
          case th: Thrown =>
            val n = steps.thrown(th.t, k, m, this)
            if n == null then throw th.t
            go(n.c, n.k, n.m)
          case _ => outer.enter(t) match
            case null => go(if guarded then guard(t()) else t(), k, m)
            // stepped into: under a barrier when the effect asks for one, else straight on — no boundary at all
            case inner => outer.barrier(t) match
              case null => go(inner, k, m)
              case b => go(inner, end[A, T], Stack.Delim(b, k, m))
        case Inject(op) => (if alone then null else outer(op, m, this)) match
          case null =>
            val n =
              if !guarded then steps.step(op, k, m, this)
              else
                try steps.step(op, k, m, this)
                catch case t: Throwable => next(Delay(Thrown(t)), k, m)
            go(n.c, n.k, n.m)
          case o => forward(o.nn, k, outer.diagonal(op).flip.substituteCo[[r] =>> Stack[G, B, S, r, Z]](m))

    /**
     * THE LOOP OF A MACHINE ALONE (strict-k-cost): nothing leaves it, so it answers `Z` itself. Through the one loop,
     * whose answer is `Freer[O, Unit, Unit, Z]` for the operations that leave, a strict `k` — a nested run per call — built a
     * `Return` to take apart again: statePara 1.12x and +32 KB against cont-atm, one `Return` a call (history.d
     * strict-k-cost). These are `go`'s arms less the two a machine alone cannot reach: an operation sent out, a
     * deferred run stepped into.
     */
    @tailrec private def goAlone[A, B, S, T, R, Z](c: Freer[G, T, R, A], k: Frames[G, A, B, S, T], m: Stack[G, B, S, R, Z]): Z =
      c match
        case Return(a) => k match
          case Frames.Frame(f, k2) => goAlone(if guarded then guard(f(a)) else f(a), k2, m)
          case Frames.End() => m match
            case Stack.Done() => a
            case Stack.Answered() => a
            case Stack.Delim(_, out, rest) => goAlone(Return(a), out, rest)
            case Stack.Bound(_, out, rest) => goAlone(Return(a), out, rest)
        case Bind(c0, f) => goAlone(c0, Frames.Frame(f, k), m)
        case Delay(t) => t match
          case th: Thrown =>
            val n = steps.thrown(th.t, k, m, this)
            if n == null then throw th.t
            goAlone(n.c, n.k, n.m)
          case _ => goAlone(if guarded then guard(t()) else t(), k, m)
        case Inject(op) =>
          val n =
            if !guarded then steps.step(op, k, m, this)
            else
              try steps.step(op, k, m, this)
              catch case t: Throwable => next(Delay(Thrown(t)), k, m)
          goAlone(n.c, n.k, n.m)

    /** a call of user code under the `try`: a throw answered as a `Thrown` program */
    private inline def guard[T, R, A](inline body: Freer[G, T, R, A]): Freer[G, T, R, A] =
      try body
      catch case t: Throwable => Delay(Thrown(t))

    /** an operation out, as a node of the answered program; its answer enters this loop again, in a `Delay` */
    private def forward[X, B, S, T, Z](o: F[X], k: Frames[G, X, B, S, T], m: Stack[G, B, S, T, Z]): Freer[O, Unit, Unit, Z] =
      Bind(Inject[O, Unit, Unit, X](leaving(o)), (x: X) => Delay(() => again(Return[G, T, X](x), k, m)))

    /** the loop entered again from the outside: a call, so `go` stays a loop */
    private def again[A, B, S, T, R, Z](c: Freer[G, T, R, A], k: Frames[G, A, B, S, T], m: Stack[G, B, S, R, Z]): Freer[O, Unit, Unit, Z] =
      go(c, k, m)

    // ---- the primitives ----

    def next[A0, B0, S0, T0, R0, Z](c0: Freer[G, T0, R0, A0], k0: Frames[G, A0, B0, S0, T0], m0: Stack[G, B0, S0, R0, Z]): Next[G, Z] =
      new Next[G, Z]:
        type A = A0
        type B = B0
        type S = S0
        type T = T0
        type R = R0
        def c = c0
        def k = k0
        def m = m0

    def end[A, S]: Frames[G, A, A, S, S] = Frames.end[G, A, S]
    def frame[A, X, B, S, T, R](f: A => Freer[G, T, R, X], rest: Frames[G, X, B, S, T]): Frames[G, A, B, S, R] =
      Frames.Frame(f, rest)
    def delim[B, S, R, B2, S2, Z](tag: Mark | Null, out: Frames[G, B, B2, S2, S], rest: Stack[G, B2, S2, R, Z]): Stack[G, B, S, R, Z] =
      Stack.Delim(tag, out, rest)
    def bound[S, R, B2, S2, X, Z](tag: Tag[R] | Null, out: Frames[G, R, B2, S2, X], rest: Stack[G, B2, S2, X, Z]): Stack[G, S, S, R, Z] =
      Stack.Bound(tag, out, rest)

    @tailrec final def holds[B, S, R, Z](m: Stack[G, B, S, R, Z], is: Mark => Boolean): Mark | Null = m match
      case Stack.Delim(tag, _, rest) => if tag != null && is(tag) then tag else holds(rest, is)
      case Stack.Bound(tag, _, rest) => if tag != null && is(tag) then tag else holds(rest, is)
      case _ => null

    def cut[A, B, S, T, R, Z](k: Frames[G, A, B, S, T], m: Stack[G, B, S, R, Z], is: Mark => Boolean,
                              stop: Mark => Boolean): Found[G, A, T, R, Z] | Null =
      m match
        case Stack.Delim(tag, out, rest) =>
          if tag != null && is(tag) then found(Piece.One(k, tag), tag, out, rest)
          else if tag != null && stop(tag) then null
          else cutFrom(Piece.One(k, tag), out, rest, is, stop)
        case _ => null

    @tailrec private def cutFrom[A0, T0, A, B, S, T, R, Z](piece: Piece[G, A0, T0, A, T], k: Frames[G, A, B, S, T],
                                                           m: Stack[G, B, S, R, Z], is: Mark => Boolean,
                                                           stop: Mark => Boolean): Found[G, A0, T0, R, Z] | Null = m match
      case Stack.Delim(tag, out, rest) =>
        if tag != null && is(tag) then found(Piece.Snoc(piece, k, tag), tag, out, rest)
        else if tag != null && stop(tag) then null
        else cutFrom(Piece.Snoc(piece, k, tag), out, rest, is, stop)
      case _ => null

    private def found[A0, T0, Y0, I0, B20, S20, R, Z](p: Piece[G, A0, T0, Y0, I0], t: Mark, o: Frames[G, Y0, B20, S20, I0],
                                                      r: Stack[G, B20, S20, R, Z]): Found[G, A0, T0, R, Z] =
      new Found[G, A0, T0, R, Z]:
        type Y = Y0
        type I = I0
        type B2 = B20
        type S2 = S20
        def piece = p
        def tag = t
        def out = o
        def rest = r

    def reinstall[A0, Y, I, B, S, R, Z](piece: Piece[G, A0, R, Y, I], c: Freer[G, R, R, A0], k: Frames[G, Y, B, S, I], m: Stack[G, B, S, R, Z]): Next[G, Z] =
      link(piece, k, m, c)

    @tailrec private def link[A0, R, A, T, B, S, Z](piece: Piece[G, A0, R, A, T], k: Frames[G, A, B, S, T],
                                                    m: Stack[G, B, S, R, Z], c: Freer[G, R, R, A0]): Next[G, Z] = piece match
      case Piece.One(kk, tag) => next(c, kk, Stack.Delim(tag, k, m))
      case Piece.Snoc(prev, kk, tag) => link(prev, kk, Stack.Delim(tag, k, m), c)

    def closed[A, B, S, T, R, Z](k: Frames[G, A, B, S, T], m: Stack[G, B, S, R, Z]): Closed[G, A, T, R, Z] | Null = m match
      // the usual one: the answer boundary right under the segment, the continuation that segment alone
      case Stack.Bound(tag, out, rest) => segmentBy(k, tag, out, rest)
      case Stack.Answered() => segmentAtTop[A, S, T, R](k)
      case Stack.Delim(tag, out, rest) => closedFrom(Piece.One(k, tag), out, rest)
      case _ => null

    @tailrec private def closedFrom[A0, T0, A, B, S, T, R, Z](piece: Piece[G, A0, T0, A, T], k: Frames[G, A, B, S, T],
                                                              m: Stack[G, B, S, R, Z]): Closed[G, A0, T0, R, Z] | Null = m match
      case Stack.Delim(tag, out, rest) => closedFrom(Piece.Snoc(piece, k, tag), out, rest)
      case Stack.Bound(tag, out, rest) => closedBy(piece, k, tag, out, rest)
      case Stack.Answered() => atTop[A0, T0, A, S, T, R](piece, k)
      case _ => null

    /** the continuation a closed level holds when it is one segment: put back over an answer boundary */
    private abstract class Segment[A, S0, T, R, Z](k: Frames[G, A, S0, S0, T]) extends Closed[G, A, T, R, Z]:
      def from(a: A): Next[G, T] = next(Return(a), k, Stack.Answered[G, S0, T]())
      private[Delimited] def forced(a: A): T = barrier(forceAt(k, a))
      private[Delimited] def forcedIn(a: A): T = forceAt(k, a)
      def resume[B2, S2, X, Z2](a: A, out: Frames[G, T, B2, S2, X], rest: Stack[G, B2, S2, X, Z2]): Next[G, Z2] =
        next(Return(a), k, Stack.Bound[G, S0, T, B2, S2, X, Z2](null, out, rest))

    private def segmentBy[A, S0, T, R, B2, S2, X, Z](k: Frames[G, A, S0, S0, T], tag: Tag[R] | Null, out: Frames[G, R, B2, S2, X],
                                                     rest: Stack[G, B2, S2, X, Z]): Closed[G, A, T, R, Z] =
      new Segment[A, S0, T, R, Z](k):
        def answer(r: R): Next[G, Z] = next(Return[G, X, R](r), out, rest)
        def instead(c: Freer[G, R, R, R]): Next[G, Z] = next(c, end[R, R], Stack.Bound[G, R, R, B2, S2, X, Z](tag, out, rest))

    private def segmentAtTop[A, S0, T, R](k: Frames[G, A, S0, S0, T]): Closed[G, A, T, R, R] =
      new Segment[A, S0, T, R, R](k):
        def answer(r: R): Next[G, R] = next(Return[G, R, R](r), end[R, R], Stack.Answered[G, R, R]())
        def instead(c: Freer[G, R, R, R]): Next[G, R] = next(c, end[R, R], Stack.Answered[G, R, R]())

    /** the continuation a closed level holds: the piece over its last segment, put back over an answer boundary */
    private abstract class Held[A0, T0, Y, S0, I, R, Z](piece: Piece[G, A0, T0, Y, I], last: Frames[G, Y, S0, S0, I])
      extends Closed[G, A0, T0, R, Z]:
      def from(a: A0): Next[G, T0] = reinstall(piece, Return(a), last, Stack.Answered[G, S0, T0]())
      private[Delimited] def forced(a: A0): T0 = barrier(forcedIn(a))
      private[Delimited] def forcedIn(a: A0): T0 =
        if alone then deeper { val n = from(a); goAlone(n.c, n.k, n.m) }
        else answerOf(deeper { val n = from(a); go(n.c, n.k, n.m) })
      def resume[B2, S2, X, Z2](a: A0, out: Frames[G, T0, B2, S2, X], rest: Stack[G, B2, S2, X, Z2]): Next[G, Z2] =
        reinstall(piece, Return(a), last, Stack.Bound[G, S0, T0, B2, S2, X, Z2](null, out, rest))

    private def closedBy[A0, T0, Y, S0, I, R, B2, S2, X, Z](piece: Piece[G, A0, T0, Y, I], last: Frames[G, Y, S0, S0, I],
                                                            tag: Tag[R] | Null, out: Frames[G, R, B2, S2, X],
                                                            rest: Stack[G, B2, S2, X, Z]): Closed[G, A0, T0, R, Z] =
      new Held[A0, T0, Y, S0, I, R, Z](piece, last):
        def answer(r: R): Next[G, Z] = next(Return[G, X, R](r), out, rest)
        def instead(c: Freer[G, R, R, R]): Next[G, Z] = next(c, end[R, R], Stack.Bound[G, R, R, B2, S2, X, Z](tag, out, rest))

    private def atTop[A0, T0, Y, S0, I, R](piece: Piece[G, A0, T0, Y, I], last: Frames[G, Y, S0, S0, I]): Closed[G, A0, T0, R, R] =
      new Held[A0, T0, Y, S0, I, R, R](piece, last):
        def answer(r: R): Next[G, R] = next(Return[G, R, R](r), end[R, R], Stack.Answered[G, R, R]())
        def instead(c: Freer[G, R, R, R]): Next[G, R] = next(c, end[R, R], Stack.Answered[G, R, R]())
