package okay

import scala.annotation.tailrec

/**
 * Call-by-need for programs, as an EFFECT (direct-once, 2026-09-16).
 *
 * `Delay` is by-name: the loop forces its thunk every time it reaches
 * the node, and a shared node reached twice runs twice — Scala's
 * by-name parameter, not its `lazy val`. By-need is by-name plus one
 * cell that remembers the answer, and for a program that cell is not
 * an optimisation but a SEMANTICS: "run these effects at most once"
 * is observable. So the cell is not hidden in the tree (a mutable
 * field would make the same program answer differently on its second
 * run, and a handler that replays would replay a stale value) — it is
 * an effect, and the cells live in the handler as state threaded
 * through its own loop, exactly as `State.handle` threads `S`.
 *
 * Two operations, one handle:
 *
 *   Force(h)    : Option[A]   what the cell holds, or None — and on None
 *                             the handler marks the cell RUNNING
 *   Store(h, a) : A           fill the cell; answers what the cell holds
 *                             AFTER, so the first answer stored wins and
 *                             every observer sees the same value
 *
 * The handle carries no program — that is what keeps the effect's
 * type free of the row it lives in, so it is written like every other
 * effect: `Int ! Once + Writer % String`. The program stays with the
 * handle's creator (`once`, below): a handle is an identity, and "once"
 * is counted per handle, which is per `!.once(p)` evaluated — the way
 * a `lazy val` is per declaration, not per right-hand side.
 *
 * MULTI-SHOT is decided by handler ORDER, not by a flag the macro
 * could not check. The cells are the handler's state, so under a
 * search they behave exactly as State does:
 *
 *   runChoice(Once.run(p))   the cells backtrack with the search: each
 *                            branch has its own "once", nothing leaks
 *                            between branches — the default reading
 *   Once.run(runChoice(p))   one store for the whole search: the second
 *                            branch sees what the first stored — the
 *                            deliberate reading, and the types show it
 *
 * A handle demanded while its own program is still running (a knot;
 * or an interleaved search with `Once.run` OUTSIDE it, whose second
 * branch demands a cell the first has not yet filled) is a loud
 * `IllegalStateException`, not a second run and not a hang.
 *
 * `Logic.once` is a different word — the Prolog cut, "the first
 * answer" — and keeps its namespace; this is `!.once` and `Once`.
 */
enum Once[+A] derives Effect:
  /** what the cell holds, or None (which marks it running) */
  case Force[A](h: Once.Handle[A]) extends Once[Option[A]]
  /** fill the cell; answers what it holds after — the first store wins */
  case Store[A](h: Once.Handle[A], a: A) extends Once[A]

object Once:

  /** the cell's identity: compared by reference, holds nothing */
  final class Handle[A]

  /**
   * p, run at most once: the first demand runs it and stores the
   * answer under a fresh handle, every later demand of THIS value
   * answers from the store. `Free.delay` keeps p unbuilt until that
   * first demand — construction must do no work.
   *
   * Two calls make two handles: `once(p).flatMap(_ => once(p))` runs p
   * twice. Share the VALUE to share the cell.
   */
  def once[A, F[+_]](p: => A ! Once + F): A ! Once + F =
    at[A, Once + F](new Handle[A])(h => effect(Force(h)))((h, a) => effect(Store(h, a)))(() => p)

  /**
   * The same over the WHOLE row R, with the two operations already
   * injected by the caller — what the `direct` macro emits for a
   * `lazy val` with a mark, since it holds the block's row as one type
   * and has no need to split `Once` out of it. `once` is this at
   * `Once + F`.
   */
  def at[A, R[+_]](h: Handle[A])
                  (force: Handle[A] => Option[A] ! R)
                  (store: (Handle[A], A) => A ! R)
                  (p: () => A ! R): A ! R =
    // an EXPLICIT thunk, not a by-name parameter: the `direct` macro
    // builds this call, and a by-name argument's thunk is synthesized
    // by the compiler AFTER the macro, leaving any definition inside
    // the program (an inline call's proxy val, say) owned by the
    // enclosing method rather than by the thunk — `Could not find proxy
    // for val a$proxy1` out of LambdaLift (direct-colourless-val,
    // 2026-09-16). The macro builds the lambda under the right owner,
    // exactly as it does for `Free.delay`.
    force(h).flatMap:
      case Some(a) => pure(a)
      case None => Free.delay(p).flatMap(a => store(h, a))

  /** a cell whose program is in flight */
  private object Running

  /** the cells, threaded through the handler's loop */
  private type Cells = Map[Handle[?], Any]

  /**
   * The one cast, isolated: a value under `h` was put there by
   * `Store[A](h: Handle[A], a: A)`, the only writer, and `h`'s type
   * parameter IS that A. The map is heterogeneous by construction and
   * the type system has no dependent map to say so.
   */
  private def stored[A](v: Any): A = v.asInstanceOf[A]

  /** one operation against the cells: the cells after, and the answer */
  private def step[X](c: Cells, o: Once[X]): (Cells, X) = o match
    case Force(h) => c.get(h) match
      case None => (c + (h -> Running), None)
      case Some(Running) => throw new IllegalStateException(
        "Once: a handle was demanded while its own program is still running — a knot " +
          "(the program demands itself), or an interleaved search with Once.run OUTSIDE " +
          "it; put Once.run inside the search, or break the cycle")
      case Some(v) => (c, Some(stored(v)))
    case Store(h, a) => c.get(h) match
      case None | Some(Running) => (c + (h -> a), a)
      case Some(v) => (c, stored(v))

  /**
   * the handler: a bespoke tail-recursive loop threading the cells,
   * like `State.handle` — a relay's ∀Y shape has nowhere to hold
   * them, and a mutable map would make the residual tree unreplayable.
   * A forwarded F-effect suspends with the cells captured immutably.
   */
  def run[A, F[+_]](a: A ! Once + F): A ! F =
    import !.*
    def again(c: Cells)(x: A ! Once + F): A ! F = loop(c)(x)
    @tailrec def loop(c: Cells)(x: A ! Once + F): A ! F = (x.resume: @unchecked) match
      case Return(v) => Return(v)
      case Inject(e) => split[Once, F](e)
        (o => Return(step(c, o)._2): A ! F)
        (g => Inject(g))
      case Bind(Inject(e), k) => split[Once, F](e)
        (o => { val (c2, x) = step(c, o); loop(c2)(k(x)) })
        (g => Inject(g).flatMap(x => again(c)(k(x))))
    loop(Map.empty)(a)
