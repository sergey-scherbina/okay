package okay.freer



import okay.std.*
import okay.std.given
import scala.annotation.tailrec
import ProbeFreerStep.Freer
import ProbeFreerStep.Freer.{Return, Op, Bind}

/**
 * McBRIDE'S READING ON AN INVARIANT BASE (freer-mcbride-probe,
 * 2026-09-30). TestFreerPara pins that the library's `Freer[G, S, +R,
 * +A]` refuses a handler that CONSUMES its index — the state before in
 * `R`, the state after in `S`, a loop threading a VALUE of `R`. The
 * question this probe answers: is that the variance and nothing else?
 * ProbeFreerStep keeps an invariant copy of the enum (value first,
 * `Freer[G, A, S, R]`), so the same signature and the same loop are
 * written against it here, with two things the covariant base cannot
 * have:
 *
 *  - NO CONTINUATION OBJECT. The loop takes the state and answers the
 *    pair, as `State.handle` does for the fixed-type state: `Return`
 *    gives `S = R` by the GADT, so `(r, a)` IS the `(S, A)` owed; `Get`
 *    gives `X = T = R`, so the continuation takes the state held. No
 *    `k`, no `Reentry`, no room counted — the whole of Cps's stack
 *    machinery exists because a shift body calls `k`, and nothing here
 *    calls anything.
 *  - `@tailrec` WITH THE TYPE ARGUMENTS CHANGING PER CALL. Every
 *    recursive call is at a different index (`k(r)` after a `Put`
 *    starts at `T`, not `R`); Scala 2 refused that ("called
 *    recursively with different type arguments"), and whether Scala 3
 *    accepts it decides whether the McBride loop is the fast shape or
 *    needs an erased inner loop.
 *
 * Kept compiling, like the other probes: the next base or compiler
 * change says by turning red whether these shapes still type.
 */
object ProbeMcBride:

  /** typestate as a consumed index: `X` the value, `S` the state after,
   * `R` the state before — the invariant enum's own order */
  enum St[X, S, R]:
    case Get[S]() extends St[S, S, S]
    case Put[S, T](t: T) extends St[Unit, T, S]

  def get[S]: Freer[St, S, S, S] = Op(St.Get())
  def put[S, T](t: T): Freer[St, Unit, T, S] = Op(St.Put(t))

  /** the threading loop: `State.handle`'s shape with the type moving */
  @tailrec def run[A, S, R](p: Freer[St, A, S, R])(r: R): (S, A) =
    (p.resume: @unchecked) match
      case Return(a) => (r, a)
      case Op(St.Get()) => (r, r)
      case Op(St.Put(t)) => (t, ())
      case Bind(Op(St.Get()), k) => run(k(r))(r)
      case Bind(Op(St.Put(t)), k) => run(k(()))(t)

  /** Int -> String -> List[String], answering the last state and a value
   * computed across all three */
  def answers: (List[String], Int) =
    val p: Freer[St, Int, List[String], Int] =
      get[Int].flatMap(n =>
        put[Int, String](n.toString).flatMap(_ =>
          get[String].flatMap(s =>
            put[String, List[String]](List(s, s)).flatMap(_ =>
              Return(n + s.length)))))
    run(p)(21)

  /** the price of the reading, in the compiler's words: a `Put` whose
   * index does not meet the continuation's, and a run from a state of
   * the wrong type, both refused — see TestFreerPara's pin */
  def refusedPut: String = scala.compiletime.testing.typeCheckErrors(
    "get[Int].flatMap(n => put[String, Int](n))").map(_.message).mkString
  def refusedRun: String = scala.compiletime.testing.typeCheckErrors(
    "run(get[Int])(\"not an Int\")").map(_.message).mkString
