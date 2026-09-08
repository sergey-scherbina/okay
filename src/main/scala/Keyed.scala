package okay

import scala.annotation.tailrec
import okay.!.*

/**
 * STATE UNDER A KEY, so a row may hold as many as it has keys.
 *
 * `State % S` is told apart by its class, and `Get()` carries no
 * trace of S — so a row holds ONE of it, and two at different S
 * misroute (TestRowIdentity says so and demonstrates it). That is a
 * property of the OPERATION, not of rows: the split is a runtime
 * test, and a signature that carries its identity at run time is told
 * apart. `Writer` gets this for free, its operation being the value
 * told; here the operation carries a key, and the test compares it.
 *
 *     type Count = Keyed.At["count", Int]
 *     type Name  = Keyed.At["name", String]
 *
 *     val p: Int ! (Count + Name) =
 *       for
 *         n <- Keyed.get["count", Int].plus[Name]
 *         _ <- Keyed.put["name", String]("ada").at[Count + Name]
 *       yield n
 *
 * The key is a SINGLETON type, so the row's type lists which states
 * the program has — the same trade `HMap` makes against `TMap`, and
 * for the same reason: everything is known statically, and nothing
 * casts except the one test below. Where the cells are made at run
 * time and cannot be named in a type, that is `Cells`.
 */
enum Keyed[K, S, +A]:
  case Get(k: K) extends Keyed[K, S, S]
  case Put(k: K, s: S) extends Keyed[K, S, S]

object Keyed:

  /** one state, named — a row member */
  type At[K, S] = [A] =>> Keyed[K, S, A]

  /**
   * The test is BY KEY, which is the whole point: `derives Effect`
   * would test by class and put every keyed state in one bucket.
   *
   * The cast is `typeableK`'s own, for the same reason — the answer
   * type is erased, so once the class and the key agree there is
   * nothing left to check.
   */
  given at[K, S](using k: ValueOf[K]): okay.Effect[At[K, S]] = okay.Effect.of(new:
    def unapply[A](x: Any): Option[x.type & Keyed[K, S, A]] = x match
      case op: Keyed[?, ?, ?] =>
        val mine = op match
          case Get(kk) => kk == k.value
          case Put(kk, _) => kk == k.value
        if mine then Some(x.asInstanceOf[x.type & Keyed[K, S, A]]) else None
      case _ => None)

  inline def get[K, S](using k: ValueOf[K]): S ! At[K, S] =
    effect(Get[K, S, S](k.value))

  inline def put[K, S](s: S)(using k: ValueOf[K]): S ! At[K, S] =
    effect(Put[K, S, S](k.value, s))

  /** the handler: `State.handle`'s loop, threading the state through
   * itself, with the key doing the splitting */
  def handle[K, S, A, F[+_]](init: S)(p: A ! (At[K, S] + F))
                            (using TypeableK[At[K, S]]): (S, A) ! F =
    def _loop(s: S)(x: A ! (At[K, S] + F)): (S, A) ! F = loop(s)(x)

    @tailrec def loop(s: S)(x: A ! (At[K, S] + F)): (S, A) ! F = (x.resume: @unchecked) match
      case Pure(a) => Pure((s, a))
      case Effect(e) => <|>[At[K, S], F](e) match
        case Left(Get(_)) => Pure((s, s))
        case Left(Put(_, s)) => Pure((s, s))
        case Right(e) => Effect(e).map((s, _))
      case Bind(Effect(e), k) => <|>[At[K, S], F](e) match
        case Left(Get(_)) => loop(s)(k(s))
        case Left(Put(_, s)) => loop(s)(k(s))
        case Right(e) => Effect(e).flatMap(x => _loop(s)(k(x)))

    loop(init)(p)

  /** run from an initial state to (final state, value) */
  inline def run[K, S, A](init: S)(p: A ! At[K, S])
                         (using TypeableK[At[K, S]]): (S, A) =
    !.run(handle[K, S, A, okay.Pure](init)(p))
