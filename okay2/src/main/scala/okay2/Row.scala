package okay2

import scala.annotation.implicitNotFound
import scala.reflect.ClassTag

/**
 * A ROW: a type of kind `*` carrying the operations it may perform as
 * a higher-kinded member. A signature is a Row whose `Op` is concrete;
 * `F + G` is a Row whose `Op` is ABSTRACT, so it erases to Object and
 * an `Inject` over a union holds the raw operation with no wrapper and
 * no checkcast — the erasure argument the Scala 3 core makes for its
 * union `[A] =>> F[A] | G[A]`, here made by an abstract type instead.
 *
 * WHY NOT A HIGHER-KINDED ALIAS (measured, specs/okay2.md, stage 0):
 * `type +[F[_], G[_]] = Or[F, G]#Row` is refused by scalac 2.13.18 —
 * "type Row takes type parameters" — with or without `-Xsource:3`, and
 * the type-lambda spelling `({ type L[A] = ... })#L` fails the same way:
 * an alias cannot be given the kind `* -> *` by partial application.
 * `(F + G)#Row` compiles only written out at every use site. A Row of
 * kind `*` needs none of that, and `A ! (State % Int + Console)` is
 * spelled exactly as in Scala 3.
 */
trait Row { type Op[+A] }

/** the union of two rows. `Op` stays abstract on purpose. A union does
 * not commute here — `A + B` and `B + A` are different types — and
 * `Member` has a `right` rule for it, where the Scala 3 union needs
 * nothing. */
sealed trait +[F <: Row, G <: Row] extends Row


/** the empty signature: no operations, so a computation over it is
 * PURE — A ! Pure has nothing to perform. The zero of the union
 * algebra (F + Pure = F, by `Member.pure`) */
sealed trait Pure extends Row { type Op[+A] = Nothing }

/**
 * F is a member of the row R — the witness `at` asks for, compile-time
 * only: an instance is never read, and `coerce` is the one cast it
 * licenses. Sound by the erasure argument above: every operation of F
 * is an operation of R, and the row is not a runtime thing.
 */
@implicitNotFound("${F} is not a member of the row ${R}.\nA program lands in a row that CONTAINS its own: `p.at[R]` needs R to mention every effect p performs.\nA union does not commute in Scala 2, so `A + B` is not `B + A` — .at[B + A] reorders it.")
sealed trait Member[F <: Row, R <: Row]

object Member extends MemberLow {
  /** F on the left of a union */
  implicit def left[F <: Row, G <: Row]: Member[F, F + G] = inst.asInstanceOf[Member[F, F + G]]
  /** F on the right of a union — its own rule, since `+` does not commute */
  implicit def right[F <: Row, G <: Row]: Member[G, F + G] = inst.asInstanceOf[Member[G, F + G]]
  /** F somewhere inside the left side */
  implicit def deeper[F <: Row, G <: Row, H <: Row](implicit ev: Member[F, G]): Member[F, G + H] = {
    val _ = ev; inst.asInstanceOf[Member[F, G + H]]
  }
  /** F somewhere inside the right side */
  implicit def deeperRight[F <: Row, G <: Row, H <: Row](implicit ev: Member[F, H]): Member[F, G + H] = {
    val _ = ev; inst.asInstanceOf[Member[F, G + H]]
  }

  /**
   * THE ONLY CAST of the row discipline. A `Free[F, A]` already IS a
   * `Free[R, A]` whenever F is a member of R, because every operation
   * it holds is an `F#Op[X]` and the row erases; `Free` is invariant in
   * its row, so the type system cannot say so, and this does.
   */
  private[okay2] def coerce[A, F <: Row, R <: Row](p: A ! F): A ! R = p.asInstanceOf[A ! R]
}

trait MemberLow extends MemberLowest {
  implicit def self[F <: Row]: Member[F, F] = inst.asInstanceOf[Member[F, F]]
}

trait MemberLowest {
  /** the one instance; the witness carries no information */
  protected val inst: Member[Pure, Pure] = new Member[Pure, Pure] {}
  /** a program with no operations rides into any row at all */
  implicit def pure[R <: Row]: Member[Pure, R] = inst.asInstanceOf[Member[Pure, R]]
}

/**
 * Every signature of the row R1 is a member of R2 — what `at` asks,
 * so that a program at a WHOLE row (`State[Int] + Produce`) lands in
 * a wider or reordered one (`Produce + State[Int] + Writer[String]`).
 * A union is taken apart member by member; a signature is a `Member`.
 * The Scala 3 core's `In`/`Sub` pair, with the same one cast behind it.
 */
@implicitNotFound("${R1} does not fit in the row ${R2}.\nA program lands in a row that CONTAINS every effect of its own: `p.at[R]` needs R to mention each of them.\nA union does not commute in Scala 2, so `A + B` is not `B + A` — .at[B + A] reorders it.")
sealed trait Sub[R1 <: Row, R2 <: Row]

object Sub extends SubLow {
  /** a union fits when both sides do */
  implicit def union[F <: Row, G <: Row, R <: Row](implicit l: Sub[F, R], r: Sub[G, R]): Sub[F + G, R] = {
    val _ = (l, r); inst.asInstanceOf[Sub[F + G, R]]
  }
}

trait SubLow {
  protected val inst: Sub[Pure, Pure] = new Sub[Pure, Pure] {}
  /** a signature fits when it is a member */
  implicit def one[F <: Row, R <: Row](implicit m: Member[F, R]): Sub[F, R] = {
    val _ = m; inst.asInstanceOf[Sub[F, R]]
  }
}

/** ∀X, the runtime test for F's operations, by the erasure of F —
 * asked by `split` on every operation of every runner */
@implicitNotFound("no TypeableK[${F}].\nSplitting a row needs a runtime test for ${F}'s operations, and a signature declares its own:\n  implicit val effect: Effect[YourOp] = Effect.of[YourOp]\nA ROW needs no instance: the split tests one side and takes the other by exclusion.")
trait TypeableK[F <: Row] { def test(x: Any): Boolean }

object TypeableK {
  /** the empty signature is trivially splittable: nothing inhabits it */
  implicit val pure: TypeableK[Pure] = new TypeableK[Pure] { def test(x: Any): Boolean = false }
}

/**
 * WHAT A SIGNATURE SAYS ABOUT ITSELF — the Scala 2 spelling of
 * `derives Effect`: one implicit in the companion,
 *
 *   implicit val effect: Effect[Console] = Effect.of[Console]
 *
 * `of` reads the class off `F#Op[Any]`'s ClassTag. For a signature
 * whose only parameter is the answer type the test is COMPLETE (the
 * answer is erased anyway, so the class is the whole identity); for a
 * parameterised one (`State % S`, `Throws % E`) it is by class only, so
 * a row may hold ONE of it — two `State % _` misroute, loudly. A ROW
 * has no ClassTag for its abstract `Op`, which is the right refusal:
 * the erasure of a union would be a class every operation matches.
 */
trait Effect[F <: Row] extends TypeableK[F]

object Effect {
  def of[F <: Row](implicit ct: ClassTag[F#Op[Any]]): Effect[F] = byClass[F](ct.runtimeClass)

  /** the class test over a run-time class */
  def byClass[F <: Row](cls: Class[_]): Effect[F] = new Effect[F] {
    def test(x: Any): Boolean = cls.isInstance(x)
  }
}

object Split {
  /**
   * THE trusted kernel: split the union by testing only the F side,
   * taking G by exclusion — a type test on an abstract G would erase to
   * an always-true test. Sound by the excluded middle of the union:
   * a value of `(F + G)#Op[A]` that passes F's test is an `F#Op[A]`,
   * and one that does not is a `G#Op[A]`. Both casts live HERE and
   * nowhere else.
   */
  def split[F <: Row, G <: Row, A, X](e: (F + G)#Op[A])(onF: F#Op[A] => X)(onG: G#Op[A] => X)(implicit T: TypeableK[F]): X =
    if (T.test(e)) onF(e.asInstanceOf[F#Op[A]]) else onG(e.asInstanceOf[G#Op[A]])

  /** the `Either` form, for drains and tests */
  def <|>[F <: Row, G <: Row, A](e: (F + G)#Op[A])(implicit T: TypeableK[F]): Either[F#Op[A], G#Op[A]] =
    split[F, G, A, Either[F#Op[A], G#Op[A]]](e)(Left(_))(Right(_))

  /** rewrite the operations of ONE member of a row in place and leave
   * the others as they are — a prism's modify, over the row */
  def over[F <: Row, R <: Row, A](e: R#Op[A])(f: F#Op[A] => F#Op[A])(implicit T: TypeableK[F]): R#Op[A] =
    if (T.test(e)) f(e.asInstanceOf[F#Op[A]]).asInstanceOf[R#Op[A]] else e
}

/** A comonadic handler interprets each operation by its own value */
@implicitNotFound("no Handler[${F}].\nA Handler answers each operation with a plain value (trait Handler: def handle[A](a: F#Op[A]): A).\nFor a ROW, build the union from the parts: implicit val h: Handler[F + G] = Handler.union[F, G]\n(each part needs its own Handler in scope first).")
trait Handler[F <: Row] { def handle[A](a: F#Op[A]): A }

object Handler {
  /** Pure has no operations left to handle */
  implicit val pure: Handler[Pure] = new Handler[Pure] { def handle[A](a: Nothing): A = a }

  /**
   * Handlers compose along the union: split the operation by the F
   * test and delegate — one handler per effect, one row. An explicit
   * combinator, not an implicit, as in the Scala 3 core.
   *
   * NOT CHECKED HERE, and the Scala 3 core checks it with a macro
   * (`Distinct[R]`): that no signature tested by class appears twice in
   * the row. Two `State % _` in one row misroute at the first wrong
   * answer (a ClassCastException, loud), not silently.
   */
  def union[F <: Row, G <: Row](implicit T: TypeableK[F], hf: Handler[F], hg: Handler[G]): Handler[F + G] =
    new Handler[F + G] {
      def handle[A](a: (F + G)#Op[A]): A = Split.split[F, G, A, A](a)(f => hf.handle(f))(g => hg.handle(g))
    }
}

/**
 * An interpretation of F in the continuation monad, with the answers
 * S: the natural transformation `F ==> ([X] =>> X /> S)` of the Scala 3
 * core, as a trait because Scala 2 has no polymorphic function type.
 * A comonadic handler is one that never captures: `Interpr.of(h)`.
 */
trait Interpr[F <: Row, S] { def apply[X](e: F#Op[X]): Cont[X, S, S] }

object Interpr {
  /** a comonadic Handler at every answer type */
  def of[F <: Row, S](implicit H: Handler[F]): F !> S = new Interpr[F, S] {
    def apply[X](e: F#Op[X]): Cont[X, S, S] = Cont.Pure(H.handle(e))
  }
}

/** an operation answered by a PROGRAM in another row: `translate`'s
 * handler shape, `F ==> ([X] =>> X ! G)` */
trait Interpret[F <: Row, G <: Row] { def apply[X](e: F#Op[X]): X ! G }

/** an answer-polymorphic handler: by parametricity it must resume the
 * continuation exactly once, which is what keeps `relay` a loop */
trait Relay[F <: Row] { def apply[X, Y](e: F#Op[X]): X /> Y }

/**
 * R WITHOUT F — the residual row a handler of F leaves behind, as a
 * type-level function: `Remove[State[Int], State[Int] + Writer[String]
 * + Produce]` has `Out = Writer[String] + Produce`.
 *
 * WHY IT EXISTS. A handler is written for the row `F + G`, F handled
 * and G forwarded. In Scala 3 a union commutes and associates, so any
 * program whose row mentions F unifies with that shape. In Scala 2
 * `A + B + C` is `(A + B) + C`, a different TYPE from `A + (B + C)`,
 * and no handler would unify with a row written the natural way. This
 * witness finds F anywhere in R, at any nesting, and names what is
 * left; `split` is the one cast it licenses, by the same erasure
 * argument as `Member.coerce`. A signature that appears TWICE in a
 * row is refused as ambiguous, which is the check the Scala 3 core's
 * `Distinct` macro makes.
 */
@implicitNotFound("${F} is not in the row ${R}, so there is nothing for its handler to handle here.\nA handler of F runs a program whose row mentions F once: `State.handle(s)(p)` needs p's row to contain State[S].\n(A signature that appears twice in a row is refused too: a class test cannot tell two `State[_]` apart.)")
sealed trait Remove[F <: Row, R <: Row] {
  type Out <: Row
  /** the program at the handler's shape, F at the head — the witness
   * says F is in R, and the row erases */
  private[okay2] def split[A](p: A ! R): A ! (F + Out) = p.asInstanceOf[A ! (F + Out)]
  /** and back: the same claim, the other way */
  private[okay2] def join[A](p: A ! (F + Out)): A ! R = p.asInstanceOf[A ! R]
}

object Remove extends RemoveLow {
  type Aux[F <: Row, R <: Row, O <: Row] = Remove[F, R] { type Out = O }
  private[okay2] def inst[F <: Row, R <: Row, O <: Row]: Aux[F, R, O] = new Remove[F, R] { type Out = O }

  /** F at the head */
  implicit def head[F <: Row, G <: Row]: Aux[F, F + G, G] = inst
  /** F at the tail */
  implicit def last[F <: Row, G <: Row]: Aux[F, G + F, G] = inst
}

trait RemoveLow extends RemoveLowest {
  /** F inside the left side */
  implicit def deeper[F <: Row, G <: Row, H <: Row, O <: Row](implicit r: Remove.Aux[F, G, O]): Remove.Aux[F, G + H, O + H] = {
    val _ = r; Remove.inst
  }
  /** F inside the right side */
  implicit def deeperRight[F <: Row, G <: Row, H <: Row, O <: Row](implicit r: Remove.Aux[F, H, O]): Remove.Aux[F, G + H, G + O] = {
    val _ = r; Remove.inst
  }
}

trait RemoveLowest {
  /** the row IS the signature: nothing is left */
  implicit def self[F <: Row]: Remove.Aux[F, F, Pure] = Remove.inst
}
