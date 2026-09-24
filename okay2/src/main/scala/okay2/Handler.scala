package okay2

/*
 * What a signature says about itself and how a row is taken apart and
 * answered: `TypeableK`/`Effect` (the runtime test), `Split` (the one
 * trusted cast), `Handler` (the comonadic answer), and the handler
 * shapes `Interpr`, `Interpret`, `Relay` — the Scala 3 core's
 * Handler.scala. The row itself is in Row.scala.
 */

import scala.annotation.implicitNotFound
import scala.reflect.ClassTag

/** ∀X, the runtime test for F's operations, by the erasure of F —
 * asked by `split` on every operation of every runner */
@implicitNotFound("no TypeableK[${F}].\nSplitting a row needs a runtime test for ${F}'s operations, and a signature declares its own:\n  implicit val effect: Effect[YourOp] = Effect.of[YourOp]\nA ROW needs no instance: the split tests one signature and takes the rest by exclusion.")
trait TypeableK[F <: Row] { def test(x: Any): Boolean }

object TypeableK {
  /** the empty signature is trivially splittable: nothing inhabits it */
  implicit val pure: TypeableK[Pure] = new TypeableK[Pure] { def test(x: Any): Boolean = false }

  /**
   * A test that reads the operation's VALUE, finer than the class — the
   * declaration `Distinct` reads to let two signatures of one class share
   * a row (the Scala 3 core's `TypeableK.ByValue`). No macro can read
   * what a hand-written test does, so the instance says it; unmarked
   * means by class, the safe direction: an unmarked fine test is refused
   * and fixed by one word, the reverse would pass a row that misroutes.
   * `Writer.byValue.writerK` is the one instance here that carries it.
   */
  trait ByValue[F <: Row] extends TypeableK[F]
}

/**
 * WHAT A SIGNATURE SAYS ABOUT ITSELF — the Scala 2 spelling of
 * `derives Effect`: one implicit in the companion,
 *
 *   implicit val effect: Effect[Console] = Effect.of[Console]
 *
 * `of` reads the class off `F#Op[Any]`'s ClassTag. For a signature
 * whose only parameter is the answer type the test is COMPLETE; for a
 * parameterised one (`State[S]`, `Throws[E]`) it is by class only, so
 * a row may hold ONE of it, and `Distinct` refuses two. Two instances
 * of one signature go under a key (`Tag`), a run-time handle
 * (`Instances`), or — for Writer — the finer test `Writer.byValue`.
 *
 * NEVER GIVE IT A ROW: an intersection's `#Op` is its last parent's, so
 * `Effect.of[State[Int] + Writer[String]]` would test for Writer's
 * class alone. Nothing asks for one — `TypeableK` is invariant, and a
 * companion's `effect` answers its own signature only.
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
   * THE trusted kernel: split an operation of the row `F + G` by
   * testing only F, taking the rest by exclusion. The F side is handed
   * over TYPED at F's own `Op` — sound because it passed F's class
   * test; the rest stays `Any`, because the rest may be a row and a
   * row's `#Op` is not a type to read at (see Row). The one cast lives
   * HERE. `G` is only there to say what the rest is.
   */
  def split[F <: Row, G <: Row, A, X](e: Any)(onF: F#Op[A] => X)(onG: Any => X)(implicit T: TypeableK[F]): X =
    if (T.test(e)) onF(e.asInstanceOf[F#Op[A]]) else onG(e)

  /** `split` for a row of exactly TWO signatures, both typed: the rest
   * is the single signature G, so its operations are `G#Op`s. The
   * caller's claim is that G is ONE signature — never a row of several
   * (an intersection's `#Op` is its last parent's, above) */
  def splitBoth[F <: Row, G <: Row, A, X](e: Any)(onF: F#Op[A] => X)(onG: G#Op[A] => X)(implicit T: TypeableK[F]): X =
    if (T.test(e)) onF(e.asInstanceOf[F#Op[A]]) else onG(e.asInstanceOf[G#Op[A]])

  /** the operation of a program whose row is ONE signature F, typed at
   * F: the row admits nothing else, so it IS an `F#Op`. Never at a row
   * of several — an intersection's `#Op` is its last parent's (above) */
  def only[F <: Row, A](e: Any): F#Op[A] = e.asInstanceOf[F#Op[A]]

  /** the `Either` form, for drains and tests */
  def <|>[F <: Row, A](e: Any)(implicit T: TypeableK[F]): Either[F#Op[A], Any] =
    split[F, Pure, A, Either[F#Op[A], Any]](e)(Left(_))(Right(_))

  /** rewrite the operations of ONE member of a row in place and leave
   * the others as they are — a prism's modify, over the row */
  def over[F <: Row, A](e: Any)(f: F#Op[A] => F#Op[A])(implicit T: TypeableK[F]): Any =
    if (T.test(e)) f(e.asInstanceOf[F#Op[A]]) else e
}

/**
 * A comonadic handler interprets each operation by its own value. It
 * takes the operation as `Any` because a row's `#Op` is not a type to
 * read at (see Row); a single signature's handler is written typed:
 * `new Handler[F] { def handle[A](a: F#Op[A]): A }`, as okay writes
 * it. `Handler.Of` is the same class under its stage-8 name.
 *
 * INVARIANT, on purpose: covariant, `Handler[F + G] <: Handler[G]`, and
 * the documented `implicit val h: Handler[F + G] = Handler.union[F, G]`
 * resolved its own `Handler[G]` argument to `h` itself (measured,
 * TestEffects, stage 8).
 */
@implicitNotFound("no Handler[${F}].\nA Handler answers each operation with a plain value:\n  new Handler[YourOp] { def handle[A](a: YourOp#Op[A]): A = ... }\nFor a ROW, build the union from the parts: implicit val h: Handler[F + G] = Handler.union[F, G]\n(each part needs its own Handler in scope first).")
trait Handler[F <: Row] {
  /** one operation of the signature F, typed — what a user writes, as
   * okay's `new Handler[F] { def handle[A](a: F[A]): A }` */
  def handle[A](a: In[A]): A

  /** `F#Op`, named once here: `handle` is declared at this name so a
   * union's handler can implement it — `(F + G)#Op` is not a type Scala
   * 2 lets a subclass write (a selection from a volatile type). For a
   * signature it IS `F#Op`, so `def handle[A](a: Console.Op[A])`
   * implements it as written */
  type In[A] = F#Op[A]

  /**
   * the same operation as the runner holds it, `Any` (a row's `#Op` is
   * not a type to read at). The default narrows to `F#Op` — sound
   * because a Handler[F] is only ever run on a program whose row is F,
   * or given F's operations by a union's split; a union overrides it.
   */
  def handleOp[A](op: Any): A = handle(op.asInstanceOf[In[A]])
}

object Handler {
  /**
   * one signature's handler, typed. The cast in `handleOp` is sound
   * because a Handler[F] is only ever run on a program whose row is F
   * (or a union whose split sent it F's operations): the row admits no
   * other operation to it.
   */
  abstract class Of[F <: Row] extends Handler[F]

  /** Pure has no operations left to handle */
  implicit val pure: Handler[Pure] = new Handler[Pure] {
    def handle[A](a: In[A]): A = handleOp[A](a)
    override def handleOp[A](op: Any): A = throw new IllegalStateException("an operation in a Pure program: " + op)
  }

  /** a row's operations as a one-hole type */
  type OpOf[F <: Row] = { type L[A] = F#Op[A] }

  /** a row whose operations form a COMONAD is handled by `extract` —
   * the Scala 3 core's `ComonadHandler` and the given built from it.
   * Found implicitly: it fires only where somebody has declared the
   * comonad, which is the declaration that the operation IS its answer
   * in a context */
  final class ComonadHandler[F <: Row](val C: Comonad[OpOf[F]#L]) extends Handler[F] {
    def handle[A](a: F#Op[A]): A = C.extract(a)
  }

  implicit def comonad[F <: Row](implicit C: Comonad[OpOf[F]#L]): Handler[F] = new ComonadHandler[F](C)

  /**
   * Handlers compose along the union: split the operation by the F
   * test and delegate — one handler per effect, one row. An explicit
   * combinator, not an implicit, as in the Scala 3 core.
   *
   * `Distinct[F + G]` refuses a row that holds two signatures of one
   * class (`Ask[Int] + Ask[String]`), which the split below could not
   * tell apart — checked at compile time, as the Scala 3 core does.
   */
  def union[F <: Row, G <: Row](implicit T: TypeableK[F], hf: Handler[F], hg: Handler[G], d: Distinct[F + G]): Handler[F + G] = {
    val _ = d
    new Handler[F + G] {
      // a row's `#Op` is its last parent's, so the typed door forwards
      // to the untyped one, which splits by class
      def handle[A](a: In[A]): A = handleOp[A](a)
      override def handleOp[A](op: Any): A = if (T.test(op)) hf.handleOp[A](op) else hg.handleOp[A](op)
    }
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
    def apply[X](e: F#Op[X]): Cont[X, S, S] = Cont.Pure(H.handleOp[X](e))
  }
}

/** an operation answered by a PROGRAM in another row: `translate`'s
 * handler shape, `F ==> ([X] =>> X ! G)` */
trait Interpret[F <: Row, G <: Row] { def apply[X](e: F#Op[X]): X ! G }

/** an answer-polymorphic handler: by parametricity it must resume the
 * continuation exactly once, which is what keeps `relay` a loop */
trait Relay[F <: Row] { def apply[X, Y](e: F#Op[X]): X /> Y }
