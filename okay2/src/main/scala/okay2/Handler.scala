package okay2

/*
 * What a signature says about itself and how a row is taken apart and
 * answered: `TypeableK`/`Effect` (the runtime test), `Split` (the one
 * trusted cast), `Answers` (the comonadic answer), the handler
 * shapes `Interpr`, `Interpret`, `Relay`, and `Handler`, the level-1
 * handler value with its author's forms — the Scala 3 core's
 * Handler.scala. The row itself is in Row.scala.
 */

import scala.annotation.{implicitNotFound, tailrec, unused}
import scala.language.experimental.macros
import scala.reflect.ClassTag
import scala.reflect.macros.{blackbox, whitebox}
import Free.{Return, Inject, Bind}

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

  /**
   * `split` as a PATTERN, for a handler loop (okay2-handler-allocs): made
   * once per run, `val Mine = Split.at[State[S]]`, then
   * `case Bind(Inject(Mine(op)), k) =>` in the loop's own match. It is
   * name-based (`isEmpty`/`get` on a value class), so a match allocates
   * nothing, and the arm is the loop's own tail call — where `split`
   * handed the step back through two closures, a `Tuple2` and an
   * `Either` per operation (+176 B per State get/set pair, okay2-bench).
   * The cast is `split`'s, here, after the same class test.
   */
  def at[F <: Row](implicit T: TypeableK[F]): At[F] = new At[F](T)

  final class At[F <: Row] private[Split] (T: TypeableK[F]) {
    def unapply(e: Any): AtMatch[F] = new AtMatch[F](if (T.test(e)) e else AtMatch.None)
  }

  final class AtMatch[F <: Row](private val e: Any) extends AnyVal {
    def isEmpty: Boolean = e.asInstanceOf[AnyRef] eq AtMatch.None
    def get: F#Op[Any] = e.asInstanceOf[F#Op[Any]]
  }

  object AtMatch {
    /** the miss: a private sentinel no operation can be */
    private[Split] val None: AnyRef = new Object
  }
}

/**
 * A comonadic handler interprets each operation by its own value. It
 * takes the operation as `Any` because a row's `#Op` is not a type to
 * read at (see Row); a single signature's handler is written typed:
 * `new Answers[F] { def handle[A](a: F#Op[A]): A }`, as okay writes
 * it. `Answers.Of` is the same class under its stage-8 name.
 *
 * INVARIANT, on purpose: covariant, `Answers[F + G] <: Answers[G]`, and
 * the documented `implicit val h: Answers[F + G] = Answers.union[F, G]`
 * resolved its own `Answers[G]` argument to `h` itself (measured,
 * TestEffects, stage 8).
 */
@implicitNotFound("no Answers[${F}].\nAn Answers answers each operation with a plain value:\n  new Answers[YourOp] { def handle[A](a: YourOp#Op[A]): A = ... }\nFor a ROW, build the union from the parts: implicit val h: Answers[F + G] = Answers.union[F, G]\n(each part needs its own Answers in scope first).")
trait Answers[F <: Row] {
  /** one operation of the signature F, typed — what a user writes, as
   * okay's `new Answers[F] { def handle[A](a: F[A]): A }` */
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
   * because a Answers[F] is only ever run on a program whose row is F,
   * or given F's operations by a union's split; a union overrides it.
   */
  def handleOp[A](op: Any): A = handle(op.asInstanceOf[In[A]])
}

object Answers {
  /**
   * one signature's handler, typed. The cast in `handleOp` is sound
   * because a Answers[F] is only ever run on a program whose row is F
   * (or a union whose split sent it F's operations): the row admits no
   * other operation to it.
   */
  abstract class Of[F <: Row] extends Answers[F]

  /** Pure has no operations left to handle */
  implicit val pure: Answers[Pure] = new Answers[Pure] {
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
  final class ComonadHandler[F <: Row](val C: Comonad[OpOf[F]#L]) extends Answers[F] {
    def handle[A](a: F#Op[A]): A = C.extract(a)
  }

  implicit def comonad[F <: Row](implicit C: Comonad[OpOf[F]#L]): Answers[F] = new ComonadHandler[F](C)

  /**
   * Handlers compose along the union: split the operation by the F
   * test and delegate — one handler per effect, one row. An explicit
   * combinator, not an implicit, as in the Scala 3 core.
   *
   * `Distinct[F + G]` refuses a row that holds two signatures of one
   * class (`Ask[Int] + Ask[String]`), which the split below could not
   * tell apart — checked at compile time, as the Scala 3 core does.
   */
  def union[F <: Row, G <: Row](implicit T: TypeableK[F], hf: Answers[F], hg: Answers[G], d: Distinct[F + G]): Answers[F + G] = {
    val _ = d
    new Answers[F + G] {
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
  /** a comonadic Answers at every answer type */
  def of[F <: Row, S](implicit H: Answers[F]): F !> S = new Interpr[F, S] {
    def apply[X](e: F#Op[X]): Cont[X, S, S] = Cont.Pure(H.handleOp[X](e))
  }
}

/** an operation answered by a PROGRAM in another row: `translate`'s
 * handler shape, `F ==> ([X] =>> X ! G)` */
trait Interpret[F <: Row, G <: Row] { def apply[X](e: F#Op[X]): X ! G }

/** an answer-polymorphic handler: by parametricity it must resume the
 * continuation exactly once, which is what keeps `relay` a loop */
trait Relay[F <: Row] { def apply[X, Y](e: F#Op[X]): X /> Y }

/**
 * A handler (Plotkin & Pretnar's sense, level 1, specs/api-levels.md): a VALUE that takes the effect `E` off
 * any program's row and answers `O[A]` — `p.handle(State(5))`, `p.handle(State(5), Throws.either[String]).run`.
 * `Handler[E, O]` (the package object's alias) is the usual one: any answer, nothing needed of the rest of the
 * row. `Full` bounds the answer by `I` and needs `Needs[F]` of the rest `F` (`Reset[R]`: the answer is `R`,
 * the rest's `Nesting`). An answer per operation and no more is `Answers[F]`. The Scala 3 core's twin; Scala 2
 * has no type lambdas, so an answer shape is a projection: `Handler.Pair[S]#L`, `Handler.Or[E]#L`.
 */
object Handler {
  /** every handler value: what `p.handle(h)` takes, its effect read off `Full` by the macro */
  trait Value

  /** the handler in full: the answers it takes (`I`) and what it needs of the rest of the row (`Needs`) */
  trait Full[E <: Row, I, O[_], Needs[_ <: Row]] extends Value {
    def run[A, F <: Row](p: Free[E with F, A])(implicit a: A <:< I, d: Distinct[E with F], n: Needs[F]): O[A] ! F
  }

  /** the evidence of nothing: always there */
  final class Nothing[F <: Row] private[Handler] ()
  object Nothing {
    implicit def any[F <: Row]: Nothing[F] = new Nothing[F]()
  }

  // THE ANSWER SHAPES, as projections (Scala 2's type lambdas)
  type Id[A] = A
  type Pair[S] = { type L[A] = (S, A) }
  type Or[E] = { type L[A] = Either[E, A] }
  type Const[R] = { type L[A] = R }
  /** what `into` needs of the rest of the row: that it holds `G` — an intersection holding G is below it */
  type Holds[G <: Row] = { type L[R <: Row] = R <:< G }

  // THE AUTHOR'S DOOR (level 2, specs/handler-forms.md): four forms by power. Scala 2 has no polymorphic
  // functions, so a clause is a trait with a polymorphic `apply`, written `new … { def apply[X](…) = … }`.

  /** 2 · a state threaded through the operations: `(s, op) => (s', answer)` */
  trait StateClause[F <: Row, S] { def apply[X](s: S, e: F#Op[X]): (S, X) }

  /** 4 · the values' answer, at every value type */
  trait Ret[O[_]] { def apply[A](a: A): O[A] }

  /** 4 · an operation with the rest of the program as `k`: resume it, many times, or not at all */
  trait Control[F <: Row, O[_]] { def apply[X, A, G <: Row](e: F#Op[X], k: X => O[A] ! G): O[A] ! G }

  /**
   * The effect named once, and its forms with nothing more to write: `Handler[Accounts] { case Find(id) => … }`
   * is the default (form 1), `Handler[Accounts].state(0) { case (n, Find(id)) => … }` form 2,
   * `.into[G] { case … }` form 3; each case checked against its operation's answer by a macro (HandlerCases).
   * `.poly(clause)` takes the trait instead. Helpers for type inference only; each form has its one
   * implementation below.
   */
  def apply[F <: Row]: For[F] = new For[F]

  final class For[F <: Row] private[Handler] () {
    /** the default form, 1: `Handler[Accounts] { case Find(id) => … }` */
    def apply(cases: F#Op[Any] => Any): Handler[F, Id] = macro HandlerCases.answer
    /** form 1: `.answer { case … }`, `.answer.poly(answers)` */
    def answer: Answering[F] = new Answering[F]
    /** form 2, `S` read off `init`: `.state(s0) { case (s, op) => … }`, `.state(s0).poly(clause)` */
    def state[S](init: S): Stating[F, S] = new Stating[F, S](init)
    /** form 3: `.into[G] { case … }`, `.into[G].poly(interpret)` */
    def into[G <: Row]: Into[F, G] = new Into[F, G]
    /** `Handler.control[F, O](ret)(c)` */
    def control[O[_]](ret: Ret[O])(c: Control[F, O])(implicit T: TypeableK[F]): Handler[F, O] = Handler.control[F, O](ret)(c)
  }

  final class Answering[F <: Row] private[Handler] () {
    /** cases, each checked at compile time against its operation's answer type */
    def apply(cases: F#Op[Any] => Any): Handler[F, Id] = macro HandlerCases.answer
    /** an `Answers[F]`, typed by the compiler with no macro */
    def poly(a: Answers[F])(implicit T: TypeableK[F]): Handler[F, Id] = Handler.answer[F](a)
  }

  final class Stating[F <: Row, S] private[Handler] (val init: S) {
    /** cases over `(state, operation)`, each answering `(state', the operation's answer)`, checked at compile time */
    def apply(cases: (S, F#Op[Any]) => (S, Any)): Handler[F, Pair[S]#L] = macro HandlerCases.state
    /** a `StateClause[F, S]`, typed by the compiler with no macro */
    def poly(c: StateClause[F, S])(implicit T: TypeableK[F]): Handler[F, Pair[S]#L] = Handler.state[F, S](init)(c)
  }

  final class Into[F <: Row, G <: Row] private[Handler] () {
    /** cases, each a program in `G` whose value is checked against its operation's answer at compile time */
    def apply(cases: F#Op[Any] => Free[G, Any]): Full[F, Any, Id, Holds[G]#L] = macro HandlerCases.into
    /** an `Interpret[F, G]`, typed by the compiler with no macro */
    def poly(i: Interpret[F, G])(implicit T: TypeableK[F]): Full[F, Any, Id, Holds[G]#L] = Handler.into[F, G](i)
  }

  /**
   * What the case forms expand to, after `HandlerCases` has checked every case: each case answers what its
   * operation's constructor declares, read off the declaration. Public because the expansion is at the user's
   * call site; call it through the forms, which check.
   */
  object Cases {
    def answer[F <: Row](cases: F#Op[Any] => Any)(implicit T: TypeableK[F]): Handler[F, Id] =
      Handler.answer[F](new Answers[F] { def handle[X](e: F#Op[X]): X = claim[X](cases(e)) })

    def state[F <: Row, S](init: S)(cases: (S, F#Op[Any]) => (S, Any))(implicit T: TypeableK[F]): Handler[F, Pair[S]#L] =
      Handler.state[F, S](init)(new StateClause[F, S] {
        def apply[X](s: S, e: F#Op[X]): (S, X) = { val (s2, a) = cases(s, e); (s2, claim[X](a)) }
      })

    def into[F <: Row, G <: Row](cases: F#Op[Any] => Free[G, Any])(implicit T: TypeableK[F]): Full[F, Any, Id, Holds[G]#L] =
      Handler.into[F, G](new Interpret[F, G] { def apply[X](e: F#Op[X]): X ! G = claimProgram[G, X](cases(e)) })

    // THE CASTS the macro's check licenses: the case that took `e` answers what `e`'s constructor declares, which
    // is `e`'s own answer type `X`; Scala 2 does not refine `X` from the constructor in a match, the check did
    private def claim[X](a: Any): X = a.asInstanceOf[X]
    private def claimProgram[G <: Row, X](p: Free[G, Any]): X ! G = p.asInstanceOf[X ! G]
  }

  /** 1 · answer each operation with a value, and the program goes on (`!.relay`) */
  def answer[F <: Row](a: Answers[F])(implicit T: TypeableK[F]): Handler[F, Id] = new Full[F, Any, Id, Nothing] {
    private val g: Relay[F] = new Relay[F] { def apply[X, Y](e: F#Op[X]): X /> Y = Cont.Pure[X, Y](a.handle[X](e)) }
    def run[A, G <: Row](p: Free[F with G, A])(implicit @unused ev: A <:< Any, d: Distinct[F with G], @unused n: Nothing[G]): A ! G =
      Effects.relay[A, A, F, G](p)(x => pure[G, A](x))(g)(T, d)
  }

  /** 1 · the same, under the Scala 3 core's name for an `Answers` */
  def from[F <: Row](a: Answers[F])(implicit T: TypeableK[F]): Handler[F, Id] = answer[F](a)

  /** 2 · a state threaded through the operations, the result carrying the last state */
  def state[F <: Row, S](init: S)(c: StateClause[F, S])(implicit T: TypeableK[F]): Handler[F, Pair[S]#L] =
    new Full[F, Any, Pair[S]#L, Nothing] {
      def run[A, G <: Row](p: Free[F with G, A])(implicit @unused ev: A <:< Any, @unused d: Distinct[F with G], @unused n: Nothing[G]): (S, A) ! G = {
        val Mine = Split.at[F](T)
        // a call from inside flatMap cannot be a jump; `again` takes it, so the walk stays a checked loop
        def again(s: S)(x: Free[F with G, A]): (S, A) ! G = loop(s)(x)
        @tailrec def loop(s: S)(x: Free[F with G, A]): (S, A) ! G = Free.resume(x) match {
          case Return(a) => Return((s, a))
          // a lone operation is a Bind with a pure continuation (package.scala)
          case Inject(e) => loop(s)(Bind(Inject[F + G, A](e), (v: A) => Return[F + G, A](v)))
          case Bind(Inject(Mine(op)), k) => val (s2, v) = c[Any](s, op); loop(s2)(k(v))
          case Bind(Inject(e), k) => Inject[G, Any](e).flatMap(v => again(s)(k(v)))
          case other => throw new IllegalStateException("resume left a non-head form: " + other)
        }
        loop(init)(p)
      }
    }

  /** 3 · each operation a program in the effects `G`, which the rest of the row must hold (`!.translate`) */
  def into[F <: Row, G <: Row](i: Interpret[F, G])(implicit T: TypeableK[F]): Full[F, Any, Id, Holds[G]#L] =
    new Full[F, Any, Id, Holds[G]#L] {
      def run[A, R <: Row](p: Free[F with R, A])(implicit @unused ev: A <:< Any, d: Distinct[F with R], n: R <:< G): A ! R =
        Effects.translate[A, F, R](p)(new Interpret[F, R] {
          // the rest holds G, so a program in G is one in the rest: the row is contravariant
          def apply[X](e: F#Op[X]): X ! R = n.liftContra[({ type L[-r] = Free[r, X] })#L](i[X](e))
        })(T, d)
    }

  /** 4 · the operation and the rest of the program, `k`: abort, resume many times, answer in the rest */
  def control[F <: Row, O[_]](ret: Ret[O])(c: Control[F, O])(implicit T: TypeableK[F]): Handler[F, O] =
    new Full[F, Any, O, Nothing] {
      def run[A, G <: Row](p: Free[F with G, A])(implicit @unused ev: A <:< Any, d: Distinct[F with G], @unused n: Nothing[G]): O[A] ! G =
        Effects.handleWith[A, O[A], F, G](p)(a => pure[G, O[A]](ret[A](a)))(new Interpr[F, O[A] ! G] {
          def apply[X](e: F#Op[X]): Cont[X, O[A] ! G, O[A] ! G] = {
            val resume = new Resume[X, O[A], G]
            val out = c[X, A, G](e, resume)
            // `resume(x)` once, as the clause's answer: the program goes on, nothing to capture
            if (resume.calls == 1 && (out eq resume.last)) Cont.Pure[X, O[A] ! G](resume.arg)
            else Cont.shift[X, O[A] ! G, O[A] ! G] { k => resume.k = k; out }
          }
        })(T, d)
    }

  /**
   * The `resume` a `control` clause gets (the Scala 3 core's, okay2-handler-control-resume). Called once and
   * returned as the clause's answer, it is a tail resume, answered with no capture; otherwise each call is a
   * program that enters the captured `k`, which the capture fills in before any of them runs.
   */
  private final class Resume[X, B, G <: Row] extends (X => B ! G) with (() => B ! G) {
    var k: X => B ! G = _
    var arg: X = _
    var last: B ! G = _
    var calls: Int = 0
    def apply(x: X): B ! G = {
      calls += 1
      // the first call's node defers to this object itself (its `arg` is that call's, and only a second call
      // could change it, which gets a closure of its own): one allocation less a resumed operation
      last = if (calls == 1) { arg = x; Free.delay[G, B](this) } else Free.delay[G, B](() => k(x))
      last
    }
    /** the first call's resumption */
    def apply(): B ! G = k(arg)
  }
}

/**
 * The check behind the case forms (specs/handler-forms.md, the Scala 3 core's `checkImpl`): every case's
 * constructor is read off the effect's operation type, and the case's answer must conform to what that
 * constructor declares (`Find extends Op[Option[String]]`), the effect's own parameters substituted
 * (`Get[S] extends Op[S, S]` at `State[Int]` answers `Int`). Scala 2 refuses a constructor pattern against an
 * opaque answer type (measured: "constructor cannot be instantiated to expected type"), so the cases are typed
 * at `F#Op[Any]`, where a constructor whose answer its caller chooses (`Update[S, B]`) or its own field ties
 * (`Emit[A](a: A)`) cannot be checked: such a case is refused by name, pointing at `.poly`. A case with no
 * constructor (`case _`) may only throw. Exhaustiveness is scalac's own: a sealed `Op` not covered is a warning,
 * which `-Werror` makes an error.
 */
object HandlerCases {
  def answer(c: blackbox.Context)(cases: c.Tree): c.Tree = {
    import c.universe._
    val f = arg(c)(0)
    check(c)(f, cases, 0)
    q"_root_.okay2.Handler.Cases.answer[$f]($cases)"
  }

  def state(c: blackbox.Context)(cases: c.Tree): c.Tree = {
    import c.universe._
    val f = arg(c)(0)
    val s = arg(c)(1)
    check(c)(f, cases, 1)
    q"_root_.okay2.Handler.Cases.state[$f, $s](${c.prefix}.init)($cases)"
  }

  def into(c: blackbox.Context)(cases: c.Tree): c.Tree = {
    import c.universe._
    val f = arg(c)(0)
    val g = arg(c)(1)
    check(c)(f, cases, 2)
    q"_root_.okay2.Handler.Cases.into[$f, $g]($cases)"
  }

  private def arg(c: blackbox.Context)(i: Int): c.Type = {
    val args = c.prefix.actualType.typeArgs
    if (i < args.length) args(i) else c.abort(c.enclosingPosition, s"no type argument $i in ${c.prefix.actualType}")
  }

  /** kind: 0 a value, 1 a `(state, value)` pair, 2 a program */
  private def check(c: blackbox.Context)(f: c.Type, cases: c.Tree, kind: Int): Unit = {
    import c.universe._
    val marker = typeOf[CasesMarker]
    val (opSym, opArgs) = f.member(TypeName("Op")).infoIn(f) match {
      case PolyType(List(p), rhs) => rhs.substituteTypes(List(p), List(marker)).dealias match {
        case TypeRef(_, sym, as) => (sym, as)
        case other => c.abort(c.enclosingPosition, s"$f's operations are not a class: $other")
      }
      case other => c.abort(c.enclosingPosition, s"$f has no `type Op[+A]`: $other")
    }
    val idx = opArgs.indexWhere(_ =:= marker)
    if (idx < 0) c.abort(c.enclosingPosition, s"$f's operation type does not carry its answer as a type argument")
    val pairSym = typeOf[(Any, Any)].typeSymbol
    val freeSym = typeOf[Free[Any, Any]].typeSymbol

    // the lambda's match, under any wrapping the typer left; bounded by the wrapping, a loop
    var t: Tree = cases
    var caseDefs: List[CaseDef] = Nil
    while (caseDefs.isEmpty && t != EmptyTree) t match {
      case Function(_, Match(_, cs)) => caseDefs = cs
      case Function(_, body) => t = body
      case Typed(e, _) => t = e
      case Block(Nil, e) => t = e
      case _ => c.abort(cases.pos, "the cases form takes `{ case … }`; for a function or a value, use `.poly`")
    }

    caseDefs.foreach { cd =>
      val body = cd.body
      val gives: Type = kind match {
        case 0 => body.tpe
        case 1 => body.tpe.baseType(pairSym) match { case TypeRef(_, _, List(_, b)) => b; case _ => body.tpe }
        case _ => body.tpe.baseType(freeSym) match { case TypeRef(_, _, List(_, b)) => b; case _ => body.tpe }
      }
      // the operation's pattern: the case's own, or the pair's second for a state form
      val opPat0: Tree = kind match {
        case 1 => cd.pat match {
          case Apply(_, List(_, p)) => p
          case c.universe.Bind(_, Apply(_, List(_, p))) => p
          case other => other
        }
        case _ => cd.pat
      }
      var opPat = opPat0
      while (opPat match { case c.universe.Bind(_, _) => true; case _ => false }) opPat = opPat match { case c.universe.Bind(_, p) => p; case p => p }
      val ctorTpe: Type = opPat match {
        case Apply(_, _) | UnApply(_, _) => opPat.tpe
        case Typed(_, tpt) => tpt.tpe
        case _ => NoType
      }
      if (ctorTpe == NoType) {
        if (!(body.tpe <:< typeOf[Nothing]))
          c.abort(cd.pos, "a case with no constructor answers no operation's type: it may only throw (`case _ => throw …`)")
      } else {
        val ctor = ctorTpe.typeSymbol
        val declared = ctor.asType.toType.baseType(opSym) match {
          case TypeRef(_, _, as) if as.length == opArgs.length => as
          case _ => c.abort(cd.pos, s"${ctor.name} is not one of $f's operations")
        }
        val own = ctor.asClass.typeParams
        val fixed = declared.zipWithIndex.collect { case (x, i) if i != idx => x }
        val chosen = own.filter(p => declared(idx).exists(_.typeSymbol == p) && !fixed.exists(_.exists(_.typeSymbol == p)))
        if (chosen.nonEmpty)
          c.abort(cd.pos, s"${ctor.name} answers what its caller chose (or its own field's type), which the case form cannot check at `Any`: write this handler with `.poly`")
        val answer = ctorTpe.baseType(opSym) match {
          case TypeRef(_, _, as) if as.length == opArgs.length => as(idx)
          case _ => c.abort(cd.pos, s"${ctor.name} is not one of $f's operations")
        }
        if (!(gives <:< answer))
          c.abort(cd.body.pos, s"${ctor.name} answers $answer, but this case gives $gives")
      }
    }
  }
}

/** the mark the case check substitutes for the answer, to find where an operation type carries it */
sealed trait CasesMarker

/** `reset` as a value: `p.handle(Reset[R])` */
object Reset {
  def apply[R](implicit k: Shift.Key[R]): Handler.Full[Shift[R], R, Handler.Const[R]#L, Shift.Nesting] =
    new Handler.Full[Shift[R], R, Handler.Const[R]#L, Shift.Nesting] {
      def run[A, F <: Row](p: Free[Shift[R] with F, A])(implicit a: A <:< R, @unused d: Distinct[Shift[R] with F], n: Shift.Nesting[F]): R ! F =
        Shift.handle[R, F](a.liftCo[({ type L[+x] = Free[Shift[R] with F, x] })#L](p))(k, n)
    }
}

/** `p.handle(h)`, `p.handle(h1, h2)`, `p.handle(h1, h2, h3)`: each handler takes its effect off the row */
final class Handles[R, A](val p: Free[R, A]) {
  def handle(h: Handler.Value): Any = macro HandleMacro.one
  def handle(h1: Handler.Value, h2: Handler.Value): Any = macro HandleMacro.two
  def handle(h1: Handler.Value, h2: Handler.Value, h3: Handler.Value): Any = macro HandleMacro.three
}

/**
 * Scala 2 cannot take an effect OFF an intersection row by inference — `Free[E with F, A]` at a known `E` solves
 * `F` as the whole row in an implicit search — so the rest is computed here: the row's members, less the
 * handler's effect, and the call is `h.run[A, Rest](p)`, typed as any call is. A whitebox macro so the result
 * type is that call's.
 */
object HandleMacro {
  def one(c: whitebox.Context)(h: c.Tree): c.Tree = step(c)(c.prefix.tree, h)

  def two(c: whitebox.Context)(h1: c.Tree, h2: c.Tree): c.Tree = {
    import c.universe._
    q"new _root_.okay2.Handles(${step(c)(c.prefix.tree, h1)}).handle($h2)"
  }

  def three(c: whitebox.Context)(h1: c.Tree, h2: c.Tree, h3: c.Tree): c.Tree = {
    import c.universe._
    q"new _root_.okay2.Handles(${step(c)(c.prefix.tree, h1)}).handle($h2, $h3)"
  }

  private def step(c: whitebox.Context)(prefix: c.Tree, h: c.Tree): c.Tree = {
    import c.universe._
    val handles = typeOf[Handles[Any, Any]].typeSymbol
    val full = c.mirror.staticModule("okay2.Handler").moduleClass.info.decl(TypeName("Full"))
    val row = typeOf[Row]
    // the members of an intersection, flattened by a worklist
    def parts(t: Type): List[Type] = {
      val out = List.newBuilder[Type]
      var todo = List(t)
      while (todo.nonEmpty) {
        val x = todo.head
        todo = todo.tail
        x.dealias match {
          case RefinedType(ps, _) => todo = ps ++ todo
          case other => out += other
        }
      }
      out.result()
    }
    val (g, a) = prefix.tpe.baseType(handles).typeArgs match {
      case g0 :: a0 :: Nil => (g0, a0)
      case _ => c.abort(prefix.pos, s"not a program: ${prefix.tpe}")
    }
    val e = h.tpe.baseType(full) match {
      case TypeRef(_, _, e0 :: _) => e0
      case _ => c.abort(h.pos, s"${h.tpe} is not a handler value (Handler.Full)")
    }
    val gs = parts(g)
    val es = parts(e).filterNot(_ =:= row)
    val missing = es.filterNot(x => gs.exists(_ =:= x))
    if (missing.nonEmpty)
      c.abort(h.pos, s"this program's row $g does not hold ${missing.mkString(" with ")}, which the handler takes off")
    val rest = gs.filterNot(x => x =:= row || es.exists(_ =:= x))
    val restTree: Tree =
      if (rest.isEmpty) tq"_root_.okay2.Row"
      else rest.map(t => TypeTree(t): Tree).reduceLeft((l, r) => tq"$l with $r")
    // the program itself rather than its wrapper, so nothing is allocated for the call
    val p = prefix match {
      case Apply(_, List(arg)) => arg
      case other => q"$other.p"
    }
    q"$h.run[$a, $restTree]($p)"
  }
}
