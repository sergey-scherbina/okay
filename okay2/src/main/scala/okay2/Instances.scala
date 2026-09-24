package okay2

/**
 * SEVERAL INSTANCES OF ONE SIGNATURE IN ONE ROW — the Scala 3 core's
 * Instances.scala, in Scala 2.13 (specs/okay2.md, stage 12).
 *
 * A row is split by a runtime test, so two members of one signature are
 * told apart exactly when the operation carries something to compare.
 * `State[Int] + State[String]` carries nothing: both tests are the
 * class, and `Distinct` refuses the row. What to carry has three
 * answers, and okay2 has all three now:
 *
 *                  | named at compile time | made at run time
 *     -------------|-----------------------|------------------
 *     any effect   | `Tag[K, F]`           | `Instances[F]`
 *     one Writer   | `Writer.byValue`      |
 *
 * plus the one Delim already offers: a fresh prompt per handler
 * installation, the most scoped and the most invasive.
 *
 * `Tag` keys an operation with a LITERAL, so the row lists the instances
 * and the compiler counts them; `Distinct` reads the key. `Instances`
 * keys it with a `Handle` made at run time — one per tenant, per shard,
 * out of a config file nobody has read at compile time — and the row
 * has ONE member however many instances there are.
 *
 * A signature here is a Row with its operations as `Op` (Row.scala), so
 * each wrapper is a signature too: `Tag[K, F]` is a Row whose `Op[A]` is
 * `Tag.Op[K, F, A]`, holding F's operation under the key.
 */
sealed trait Tag[K, F <: Row] extends Row { type Op[+A] = Tag.Op[K, F, A] }

/**
 * ANY effect, under a key.
 *
 *     type Small = Tag["small", State[Int]]
 *     type Big   = Tag["big",   State[Int]]
 *
 * TAG A PROGRAM, NOT AN OPERATION: `tag` walks a finished program and
 * puts every F operation under the key, so a function written against a
 * plain `State[Int]` — anyone's, already compiled — runs twice in one
 * program at two states. HANDLING IS THE EFFECT'S OWN: `untag` strips
 * one key and hands the plain signature back to its existing handler.
 *
 * THE TEST IS THE KEY AND THE SIGNATURE, so `Tag["k", Beep] + Tag["k",
 * Buzz]` is an ordinary row. What no runtime test can fix is the same
 * SIGNATURE CLASS under the same key — `Tag["k", Reader[Int]] +
 * Tag["k", Reader[String]]` — and that `Distinct` refuses.
 */
object Tag {
  final case class Op[K, F <: Row, +A](key: K, op: Any)

  /** the Scala 3 core's spelling of the row member */
  type Of[K, F <: Row] = Tag[K, F]

  /** the test is the KEY and the SIGNATURE; a type argument under that
   * signature is erased and no test can reach it */
  implicit def effect[K, F <: Row](implicit k: ValueOf[K], t: TypeableK[F]): Effect[Tag[K, F]] =
    new Effect[Tag[K, F]] {
      def test(x: Any): Boolean = x match {
        case o: Op[_, _, _] => o.key == k.value && t.test(o.op)
        case _ => false
      }
    }

  /** perform one operation under the key */
  def one[K, F <: Row, A](op: F#Op[A])(implicit k: ValueOf[K]): A ! Tag[K, F] =
    Free.inject[Tag[K, F], A](Op[K, F, A](k.value, op))

  /**
   * put every F operation of a program under the key, leaving the rest
   * of the row alone. `Distinct` is asked of the WHOLE picture, source
   * and target: a Tag of the same key over a colliding signature in G
   * would take this one's operations.
   */
  def tag[K, F <: Row, A, G <: Row](p: Free[F with G, A])(implicit T: TypeableK[F], k: ValueOf[K], d: Distinct[F with (Tag[K, F] + G)]): A ! (Tag[K, F] + G) =
    Effects.interpret[A, F, Tag[K, F], G](p)(new Interpret[F, Tag[K, F] + G] {
      def apply[X](e: F#Op[X]): X ! (Tag[K, F] + G) = one[K, F, X](e)
    })

  /** strip one key, handing back the plain signature for its own
   * handler to take */
  def untag[K, F <: Row, A, G <: Row](p: Free[Tag[K, F] with G, A])(implicit T: TypeableK[Tag[K, F]], d: Distinct[Tag[K, F] with (F + G)]): A ! (F + G) =
    Effects.interpret[A, Tag[K, F], F, G](p)(new Interpret[Tag[K, F], F + G] {
      def apply[X](e: Op[K, F, X]): X ! (F + G) = Free.Inject[F, X](e.op)
    })

  /** a comonadic handler for one key, out of the effect's own */
  def handler[K, F <: Row](h: Handler[F]): Handler[Tag[K, F]] = new Handler.Of[Tag[K, F]] {
    def handle[A](e: Op[K, F, A]): A = h.handleOp[A](e.op)
  }
}

/**
 * INSTANCES OF ANY EFFECT, MADE AT RUN TIME.
 *
 *     val alice = Instances.handle("alice")
 *     val bob   = Instances.handle("bob")
 *
 *     val p: (Int, Int) ! Instances[State[Int]] =
 *       for {
 *         a <- Instances.at(alice)(State.get[Int])
 *         b <- Instances.at(bob)(State.get[Int])
 *       } yield (a, b)
 *
 * ONE ROW MEMBER, HOWEVER MANY INSTANCES: a type cannot list what does
 * not exist yet. The row says "this program uses instances of F", and
 * which ones is a question about the run.
 *
 * THE TEST IS BY SIGNATURE AND THEN BY HANDLE: the row's test asks the
 * signature only, so `Instances[State[Int]] + Instances[Writer[String]]`
 * is a legitimate row, and telling the instances apart is the runner's
 * job — `handler(pick)` for all of them in one pass, `only(h)` to strip
 * one back to the plain signature for the effect's own runner.
 */
sealed trait Instances[F <: Row] extends Row { type Op[+A] = Instances.Op[F, A] }

object Instances {
  final case class Op[F <: Row, +A](at: Handle, op: Any)

  /** the Scala 3 core's spelling of the row member */
  type Of[F <: Row] = Instances[F]

  /** an instance's identity: a fresh object, compared by reference; the
   * name is for failure messages and traces */
  final class Handle(val name: String) {
    override def toString: String = s"instance($name)"
  }

  /** a new instance of anything, made where it is needed */
  def handle(name: String): Handle = new Handle(name)

  /** the test asks the signature and never the handle */
  implicit def effect[F <: Row](implicit t: TypeableK[F]): Effect[Instances[F]] =
    new Effect[Instances[F]] {
      def test(x: Any): Boolean = x match {
        case o: Op[_, _] => t.test(o.op)
        case _ => false
      }
    }

  /**
   * perform at one instance — given a PROGRAM of F, the way okay2's
   * constructors hand operations out (`State.get[Int]`); every operation
   * in it goes to `h`. The Scala 3 core takes the bare operation.
   */
  def at[F <: Row, A](h: Handle)(op: A ! F): A ! Instances[F] = routeLoop[F, A](h)(op)

  /** the row is F alone, so every operation is F's: no test needed */
  private def routeLoop[F <: Row, A](h: Handle)(p: Free[F, A]): A ! Instances[F] =
    Free.resume(p) match {
      case Free.Return(a) => Free.Return(a)
      case Free.Inject(e) => Free.Inject[Instances[F], A](Op[F, A](h, e))
      case Free.Bind(Free.Inject(e), k) =>
        Free.Inject[Instances[F], Any](Op[F, Any](h, e)).flatMap(x => routeLoop[F, A](h)(k(x)))
      case other => throw new IllegalStateException("resume left a non-head form: " + other)
    }

  /** send every F operation of an already-written program to one
   * instance, leaving the rest of the row alone */
  def route[F <: Row, A, G <: Row](h: Handle)(p: Free[F with G, A])(implicit T: TypeableK[F], d: Distinct[F with (Instances[F] + G)]): A ! (Instances[F] + G) =
    Effects.interpret[A, F, Instances[F], G](p)(new Interpret[F, Instances[F] + G] {
      def apply[X](e: F#Op[X]): X ! (Instances[F] + G) = Free.Inject[Instances[F] + G, X](Op[F, X](h, e))
    })

  /** ONE handler for every instance, choosing by handle; per-instance
   * state is the caller's to keep however it likes */
  def handler[F <: Row](pick: Handle => Handler[F]): Handler[Instances[F]] = new Handler.Of[Instances[F]] {
    def handle[A](e: Op[F, A]): A = pick(e.at).handleOp[A](e.op)
  }

  /** strip ONE instance back to the plain signature, so the effect's own
   * runner takes it; the others stay wrapped, to be stripped in turn */
  def only[F <: Row, A, G <: Row](h: Handle)(p: Free[Instances[F] with G, A])(implicit T: TypeableK[Instances[F]], d: Distinct[Instances[F] with (F + Instances[F] + G)]): A ! (F + Instances[F] + G) =
    Effects.interpret[A, Instances[F], F + Instances[F], G](p)(new Interpret[Instances[F], F + Instances[F] + G] {
      def apply[X](e: Op[F, X]): X ! (F + Instances[F] + G) =
        if (e.at eq h) Free.Inject[F + Instances[F] + G, X](e.op)
        else Free.Inject[F + Instances[F] + G, X](e)
    })

  /** the residual member, discharged: after the last handle was stripped
   * the row still SAYS `Instances[F]`; this asserts none remains and
   * names the handle that was never stripped if one does */
  def exhausted[F <: Row, A, G <: Row](p: Free[Instances[F] with G, A])(implicit T: TypeableK[Instances[F]], d: Distinct[Instances[F] with G]): A ! G =
    Effects.interpret[A, Instances[F], Pure, G](p)(new Interpret[Instances[F], Pure + G] {
      def apply[X](e: Op[F, X]): X ! (Pure + G) =
        throw new IllegalStateException(s"an operation of ${e.at} survived: that instance was never stripped by `only`")
    })
}
