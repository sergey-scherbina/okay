package okay

import okay.RowLift.plus

/**
 * INSTANCES OF ANY EFFECT, MADE AT RUN TIME — the fourth corner of a
 * square whose other three were already built.
 *
 * A row is split by a runtime test, so two members of one signature
 * are told apart exactly when the operation carries something to
 * compare (docs/many-instances.md). What to carry has three answers,
 * and until now they left one shape unserved:
 *
 *                  | named at compile time | made at run time
 *     -------------|-----------------------|------------------
 *     one effect   | `Keyed`/`Tag` at State| `Refs`
 *     any effect   | `Tag`                 | THIS
 *
 * `Tag` keys an operation with a literal, so the row lists the
 * instances and the compiler counts them. That is the right trade
 * when the instances can be named — and no trade at all when they
 * cannot: one `Users` per tenant, one `Cache` per shard, where the
 * tenants come out of a config file nobody has read at compile time.
 * `Refs` solves exactly that, and only for state.
 *
 * So the key becomes a VALUE:
 *
 *     val alice = Instances.handle("alice")
 *     val bob   = Instances.handle("bob")
 *
 *     val p: (Int, Int) ! Instances.Of[State % Int] =
 *       for
 *         a <- Instances.at(alice)(State.Get[Int]())
 *         b <- Instances.at(bob)(State.Get[Int]())
 *       yield (a, b)
 *
 * ONE ROW MEMBER, HOWEVER MANY INSTANCES — the same trade `Refs`
 * makes, for the same reason: a type cannot list what does not exist
 * yet. The row says "this program uses instances of F", and which
 * ones is a question about the run rather than about the type.
 *
 * ROUTE A PROGRAM, NOT AN OPERATION. `route` walks a FINISHED program
 * and sends every F operation to one handle, so a function written
 * against a plain `F` — anyone's function, already compiled — serves
 * a tenant it was never told about. That is `Tag.tag`'s trick with
 * the key chosen at run time.
 *
 * HANDLING. `handle` runs every instance in one pass, choosing the
 * handler by handle; the choice is an ordinary function, so it may
 * close over whatever per-instance state the caller keeps. When an
 * instance wants the effect's OWN runner instead — `State.run` at its
 * own initial state, say — `only` strips one handle back to the plain
 * signature and leaves the rest of the instances in the row, to be
 * stripped in turn.
 *
 * THE TEST IS BY SIGNATURE AND THEN BY HANDLE, which is one more than
 * `Tag` does: `Tag` tests the key alone, and two of its members
 * sharing a key collide (tag-key-collision). Here the signature is
 * tested first, so `Of[State % Int] + Of[Writer % String]` is a
 * legitimate row, and only a shared HANDLE within one signature can
 * confuse anything — and a handle is a fresh object, so sharing one
 * is something the caller has to do on purpose.
 */
final case class Instances[F[+_], +A](at: Instances.Handle, op: F[A])

object Instances:

  /**
   * An instance's identity: a fresh object, compared by reference.
   * The name is for the failure message and for reading a trace; two
   * handles of the same name are still two handles.
   */
  final class Handle(val name: String):
    override def toString: String = s"instance($name)"

  /** a new instance of anything, made where it is needed */
  def handle(name: String): Handle = new Handle(name)

  /** one row member per SIGNATURE, however many instances of it */
  type Of[F[+_]] = [A] =>> Instances[F, A]

  /**
   * The test asks the signature first and the handle never: a row
   * holds one member per F, and telling the instances apart is the
   * runner's job, not the row's. That is what makes a handle able to
   * be a run-time value at all.
   */
  given of[F[+_]](using t: TypeableK[F]): okay.Effect[Of[F]] = okay.Effect.of(new:
    def unapply[A](x: Any): Option[x.type & Instances[F, A]] = x match
      case i: Instances[?, ?] if t.test(i.op) =>
        Some(x.asInstanceOf[x.type & Instances[F, A]])
      case _ => None)

  /** perform one operation at one instance */
  inline def at[F[+_]](h: Handle)[A](op: F[A]): A ! Of[F] =
    effect(Instances[F, A](h, op))

  /** send every F operation of an already-written program to one
   * instance, leaving the rest of the row alone */
  def route[F[+_]](h: Handle)(using TypeableK[F])[A, G[+_]]
                  (p: A ! (F + G)): A ! (Of[F] + G) =
    !.interpret[A, F, Of[F], G](p)([X] => (e: F[X]) => at[F](h)(e).plus[G])

  /**
   * ONE handler for every instance, choosing by handle — `Tag.handler`
   * with the key read at run time.
   *
   * `pick` is an ordinary function, so per-instance state is the
   * caller's to keep however it likes: a map built from config, a
   * `Refs` cell each, a connection per tenant. What it does not do is
   * thread state for you; that is `only` plus the effect's own runner.
   */
  def handler[F[+_]](pick: Handle => Handler[F]): Handler[Of[F]] = new:
    def handle[A](e: Instances[F, A]): A = pick(e.at).handle(e.op)

  /**
   * Strip ONE instance back to the plain signature, so the effect's
   * own runner takes it at its own state — and leave every other
   * instance where it was, to be stripped in its turn.
   *
   * The answer's row says both: `F` for the instance now unwrapped,
   * `Of[F]` for the ones still wrapped.
   */
  def only[F[+_]](h: Handle)(using TypeableK[Of[F]])[A, G[+_]]
                 (p: A ! (Of[F] + G)): A ! (F + (Of[F] + G)) =
    !.interpret[A, Of[F], F, Of[F] + G](p)([X] => (e: Instances[F, X]) =>
      if e.at eq h then effect[F + (Of[F] + G), X](e.op)
      else effect[F + (Of[F] + G), X](e))

  /**
   * The residual member, discharged. After the last handle has been
   * stripped by `only`, the row still SAYS `Of[F]` — a type cannot
   * know that the instances are used up, which is the same trade this
   * whole effect makes. This asserts what the caller believes: no
   * operation of F remains, and if one does it names the handle that
   * was never stripped rather than failing somewhere later.
   */
  def exhausted[F[+_], A, G[+_]](using TypeableK[Of[F]])
                                (p: A ! (Of[F] + G)): A ! G =
    !.interpret[A, Of[F], okay.Pure, G](p)([X] => (e: Instances[F, X]) =>
      throw new IllegalStateException(
        s"an operation of ${e.at} survived: that instance was never stripped by `only`"))
