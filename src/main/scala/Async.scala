package okay

/**
 * Asynchrony, Loom-style: on virtual threads blocking IS asynchrony,
 * so the whole effect is one operation — run this (possibly blocking)
 * computation. No callbacks, no scheduler, no fiber runtime of our
 * own: a program runs on a virtual thread (spawn), a blocked
 * operation parks that thread for free, and concurrency is just more
 * virtual threads (par, race). The handler answers every operation by
 * executing it — exactly tail-resumptive, so run is a relay, at relay
 * speed.
 */
enum Async[+A]:
  /** a suspended (possibly blocking) computation */
  case Run[A](run: () => A) extends Async[A]

/** suspend a (possibly blocking) computation as an operation */
inline def async[A](a: => A): A ! Async = effect(Async.Run(() => a))

/** execute the operation on the current (ideally virtual) thread */
given Handler[Async] = new:
  def handle[A](e: Async[A]): A = e match
    case Async.Run(f) => f()

object Async {

  import !.*
  import java.util.concurrent.CompletableFuture

  /** handle by executing each operation in place, forwarding the effects F */
  def run[A, F[+_]](prog: A ! Async + F): A ! F =
    relay[A, A, Async, F](prog)(pure(_)):
      [X, Y] => e => e match
        case Run(f) => Cont.Pure(f())

  /** run the program on its own virtual thread */
  def spawn[A](prog: => A ! Async): CompletableFuture[A] =
    val f = CompletableFuture[A]()
    Thread.startVirtualThread: () =>
      try f.complete(prog.runWith)
      catch case e: Throwable => f.completeExceptionally(e)
    f

  /** both, each on its own virtual thread */
  def par[A, B](a: => A ! Async, b: => B ! Async): (A, B) ! Async =
    async:
      val (fa, fb) = (spawn(a), spawn(b))
      (fa.join(), fb.join())

  /** the first of the two to finish */
  def race[A](a: => A ! Async, b: => A ! Async): A ! Async =
    async(spawn(a).applyToEither(spawn(b), x => x).join())
}
