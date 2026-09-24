package okay2

/**
 * okay2-async — okay-async for the Scala 2 core (specs/okay2.md,
 * stage 4). Programs stay in the effect world: `A ! Async` composes
 * by flatMap, non-blocking by construction. The effect has two
 * operations — `Run`, a suspended (possibly blocking) computation, and
 * `Await`, the universal callback-form suspension with an error
 * channel and a canceller. Blocking exists only at the run boundary
 * and only under `CanBlock` evidence, which the platform module
 * provides (`import okay2.platform._`).
 */
package object async extends AsyncFailingLow {

  /** the typed road, for the row every second `Resource.run` passes:
   * one effect is a shape the compiler pins, no cast needed */
  implicit val asyncFailing: Failing[Async] = AsyncFailing.async

  /** suspend on a callback registration — the simple form: success
   * only, nothing to unregister. The suspended COMPUTATION is
   * `Async(a)`: a function named `async` in this package object would
   * collide with the package's own name under `import okay2._` (the
   * package) and `import okay2.async._` (the function) — measured,
   * "reference to async is ambiguous" at every call site. */
  def await[A](register: (A => Unit) => Unit): A ! Async =
    Free.inject[Async, A](Async.Await[A](k => { register(a => k(Right(a))); () => () }))
}
