package okay

/**
 * The core effects' operations as VALUES, for another language to hand
 * back to okay to perform (interop-shared): okay-clojure's `Ops` and
 * okay-frege's `Ops` are the names those languages bind to, and both are
 * this, once. Each answers the operation value; the okay program's row
 * decides at run time whether it is its own (`Member`), refusing it by
 * name if not.
 */
object Operations {
  def ask(): AnyRef = Reader.Ask[Any, Any]()
  def get(): AnyRef = State.Get[Any, Any]()
  def set(s: Any): AnyRef = State.Set[Any, Any](s)
  def raise(e: Any): AnyRef = Throws[Any, Nothing](e)
  def choose(options: Seq[Any]): AnyRef = Choose(options)

  /**
   * `Async`: park for `millis` on the platform timer (cancellable, as
   * `Async.sleep` is) and answer the milliseconds slept — a number, not
   * `()`, because a foreign language's unit is its own type (Frege's is a
   * Java `short`) and a boxed Scala Unit would not cross.
   */
  def sleep(millis: Long): AnyRef =
    val timer = summon[Timer]
    Async.Await[java.lang.Long](k => timer.after(millis)(() => k(Right(Long.box(millis)))))
}
