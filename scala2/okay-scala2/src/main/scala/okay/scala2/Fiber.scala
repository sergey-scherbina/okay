package okay.scala2

/**
 * A running computation, for Scala 2.13 (specs/scala2-facade.md,
 * stage 5). `Async.fork` makes one. Waiting for it is a program, like
 * everything else that waits here.
 */
final class Fiber[A] private[scala2] (private val fiber: okay.Fiber[A]) {

  /** its answer; fails as it failed */
  def join: Eff[Async, A] = Async.lift(fiber.joinAsync)

  /** its answer, or how it failed */
  def joinEither: Eff[Async, Either[Throwable, A]] =
    Async.lift(okay.Async.await[Either[Throwable, A]] { k =>
      fiber.onComplete(r => k(Right(r)))
      () => ()
    })

  /** ask it to stop (best effort: it notices between operations) */
  def cancel: Eff[Async, Unit] = Async(fiber.cancel())
}

/**
 * A channel between fibers, for Scala 2.13 (stage 5): okay's own,
 * bounded by `capacity`. `send` suspends while it is full and
 * `receive` while it is empty; both are programs.
 */
final class Channel[A] private (private val channel: okay.Channel[A]) {

  /** true if the element was taken, false if the channel is closed */
  def send(a: A): Eff[Async, Boolean] = Async.lift(channel.send(a))

  /** the next element, or None once it is closed and drained */
  def receive: Eff[Async, Option[A]] = Async.lift(channel.receive)

  /** send without waiting: false if it is full or closed */
  def offer(a: A): Boolean = channel.offer(a)

  def close(): Unit = channel.close()

  def isClosed: Boolean = channel.isClosed

  /** every element until it is closed, as a stream */
  def source: Source[A] = Source.of(okay.drained(channel))
}

object Channel {
  def apply[A](capacity: Int): Channel[A] = new Channel(okay.Channel[A](capacity))
}
