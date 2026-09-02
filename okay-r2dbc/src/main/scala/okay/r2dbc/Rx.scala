package okay.r2dbc

import org.reactivestreams.{Publisher, Subscriber, Subscription}
import java.util.concurrent.{CountDownLatch, LinkedBlockingQueue}

/**
 * The reactive-streams bridge, as small as it can be: a Publisher
 * becomes a PULL — `next(n)` requests n and parks until they arrive.
 * Parking is the whole point: behind `Async.Run` the thread is a
 * virtual one, so the "non-blocking" driver and the blocking JDBC one
 * cost the same here (specs/sql.md, the hatch honestly framed); what
 * R2DBC buys is the DRIVER, not the thread.
 */
private[r2dbc] object Rx:

  private object Done
  private final case class Failed(e: Throwable)

  /** subscribe, pull everything, answer it all — for the small
   * publishers (row counts, commit, begin) */
  def all[T](p: Publisher[T]): Vector[T] =
    val pull = Pull(p)
    try
      val out = Vector.newBuilder[T]
      var going = true
      while going do
        val (items, done) = pull.next(256)
        out ++= items
        going = !done
      out.result()
    finally pull.cancel()

  /** the first element, if any — then cancel */
  def first[T](p: Publisher[T]): Option[T] =
    val pull = Pull(p)
    try pull.next(1)._1.headOption
    finally pull.cancel()

  /** a demand-driven subscriber: `next(n)` requests n items and parks
   * until n arrived or the stream ended; `(items, done)` */
  final class Pull[T](p: Publisher[T]) extends Subscriber[T]:
    private val q = LinkedBlockingQueue[AnyRef]()
    private val subscribed = CountDownLatch(1)
    @volatile private var sub: Subscription = null
    @volatile private var ended = false
    p.subscribe(this)

    def onSubscribe(s: Subscription): Unit = { sub = s; subscribed.countDown() }
    def onNext(t: T): Unit = q.put(t.asInstanceOf[AnyRef])
    def onError(e: Throwable): Unit = { q.put(Failed(e)) }
    def onComplete(): Unit = { q.put(Done) }

    def next(n: Int): (Vector[T], Boolean) =
      if ended then (Vector.empty, true)
      else
        subscribed.await()
        sub.request(n.toLong)
        val out = Vector.newBuilder[T]
        var got = 0
        var done = false
        while got < n && !done do
          q.take() match
            case Done => done = true; ended = true
            case Failed(e) => ended = true; throw e
            case item => out += item.asInstanceOf[T]; got += 1
        (out.result(), done)

    def cancel(): Unit =
      if !ended then
        ended = true
        val s = sub
        if s != null then s.cancel()
