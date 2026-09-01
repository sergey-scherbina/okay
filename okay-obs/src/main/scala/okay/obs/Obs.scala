package okay.obs

import okay.codec.{Cbor, Schema}
import okay.persist.{Ack, Topic}

/**
 * Tracing without a framework (specs/obs.md): a span is a VALUE with
 * a Schema like every other observable here, appended to a trace
 * topic — so retention is a Policy, sampling is which spans you
 * write, shipping is a consumer, audit is free. Propagation is W3C
 * traceparent, the vocabulary every collector already speaks. The
 * current span is HANDLER state, not an effect programs request:
 * programs stay observability-blind, the edge composes a tracing
 * handler around any other.
 */
final case class Attr(key: String, value: String)
object Attr:
  given Schema[Attr] = Schema.derived

final case class Span(traceId: String, spanId: String, parentId: Option[String],
                      name: String, start: Long, end: Long,
                      attrs: Vector[Attr] = Vector.empty,
                      status: String = "ok")
object Span:
  given Schema[Span] = Schema.derived

/** an inbound trace context — what a valid traceparent carries;
 * tracestate rides opaquely, interpreted never */
final case class Parent(traceId: String, spanId: String, sampled: Boolean,
                        state: Option[String] = None)

object Trace {

  /** total parse of a traceparent header: anything not exactly the
   * W3C shape is None — and the caller starts a fresh root, NAMED as
   * such, rather than guessing */
  def parse(traceparent: String, tracestate: Option[String] = None): Option[Parent] =
    traceparent.split('-') match
      case Array(ver, t, s, f)
        if ver.length == 2 && hex(ver) && ver != "ff"
          && t.length == 32 && hex(t) && t.exists(_ != '0')
          && s.length == 16 && hex(s) && s.exists(_ != '0')
          && f.length == 2 && hex(f) =>
        Some(Parent(t, s, (Integer.parseInt(f, 16) & 1) == 1, tracestate))
      case _ => None

  /** the outbound header — version 00, the only one there is */
  def render(traceId: String, spanId: String, sampled: Boolean = true): String =
    s"00-$traceId-$spanId-${if sampled then "01" else "00"}"

  private def hex(s: String): Boolean =
    s.forall(c => (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f'))

  private val rnd = new scala.util.Random
  private def id(bytes: Int): String =
    val bs = new Array[Byte](bytes)
    rnd.nextBytes(bs)
    val s = bs.map(b => f"$b%02x").mkString
    if s.exists(_ != '0') then s else "1" + s.drop(1)   // all-zero ids are invalid by W3C
  def freshTraceId(): String = id(16)
  def freshSpanId(): String = id(8)
}

/** which spans get written — sampling is a write decision, nothing
 * upstream knows it exists */
enum Sample:
  case Never
  case RootOnly
  case Always

/**
 * The tracer: one per request (or per fiber) — the current span is
 * its state, exactly as a cursor is a terminal's. `root` is the
 * inbound edge, `span` a child region, `outbound` the header for a
 * call that leaves, and `traced` wraps ANY comonadic handler with a
 * span per operation, without that handler's knowledge.
 */
final class Tracer(topic: Topic, sample: Sample = Sample.Always,
                   clock: () => Long = () => System.currentTimeMillis):

  private var current: Option[(String, String)] = None   // (traceId, spanId)
  private var state: Option[String] = None                // tracestate, opaque

  /** the inbound edge: a valid traceparent continues the trace; an
   * ABSENT one starts a root; a DAMAGED one starts a fresh root
   * carrying the damage as an attribute — named, never guessed */
  def root[A](name: String, traceparent: Option[String] = None,
              tracestate: Option[String] = None)(body: => A): A =
    val (parent, extra) = traceparent match
      case None => (None, Vector.empty)
      case Some(h) => Trace.parse(h, tracestate) match
        case Some(p) => (Some(p), Vector.empty)
        case None => (None, Vector(Attr("traceparent.damaged", h)))
    val traceId = parent.map(_.traceId).getOrElse(Trace.freshTraceId())
    state = parent.flatMap(_.state)
    run(name, traceId, parent.map(_.spanId), extra)(body)

  /** a child region of whatever is current (a root when nothing is) */
  def span[A](name: String, attrs: Attr*)(body: => A): A = current match
    case Some((t, s)) => run(name, t, Some(s), attrs.toVector)(body)
    case None => run(name, Trace.freshTraceId(), None, attrs.toVector)(body)

  /** the traceparent an outgoing call should carry */
  def outbound: Option[String] =
    current.map((t, s) => Trace.render(t, s))

  /** the tracestate that arrived, passed through OPAQUELY — carried,
   * never interpreted */
  def outboundState: Option[String] = state

  /** wrap any Handler: one child span per operation, the operation
   * named by the caller — composition, not instrumentation */
  def traced[F[_]](inner: okay.Handler[F], name: [X] => F[X] => String): okay.Handler[F] = new:
    def handle[A](e: F[A]): A = span(name(e))(inner.handle(e))

  private def run[A](name: String, traceId: String, parentId: Option[String],
                     attrs: Vector[Attr])(body: => A): A =
    val self = Trace.freshSpanId()
    val before = current
    current = Some((traceId, self))
    val start = clock()
    var status = "ok"
    try body
    catch case e: Throwable =>
      status = s"error: ${e.getMessage}"
      throw e
    finally
      current = before
      val keep = sample match
        case Sample.Always => true
        case Sample.RootOnly => parentId.isEmpty
        case Sample.Never => false
      if keep then
        val _ = topic.append(traceId.getBytes("UTF-8"),
          Cbor.write(Span(traceId, self, parentId, name, start, clock(), attrs, status)),
          Ack.Durable)
