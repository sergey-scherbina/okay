package okay.obs

import okay.*
import okay.codec.{Json, Schema}
import okay.persist.{Ack, Topic}

/**
 * Log lines as values, joined to the trace by the HANDLER.
 *
 * specs/obs.md said outright that log lines were not its business,
 * and fifty printlns across the modules are what stood in for one.
 * A line nobody can join to a trace is a line nobody reads during an
 * incident, so this closes the observability doctrine's third leg on
 * the same two rules the other two follow.
 *
 * **No new signature is minted.** A program that logs is a program
 * that TELLS, and the core already has the effect for that: `Writer
 * % Line`. `Log.info("...")` is `tell(Line(...))`; a row that logs
 * reads `A ! (Writer % Log.Line + Async)`, and everything the Writer
 * algebra can do — collect it, fold it, ignore it — works here for
 * free. What is new is only the VALUE and the handlers.
 *
 * **Correlation belongs to the handler, not to the program.** The
 * program says a level, a message and fields; the traceId and spanId
 * are stamped when the line is written, out of the ambient `Tracer`.
 * That is obs.md's own ruling for spans ("the current span is HANDLER
 * state, not an effect programs request") applied to the line, and it
 * is what keeps a domain function free of an observability argument.
 * A line told with no tracer in scope simply carries no ids.
 *
 * A handler here is COMONADIC — `Say(w)` answers `Unit`, so a line is
 * written the moment it is told, not accumulated. A crash after a log
 * call has already logged, which is the entire point of logging.
 */
object Log:

  enum Level derives Schema:
    case Debug, Info, Warn, Error
    /** Debug < Info < Warn < Error — a handler writes at or above its own */
    def atLeast(min: Level): Boolean = ordinal >= min.ordinal

  /**
   * One line. `Attr` is the same pair `Span` carries, so a field on a
   * line and an attribute on a span are the same thing said twice —
   * which is what makes them join.
   */
  final case class Line(level: Level, message: String,
                        fields: Vector[Attr] = Vector.empty,
                        traceId: Option[String] = None,
                        spanId: Option[String] = None,
                        at: Long = 0L,
                        logger: String = "") derives Schema

  /** the row a logging program carries */
  type Says = Writer % Line

  // ── the program side: one operation, the core's own ──────────────

  def at(level: Level, message: String, fields: (String, String)*): Unit ! Says =
    Writer.tell(Line(level, message, fields.toVector.map(Attr.apply)))

  def debug(message: String, fields: (String, String)*): Unit ! Says = at(Level.Debug, message, fields*)
  def info(message: String, fields: (String, String)*): Unit ! Says = at(Level.Info, message, fields*)
  def warn(message: String, fields: (String, String)*): Unit ! Says = at(Level.Warn, message, fields*)
  def error(message: String, fields: (String, String)*): Unit ! Says = at(Level.Error, message, fields*)

  /** the failure as a line, its class and message as fields — what an
    * incident actually needs off a caught throwable */
  def failure(message: String, e: Throwable, fields: (String, String)*): Unit ! Says =
    at(Level.Error, message,
      (fields :+ ("error" -> e.getClass.getName)
        :+ ("error.message" -> Option(e.getMessage).getOrElse("")))*)

  // ── the handlers: a line is written as it is told ────────────────

  /**
   * The one place a told line becomes a complete one: the clock and
   * the ids are the handler's, never the program's.
   */
  private def stamp(l: Line, tracer: Option[Tracer], clock: () => Long, logger: String): Line =
    val ctx = tracer.flatMap(_.context)
    l.copy(at = if l.at == 0L then clock() else l.at,
      traceId = l.traceId.orElse(ctx.map(_._1)),
      spanId = l.spanId.orElse(ctx.map(_._2)),
      logger = if l.logger.isEmpty then logger else l.logger)

  /** every complete line, to wherever `write` puts it */
  def to(write: Line => Unit, min: Level = Level.Info,
         tracer: Option[Tracer] = None,
         clock: () => Long = () => System.currentTimeMillis,
         logger: String = ""): Handler[Says] = new:
    def handle[A](op: Writer[Line, A]): A = op match
      case Writer.Say(l) =>
        if l.level.atLeast(min) then write(stamp(l, tracer, clock, logger))

  /**
   * One JSON object per line on stdout — the shape every collector
   * (Fluent Bit, Vector, the Docker json-file driver, a Kubernetes
   * node agent) already reads without being told anything. The same
   * move `Otlp` makes for spans and `Prom` for metrics: a documented
   * wire, not a dependency.
   */
  def console(min: Level = Level.Info, out: String => Unit = println,
              tracer: Option[Tracer] = None,
              clock: () => Long = () => System.currentTimeMillis,
              logger: String = ""): Handler[Says] =
    to(l => out(json(l)), min, tracer, clock, logger)

  /**
   * The lines as a topic, which is the span treatment exactly:
   * retention is a Policy, shipping is a consumer, and an incident's
   * lines are a READ rather than a grep. The traceId is the record
   * key, so every line of one request is one key.
   */
  def topic(t: Topic, min: Level = Level.Info,
            tracer: Option[Tracer] = None,
            clock: () => Long = () => System.currentTimeMillis,
            logger: String = "",
            ack: Ack = Ack.Received): Handler[Says] =
    to({ l =>
      val key = l.traceId.getOrElse("").getBytes("UTF-8")
      t.append(key, okay.codec.Cbor.write(l), ack): Unit
    }, min, tracer, clock, logger)

  /** the test's handler: the lines, in order, as they were written */
  def collecting(min: Level = Level.Debug,
                 tracer: Option[Tracer] = None,
                 clock: () => Long = () => 0L,
                 logger: String = ""): (Handler[Says], () => Vector[Line]) =
    var seen = Vector.empty[Line]
    (to(l => seen :+= l, min, tracer, clock, logger), () => seen)

  /** a line as the one JSON object a collector reads: the ids at the
    * top level (so a search engine indexes them) and the caller's
    * fields flattened beside them, never nested */
  def json(l: Line): String =
    val sb = new StringBuilder
    def put(x: String): Unit = { sb ++= x; () }
    def str(x: String): String = Json.encode(Schema.SString)(x)
    put("{\"level\":\"" + l.level.toString.toLowerCase + "\"")
    put(",\"at\":" + l.at.toString)
    put(",\"message\":" + str(l.message))
    if l.logger.nonEmpty then put(",\"logger\":" + str(l.logger))
    l.traceId.foreach(t => put(",\"traceId\":" + str(t)))
    l.spanId.foreach(s => put(",\"spanId\":" + str(s)))
    // a field may not shadow a name above: the reserved ones are
    // prefixed rather than dropped, so nothing a caller said is lost
    val reserved = Set("level", "at", "message", "logger", "traceId", "spanId")
    l.fields.foreach { a =>
      val k = if reserved(a.key) then s"field.${a.key}" else a.key
      put("," + str(k) + ":" + str(a.value))
    }
    put("}")
    sb.result()
