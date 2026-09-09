package okay.ops

import okay.persist.{Offsets, Store, Topic}
import okay.resilience.{Breaker, Bulkhead, Limiter, Reporting}

/**
 * `Store.Stats` (and, optionally, `Offsets`) as Prometheus's text
 * exposition format — a PURE mapping (specs/ops.md), the same move
 * `Otlp.body` makes for spans: no client library, a documented
 * string. Every scraper that speaks the format (a Prometheus
 * server, an OTEL Collector's prometheus receiver, a Kubernetes
 * `ServiceMonitor`) reads this without knowing this stack exists.
 */
object Prom:

  private def esc(s: String): String =
    s.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n")

  private def line(metric: String, topic: String, partition: Int, value: Long): String =
    s"""$metric{topic="${esc(topic)}",partition="$partition"} $value"""

  /** the gauges any `Store.Stats` carries; `lagOf`, when given,
   * names the consumer groups to report lag for — a store has no
   * registry of its own groups, so the caller names them (Out of
   * scope in specs/ops.md) */
  def render(stats: Store.Stats, lagOf: Vector[(String, Offsets, Vector[Topic])] = Vector.empty): String =
    val sb = new StringBuilder
    def metric(name: String, help: String, tpe: String)(rows: => Vector[String]): Unit =
      val body = rows
      if body.nonEmpty then
        sb ++= s"# HELP $name $help\n# TYPE $name $tpe\n"
        body.foreach(r => sb ++= r += '\n')

    metric("okay_persist_partition_begin", "the lowest live offset", "gauge") {
      stats.topics.flatMap(t => t.partitions.map(p => line("okay_persist_partition_begin", t.name, p.partition, p.begin)))
    }
    metric("okay_persist_partition_end", "the next offset to be written", "gauge") {
      stats.topics.flatMap(t => t.partitions.map(p => line("okay_persist_partition_end", t.name, p.partition, p.end)))
    }
    metric("okay_persist_partition_bytes", "bytes held on disk (or in memory) for this partition", "gauge") {
      stats.topics.flatMap(t => t.partitions.map(p => line("okay_persist_partition_bytes", t.name, p.partition, p.bytes)))
    }
    metric("okay_persist_partition_segments", "segment count for this partition", "gauge") {
      stats.topics.flatMap(t => t.partitions.map(p => line("okay_persist_partition_segments", t.name, p.partition, p.segments.toLong)))
    }
    if lagOf.nonEmpty then
      sb ++= "# HELP okay_persist_consumer_lag end offset minus the committed offset, per group\n# TYPE okay_persist_consumer_lag gauge\n"
      for (group, offsets, topics) <- lagOf; t <- topics do
        val g = Prom.esc(group)
        sb ++= s"""okay_persist_consumer_lag{group="$g",topic="${esc(t.name)}"} ${offsets.lag(group, t)}"""
        sb += '\n'
    sb.result()

  /**
   * The resilience pieces' stats (specs/resilience.md, stage 1):
   * `name` is the label, gauges for what IS, counters for what
   * happened. A breaker's state is a gauge 0/1/2 = closed/open/
   * half-open, the usual Prometheus shape for a small enum.
   */
  def guards(pieces: Vector[Reporting[?]]): String =
    val sb = new StringBuilder
    def metric(name: String, help: String, tpe: String)(rows: Vector[String]): Unit =
      if rows.nonEmpty then
        sb ++= s"# HELP $name $help\n# TYPE $name $tpe\n"
        rows.foreach(r => sb ++= r += '\n')
    def row(metric: String, name: String, value: Long): String =
      s"""$metric{name="${esc(name)}"} $value"""

    // each piece's stats is read ONCE, and sorted by its type — a
    // type pattern, not a cast: the match checks it
    val read = pieces.map(p => (p.name, p.stats))
    val breakers = read.collect { case (n, s: Breaker.Stats) => (n, s) }
    val bulkheads = read.collect { case (n, s: Bulkhead.Stats) => (n, s) }
    val limiters = read.collect { case (n, s: Limiter.Stats) => (n, s) }

    metric("okay_breaker_state", "0 closed, 1 open, 2 half-open", "gauge")(
      breakers.map((n, s) => row("okay_breaker_state", n, s.state.ordinal.toLong)))
    metric("okay_breaker_consecutive_failures", "failures in a row, as of now", "gauge")(
      breakers.map((n, s) => row("okay_breaker_consecutive_failures", n, s.consecutiveFailures.toLong)))
    metric("okay_breaker_calls_total", "calls admitted", "counter")(
      breakers.map((n, s) => row("okay_breaker_calls_total", n, s.calls)))
    metric("okay_breaker_failures_total", "admitted calls that failed", "counter")(
      breakers.map((n, s) => row("okay_breaker_failures_total", n, s.failures)))
    metric("okay_breaker_rejected_total", "calls refused while open", "counter")(
      breakers.map((n, s) => row("okay_breaker_rejected_total", n, s.rejected)))
    metric("okay_breaker_opened_total", "times the circuit opened", "counter")(
      breakers.map((n, s) => row("okay_breaker_opened_total", n, s.opened)))

    metric("okay_bulkhead_permits", "permits configured", "gauge")(
      bulkheads.map((n, s) => row("okay_bulkhead_permits", n, s.permits.toLong)))
    metric("okay_bulkhead_in_flight", "permits held now", "gauge")(
      bulkheads.map((n, s) => row("okay_bulkhead_in_flight", n, s.inFlight.toLong)))
    metric("okay_bulkhead_waiting", "callers parked for a permit", "gauge")(
      bulkheads.map((n, s) => row("okay_bulkhead_waiting", n, s.waiting.toLong)))
    metric("okay_bulkhead_rejected_total", "callers refused, queue full", "counter")(
      bulkheads.map((n, s) => row("okay_bulkhead_rejected_total", n, s.rejected)))

    metric("okay_limiter_keys", "buckets alive", "gauge")(
      limiters.map((n, s) => row("okay_limiter_keys", n, s.keys.toLong)))
    metric("okay_limiter_admitted_total", "calls that took a token", "counter")(
      limiters.map((n, s) => row("okay_limiter_admitted_total", n, s.admitted)))
    metric("okay_limiter_delayed_total", "admitted calls that parked for their token", "counter")(
      limiters.map((n, s) => row("okay_limiter_delayed_total", n, s.delayed)))
    metric("okay_limiter_rejected_total", "calls refused, no token within the wait", "counter")(
      limiters.map((n, s) => row("okay_limiter_rejected_total", n, s.rejected)))
    sb.result()

  /** a connection pool's standing, one row per gauge, named by the
   * pool (persistence-e2e): `okay.sql.Pool.stats`, read once */
  def pools(pieces: Vector[(String, () => okay.sql.Pool.Stats)]): String =
    val sb = new StringBuilder
    val read = pieces.map((n, f) => (n, f()))
    def metric(name: String, help: String)(value: okay.sql.Pool.Stats => Long): Unit =
      if read.nonEmpty then
        sb ++= s"# HELP $name $help\n# TYPE $name gauge\n"
        read.foreach((n, s) => sb ++= s"""$name{name="${esc(n)}"} ${value(s)}""" += '\n')
    metric("okay_pool_size", "connections at most")(_.size.toLong)
    metric("okay_pool_idle", "connections open and free")(_.idle.toLong)
    metric("okay_pool_busy", "connections borrowed")(_.busy.toLong)
    metric("okay_pool_waiting", "borrowers waiting for one")(_.waiting.toLong)
    metric("okay_pool_created_total", "connections ever opened")(_.created)
    sb.result()

  /** a saga's standing: its phase as a labelled gauge (1 at the phase
   * it is in) and its progress in steps (persistence-e2e) */
  def sagas(pieces: Vector[() => okay.persist.Saga.Status]): String =
    val sb = new StringBuilder
    val read = pieces.map(_())
    if read.nonEmpty then
      sb ++= "# HELP okay_saga_phase 1 at the phase the saga is in\n# TYPE okay_saga_phase gauge\n"
      read.foreach(s => sb ++= s"""okay_saga_phase{id="${esc(s.id)}",phase="${esc(s.phase)}"} 1""" += '\n')
      sb ++= "# HELP okay_saga_steps_done steps completed forward\n# TYPE okay_saga_steps_done gauge\n"
      read.foreach(s => sb ++= s"""okay_saga_steps_done{id="${esc(s.id)}"} ${s.done.length}""" += '\n')
      sb ++= "# HELP okay_saga_steps_undone steps compensated\n# TYPE okay_saga_steps_undone gauge\n"
      read.foreach(s => sb ++= s"""okay_saga_steps_undone{id="${esc(s.id)}"} ${s.undone.length}""" += '\n')
    sb.result()

