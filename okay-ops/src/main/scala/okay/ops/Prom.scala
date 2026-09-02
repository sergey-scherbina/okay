package okay.ops

import okay.persist.{Offsets, Store, Topic}

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
