package okay.cluster

import okay.{Aggregator, Chunks}
import okay.codec.Json

/**
 * The ONE shared-source definition both ends of the acceptance run
 * compile (specs/cluster.md): the source, the statistic, the wire
 * frames and the expected answer. A JS client under Node and a JVM
 * server use exactly this object — the cross-platform policy's
 * acceptance test is that they agree.
 */
object Acceptance:

  val agg = Aggregator.variance[Double]

  /** the shared source: a value, so both ends can recompute it */
  def source: Chunks[Double] = Chunks.map(Chunks.range(0, 1000, 16))(x => x * 0.5 + 1)

  /** the chunks as wire frames, one Schema-encoded JSON line each */
  def frames: List[String] =
    def go(rest: Chunks[Double], acc: List[String]): List[String] =
      Chunks.pull(rest) match
        case Some((c, r)) => go(r, Json.write(c.toList) :: acc)
        case None => acc.reverse
    go(source, Nil)

  /** what the server's fold must answer — computed locally, same source */
  def expected: Double = agg.present(Chunks.fold(source)(using agg.fold))
