package okay.kafka

import java.net.{InetSocketAddress, Socket}

/**
 * A FAST liveness probe, ahead of the Kafka client entirely: the
 * client's own defaults (`request.timeout.ms` 30s,
 * `default.api.timeout.ms` 60s) turn "nothing is listening" into a
 * slow failure that eats munit's own timeout instead of skipping —
 * found 2026-09-02 when the live suites hung to their `munitTimeout`
 * with no broker running rather than skipping in milliseconds. A raw
 * TCP connect with a short deadline answers "is ANYTHING here" in
 * well under a second either way, before the client's generous
 * production-grade retry policy ever gets a chance to run.
 */
object TestKafkaSupport:
  def reachable(bootstrap: String, timeoutMs: Int = 1000): Boolean =
    bootstrap.split(":", 2) match
      case Array(host, portS) =>
        portS.toIntOption.exists { port =>
          val s = new Socket()
          try { s.connect(new InetSocketAddress(host, port), timeoutMs); true }
          catch case _: Exception => false
          finally s.close()
        }
      case _ => false
