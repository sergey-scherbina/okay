package okay.persist

/**
 * The queue bridges (specs/data.md, "Queues"): per-message-ack
 * brokers (RabbitMQ/AMQP, SQS, NATS, Pulsar, MQTT) are DELIVERY
 * machinery — per-message ack, redelivery, competing consumers, no
 * offsets — a shape the log deliberately is NOT. A native `Queue`
 * seam is rejected: it would mirror a lossy shape into the core and
 * every consumer would rebuild the log's properties on top. Instead,
 * two BRIDGES and no new seam:
 *
 *  - INGRESS turns a queue into an entry ramp to the log: consume,
 *    append into a topic, ack AFTER the append. At-least-once, and
 *    everything the queue could not do — replay, audit, fan-out to
 *    late consumers — is restored one hop in. Redelivery after a
 *    lost ack appends the message twice; the record carries the
 *    broker's message id, and a downstream consumer dedups by it
 *    (WithKey's shape again). Dedup is the consumer's, one hop in.
 *
 *  - EGRESS turns a topic into a publisher: a consumer reads from an
 *    offset and publishes outward, the offset (journaled by the
 *    caller) making it resumable. A broker that dedups on the
 *    carried message id gets exactly-once OUTCOME; the rest are
 *    at-least-once, said out loud.
 *
 * The SPIs below are the whole coupling to a real broker — an engine
 * adapter (RabbitMQ, SQS, ...) implements `Source`/`Sink` and is a
 * named DEPLOYMENT, not a core seam.
 */
object Queues:

  /** a message pulled from a foreign broker: an opaque id the broker
   * assigned (the dedup handle) and the payload bytes */
  final case class Incoming(id: String, value: Array[Byte])

  /** the INGRESS SPI a broker adapter implements: pull the next
   * available message (None = nothing right now), and ack by id —
   * ack is called only AFTER the append is durable */
  trait Source:
    def poll(): Option[Incoming]
    def ack(id: String): Unit

  /** the EGRESS SPI: publish one payload outward under its message
   * id; a broker that dedups on that id gives exactly-once outcome */
  trait Sink:
    def publish(id: String, value: Array[Byte]): Unit

  /**
   * INGRESS: drain the source into one topic partition, acking each
   * message only AFTER its append returns durable — so a crash
   * between append and ack redelivers and re-appends (at-least-once),
   * never drops. The broker id becomes the record KEY, so a
   * downstream consumer can dedup by it. Stops when the source is
   * empty or `max` messages have been bridged; returns the count.
   */
  def ingress(source: Source, topic: Topic, partition: Int = 0,
              max: Int = Int.MaxValue): Int =
    var bridged = 0
    var going = true
    while going && bridged < max do
      source.poll() match
        case None => going = false
        case Some(msg) =>
          // append FIRST, ack SECOND: the order is the whole
          // at-least-once guarantee (a crash between them redelivers)
          topic.append(partition, msg.id.getBytes("UTF-8"), msg.value, Ack.Durable): Unit
          source.ack(msg.id)
          bridged += 1
    bridged

  /**
   * EGRESS: publish records from `from` outward, in order, up to
   * `max`; returns the next offset to resume from, which the caller
   * journals (an Offsets topic, say) so a restart continues where it
   * left off. A crash after a publish but before the offset is
   * journaled re-publishes on resume — at-least-once, collapsed to
   * exactly-once by a broker that dedups on the record's key.
   */
  def egress(topic: Topic, sink: Sink, from: Long, partition: Int = 0,
             max: Int = 256): Long =
    topic.read(partition, from, max) match
      case Topic.Read.TooEarly(begin) =>
        // history the publisher needed is gone: resume at the live
        // begin rather than silently skipping or looping
        begin
      case Topic.Read.Records(rs) =>
        var next = from
        for r <- rs do
          sink.publish(new String(r.key, "UTF-8"), r.value)
          next = r.offset + 1
        next

  /**
   * The one-hop dedup a downstream consumer applies: keep the FIRST
   * record seen per message-id key, dropping redelivery duplicates.
   * This is where at-least-once becomes effectively-once for the
   * reader — the property the bridge restores that the queue lacked.
   */
  def dedup(records: Vector[Record]): Vector[Record] =
    val seen = scala.collection.mutable.HashSet.empty[String]
    records.filter(r => seen.add(new String(r.key, "UTF-8")))
