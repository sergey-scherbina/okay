package okay2.persist

/**
 * A QUEUE IS NOT A LOG (okay-persist's Queues.scala): the two bridges
 * between a broker's at-least-once delivery and the log. `ingress` moves
 * messages into a topic and acks them only after the append is durable;
 * `egress` publishes a topic slice and answers the next offset; `dedup`
 * keeps the first record per message id — a redelivery is a duplicate,
 * and the id is what says so.
 */
object Queues {

  final case class Incoming(id: String, value: Array[Byte])

  trait Source {
    def poll(): Option[Incoming]
    def ack(id: String): Unit
  }

  trait Sink {
    def publish(id: String, value: Array[Byte]): Unit
  }

  def ingress(source: Source, topic: Topic, partition: Int = 0, max: Int = Int.MaxValue): Int = {
    var bridged = 0
    var going = true
    while (going && bridged < max) {
      source.poll() match {
        case None => going = false
        case Some(msg) =>
          topic.append(partition, msg.id.getBytes("UTF-8"), msg.value, Ack.Durable)
          source.ack(msg.id)
          bridged += 1
      }
    }
    bridged
  }

  def egress(topic: Topic, sink: Sink, from: Long, partition: Int = 0, max: Int = 256): Long =
    topic.read(partition, from, max) match {
      case Topic.Read.TooEarly(begin) => begin
      case Topic.Read.Records(rs) =>
        var next = from
        for (r <- rs) {
          sink.publish(new String(r.key, "UTF-8"), r.value)
          next = r.offset + 1
        }
        next
    }

  def dedup(records: Vector[Record]): Vector[Record] = {
    val seen = scala.collection.mutable.HashSet.empty[String]
    records.filter(r => seen.add(new String(r.key, "UTF-8")))
  }
}
