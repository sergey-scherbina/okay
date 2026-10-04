package okay2.persist

/**
 * A remote node presented as a `Store` (okay-persist's RemoteStore.scala;
 * specs/persist.md, persist-wire-repl): the replication MACHINERY does
 * not change when a replica lives across a wire, because `Replicated`
 * drives its replicas through the ordinary synchronous `Store`/`Topic`
 * trait, and this adapter answers that trait from a `Wire.Remote`.
 *
 *   val here  = new MemoryStore
 *   val there = new RemoteStore(Wire.Remote.connect(host, port, token))
 *   val log   = Replicated("orders", 4, Policy(), Vector(here, there))
 *
 * The coordinator calls these on its own thread, so the remote round
 * trips block there; this adapter is therefore JVM-only and deliberately
 * synchronous. Only the topics the handshake GRANTED are reachable.
 */
final class RemoteStore(remote: Wire.Remote) extends Store {

  def topics: Vector[String] = remote.topics

  def topic(name: String, partitions: Int, policy: Policy): Topic =
    new RemoteStore.RemoteTopic(remote, name, partitions)

  /** stats do not cross this wire (there is no stats frame): a remote
   * replica's lag is read from the COORDINATOR's `replicaStats` */
  def stats: Store.Stats = Store.Stats(Vector.empty)
}

object RemoteStore {

  private final class RemoteTopic(remote: Wire.Remote, val name: String, val partitions: Int) extends Topic {
    def append(partition: Int, key: Array[Byte], value: Array[Byte], ack: Ack): Long =
      remote.appendSync(name, partition, key, value, ack)
    def read(partition: Int, from: Long, max: Int): Topic.Read = remote.readSync(name, partition, from, max)
    def begin(partition: Int): Long = remote.beginSync(name, partition)
    def end(partition: Int): Long = remote.endSync(name, partition)
    def compact(partition: Int): Unit = remote.compactSync(name, partition)
  }
}
