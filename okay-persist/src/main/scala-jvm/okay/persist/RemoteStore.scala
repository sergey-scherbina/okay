package okay.persist

/**
 * A remote node presented as a `Store` (specs/persist.md,
 * persist-wire-repl): the replication MACHINERY does not change when
 * a replica lives across a wire, because a `Replicated` drives its
 * replicas through the ordinary synchronous `Store`/`Topic` trait,
 * and this adapter answers that trait from a `Wire.Remote`.
 *
 *   val here  = MemoryStore()
 *   val there = RemoteStore(Wire.Remote.connect(host, port, token))
 *   val log   = Replicated("orders", 4, Policy(), Vector(here, there))
 *
 * `Replicated.replicate` is a consumer that writes what it reads —
 * the "replicate-pull" is the remote's Read, verbatim; the eager
 * push is the remote's Append. The coordinator calls these on its
 * own thread, so the remote round trips block there (the okay-pg
 * waist: a blocking access path under an Async engine). This adapter
 * is therefore JVM-only and deliberately synchronous; the async
 * `Wire.Remote` surface stays the access path for everyone else.
 *
 * Only the topics the handshake GRANTED are reachable — asking for a
 * name outside the offer refuses by name, the same capability rule
 * the server enforces.
 */
final class RemoteStore(remote: Wire.Remote) extends Store:

  def topics: Vector[String] = remote.topics

  def topic(name: String, partitions: Int = 1, policy: Policy = Policy.default): Topic =
    RemoteStore.RemoteTopic(remote, name, partitions)

  /** stats do not cross this wire (there is no stats frame): a remote
   * replica's lag is read from the COORDINATOR's `replicaStats`, which
   * is where an operator watches replication anyway */
  def stats: Store.Stats = Store.Stats(Vector.empty)

object RemoteStore:

  private final class RemoteTopic(remote: Wire.Remote, val name: String,
                                  val partitions: Int) extends Topic:
    def append(partition: Int, key: Array[Byte], value: Array[Byte], ack: Ack): Long =
      remote.appendSync(name, partition, key, value, ack)
    def read(partition: Int, from: Long, max: Int): Topic.Read =
      remote.readSync(name, partition, from, max)
    def begin(partition: Int): Long = remote.beginSync(name, partition)
    def end(partition: Int): Long = remote.endSync(name, partition)
    def compact(partition: Int): Unit = remote.compactSync(name, partition)
