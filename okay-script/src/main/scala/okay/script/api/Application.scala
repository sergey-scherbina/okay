package okay.script.api

import okay.codec.Schema
import okay.persist.{Ack, Policy, Store, Topic}

import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters.*

/** JSP's `application` scope: attributes shared by EVERY page of a
 * Site -- the catalog an admin page edits and the index reads, a
 * counter, a feature flag. String values, with `value`/`put` typed
 * through a Schema's JSON so pages trade DATA, never objects: each
 * page is its own compilation unit, and a `Product` declared in two
 * pages is two classes -- their JSON is one. See specs/okay-script.md
 * "Application scope".
 */
trait Application:
  def get(key: String): Option[String]
  def set(key: String, value: String): Unit
  def remove(key: String): Unit
  def attributes: Map[String, String]

  /** the attribute decoded by the Schema; damage or absence is `None` */
  def value[A](key: String)(using Schema[A]): Option[A] =
    get(key).flatMap(s => okay.codec.Codecs.readJson[A](s).toOption)

  def put[A](key: String, a: A)(using Schema[A]): Unit = set(key, okay.codec.Codecs.writeJson(a))

object Application:
  /** outside a Site (a bare `render`) there is still one, in memory,
   * shared by every bare render in the process */
  val detached: Application = memory()

  private val local: ThreadLocal[Application] = ThreadLocal.withInitial(() => detached)

  def current: Application = local.get()

  def setCurrent(a: Application): Unit = local.set(a)

  def memory(): Application = new Memory

  /** written through to an okay-persist topic (keyed, compacted: one
   * key per attribute, an empty value the tombstone, `Ack.Durable`)
   * and rebuilt from it on open -- the catalog survives a restart,
   * and `Replicated`/`RemoteStore` topics share it across nodes the
   * way `Sessions.shared` does */
  def persisted(store: Store, topic: String = "__application"): Application =
    new Persisted(store.topic(topic, 1, Policy(compact = true)))

  private final class Memory extends Application:
    private val m = new ConcurrentHashMap[String, String]
    def get(key: String): Option[String] = Option(m.get(key))
    def set(key: String, value: String): Unit = m.put(key, value): Unit
    def remove(key: String): Unit = m.remove(key): Unit
    def attributes: Map[String, String] = m.asScala.toMap

  private final class Persisted(topic: Topic) extends Application:
    private val index = new Memory
    rebuild()

    private def rebuild(): Unit =
      for p <- 0 until topic.partitions do
        var from = topic.begin(p)
        var going = true
        while going do
          topic.read(p, from, 512) match
            case Topic.Read.TooEarly(b) => from = b
            case Topic.Read.Records(rs) =>
              if rs.isEmpty then going = false
              else
                for r <- rs do
                  val k = new String(r.key, "UTF-8")
                  if r.value.isEmpty then index.remove(k) else index.set(k, new String(r.value, "UTF-8"))
                from = rs.last.offset + 1

    def get(key: String): Option[String] = index.get(key)
    def set(key: String, value: String): Unit =
      index.set(key, value)
      topic.append(key.getBytes("UTF-8"), value.getBytes("UTF-8"), Ack.Durable): Unit
    def remove(key: String): Unit =
      index.remove(key)
      topic.append(key.getBytes("UTF-8"), Array.empty[Byte], Ack.Durable): Unit
    def attributes: Map[String, String] = index.attributes
