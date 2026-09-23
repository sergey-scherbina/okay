package okay.scalus

/**
 * Wires by name, in this JVM — how an engine adapter whose options are
 * STRINGS (Spark's `relay`, a Flink source serialized to a task manager)
 * reaches a transport that is not a socket: a recorded session, a fake
 * relay, an embedded one. `connect("host:port")` is TCP;
 * `connect("registered:<name>")` is what was registered.
 */
object Relays:
  private val wires = java.util.concurrent.ConcurrentHashMap[String, () => Wire]()

  def register(name: String, make: () => Wire): Unit = wires.put(name, make): Unit

  def apply(name: String): Wire =
    Option(wires.get(name)).map(_()).getOrElse(throw IllegalArgumentException(s"no relay registered as '$name'"))

  def connect(relay: String): Wire =
    if relay.startsWith("registered:") then apply(relay.stripPrefix("registered:"))
    else relay.split(':') match
      case Array(host, port) => Wire.tcp(host, port.toInt)
      case _ => throw IllegalArgumentException(s"relay '$relay' is neither host:port nor registered:<name>")
