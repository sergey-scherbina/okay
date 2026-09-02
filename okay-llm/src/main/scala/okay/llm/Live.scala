package okay.llm

/**
 * What "the gateway is absent" means to a LIVE test (live-skip-on-
 * gateway-loss, 2026-09-02): not only a probe that fails before the
 * test, but a wire that breaks during it — a shared local gateway
 * under load closes connections ("HTTP/1.1 header parser received
 * no bytes"), times out, or refuses. Any of those, anywhere in an
 * exception's cause chain, is the environment, not the code; a live
 * suite skips on it, named, and keeps failing on a wrong ANSWER.
 */
object Live {
  /** the innermost cause */
  def root(e: Throwable): Throwable =
    var t = e
    while t.getCause != null && (t.getCause ne t) do t = t.getCause
    t

  /** did the wire break: an I/O failure at any depth */
  def wireDropped(e: Throwable): Boolean =
    var t: Throwable | Null = e
    var found = false
    while !found && t != null do
      found = t.isInstanceOf[java.io.IOException] || t.isInstanceOf[java.net.http.HttpTimeoutException]
      t = if t.nn.getCause eq t then null else t.nn.getCause
    found
}
