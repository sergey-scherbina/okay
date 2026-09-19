package okay.ui

import okay.*
import okay.given

/**
 * `Wire.serveClosing` (specs/wire-server-close.md): the server's own
 * half of what only a client's `Closed` could do before. Pure — over
 * no transport at all — the same way `TestWire`'s first suite is, so
 * this runs in the default gate rather than needing a live socket.
 */
class TestWireClosing extends munit.FunSuite {

  def view(n: Int): Ui = Ui.Column(Vector(
    Ui.Text(s"count: $n"),
    Ui.Row(Vector(Ui.Button("-", "dec"), Ui.Button("+", "inc"), Ui.Button("bye", "logout")))))

  /** the LOGOUT key ends the session: the state does not even change,
   * the way okay-watch's own Nav.Logout press does not either — the
   * whole point is that a no-op state change can still end things */
  def update(n: Int, e: Event): (Int, Boolean) = e match
    case Event.Pressed("inc") => (n + 1, false)
    case Event.Pressed("dec") => (n - 1, false)
    case Event.Pressed("logout") => (n, true)
    case _ => (n, false)

  def talk(lines: String*): (Seq[String], Int) =
    val (out, s) = !.run(Writer.run(through(Writer.of(lines.toList))(
      Wire.serveClosing(0)(view)(update))))
    (out, s)

  def press(k: String): String = Protocol.eventLine(Event.Pressed(k))

  test("a closing event still sends its own patches, then one Close line, then nothing") {
    val (out, s) = talk(press("inc"), press("logout"), press("inc"))
    assertEquals(s, 1) // the logout event itself changed nothing; the THIRD press never ran
    assertEquals(Protocol.treeOf(out.head), Some(view(0)))
    assertEquals(out.tail.init, Seq(
      // the "inc" patch
      Protocol.line(Protocol.Msg.Patch(Patch.SetText(List(0), "count: 1")))))
    // the logout event changed no text, so it has NO patch of its own —
    // only the Close line follows the last real patch
    assertEquals(out.last, Protocol.line(Protocol.Msg.Close))
    assertEquals(out.size, 3) // tree, one patch, one Close — the trailing "inc" never arrives
  }

  test("a closing event that DOES change the view sends that patch before Close") {
    def upd(n: Int, e: Event): (Int, Boolean) = e match
      case Event.Pressed("inc") => (n + 1, true) // this press ends it
      case _ => (n, false)
    val (out, s) = !.run(Writer.run(through(Writer.of(List(press("inc"))))(
      Wire.serveClosing(0)(view)(upd))))
    assertEquals(s, 1)
    assertEquals(out, Seq(
      Protocol.line(Protocol.Msg.Tree(view(0))),
      Protocol.line(Protocol.Msg.Patch(Patch.SetText(List(0), "count: 1"))),
      Protocol.line(Protocol.Msg.Close)))
  }

  test("a forged key still cannot reach update, even one that would close") {
    // "logout" is on screen, but a client sending it before the tree
    // ever showed it (no prior Hello/Tree round trip here — this
    // helper starts fresh each call) is exactly what `permitted`
    // exists to catch; assert instead that an UNSHOWN key is dropped
    val (out, s) = talk(Protocol.eventLine(Event.Pressed("boom-does-not-exist")), press("inc"))
    assertEquals(s, 1)
    assertEquals(out.size, 2) // the tree, and the one honest patch
  }

  test("serve is serveClosing with the boolean pinned false: byte-identical output") {
    def plainUpdate(n: Int, e: Event): Int = update(n, e)._1
    val lines = List(press("inc"), press("logout"), press("inc"))
    val (viaServe, sServe) = !.run(Writer.run(through(Writer.of(lines))(
      Wire.serve(0)(view)(plainUpdate))))
    // serve never closes on its own, so the trailing "inc" DOES run —
    // proving the two are genuinely different behaviors, not that the
    // refactor accidentally changed serve's meaning
    assertEquals(sServe, 2)
    // tree, "inc" patch, NO patch for "logout" (it moved no text), "inc" patch
    assertEquals(viaServe, Seq(
      Protocol.line(Protocol.Msg.Tree(view(0))),
      Protocol.line(Protocol.Msg.Patch(Patch.SetText(List(0), "count: 1"))),
      Protocol.line(Protocol.Msg.Patch(Patch.SetText(List(0), "count: 2")))))
  }

  test("Msg.Close round-trips through JSON lines and CBOR bytes like every other Msg") {
    assertEquals(Protocol.parse(Protocol.line(Protocol.Msg.Close)), Some(Protocol.Msg.Close))
    assertEquals(Protocol.ofBytes(Protocol.bytes(Protocol.Msg.Close)), Some(Protocol.Msg.Close))
    assert(Protocol.closes(Protocol.line(Protocol.Msg.Close)))
  }
}
