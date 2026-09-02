package okay.llm

/** what a live suite treats as "the gateway went away": the wire's
 * own failures at any depth, and nothing else */
class TestLiveSkip extends munit.FunSuite {

  test("an I/O failure anywhere in the cause chain is the wire dropping; the root is named") {
    val eof = new java.io.EOFException("EOF reached while reading")
    val parser = new java.io.IOException("HTTP/1.1 header parser received no bytes", eof)
    val wrapped = new RuntimeException("the turn failed", parser)
    assert(Live.wireDropped(wrapped))
    assert(Live.wireDropped(parser))
    assertEquals(Live.root(wrapped).getMessage, "EOF reached while reading")
    assert(Live.wireDropped(new java.net.ConnectException("Connection refused")))
    assert(Live.wireDropped(new java.net.http.HttpTimeoutException("request timed out")))
  }

  test("a wrong answer is not the wire: assertion and plain failures still fail") {
    assert(!Live.wireDropped(new AssertionError("expected 'weld', got 'wild'")))
    assert(!Live.wireDropped(new RuntimeException("the model refused", new IllegalStateException("no tools"))))
    // a self-referential cause chain terminates
    val loop = new RuntimeException("loop")
    assert(!Live.wireDropped(loop))
    assertEquals(Live.root(loop), loop)
  }
}
