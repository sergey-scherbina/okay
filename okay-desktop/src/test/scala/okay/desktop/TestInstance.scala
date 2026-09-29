package okay.desktop

import java.nio.file.Files
import java.util.concurrent.{CountDownLatch, TimeUnit}

/**
 * app-in-process (specs/app-in-process.md): one copy of the app per data
 * folder — a lock, and `front` over a Unix-domain socket in the folder.
 * No port.
 */
class TestInstance extends okay.testkit.Munit.Diagnosed:

  test("the first claim holds; a second in the same folder is refused and its front reaches the first") {
    // a short path: a Unix-domain socket's path is capped (~104 bytes on a Mac)
    val dir = Files.createTempDirectory("oki")
    val first = Instance.claim(dir)
    assert(first.isDefined, "the first copy")
    assertEquals(Instance.claim(dir), None, "a second copy is refused")
    val fronted = CountDownLatch(1)
    assert(first.get.listen(() => fronted.countDown()), "the first copy listens")
    assert(Instance.front(dir), "the second copy's word is heard")
    assert(fronted.await(5, TimeUnit.SECONDS), "the first copy came to the front")
    first.get.release()
    val again = Instance.claim(dir)
    assert(again.isDefined, "once it ends, the next copy is the first")
    again.get.release()
  }

  test("front with nobody listening says so") {
    assert(!Instance.front(Files.createTempDirectory("oki")))
  }

/** binds real ports on this computer: Live (okay's AGENTS.md "no flaky
 * tests in the default gate" — every suite that binds a port is tagged) */
class TestFreePort extends okay.testkit.Munit.Diagnosed:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(munit.Tag("Live")))

  test("a port held on the loopback alone is not free (Docker's 127.0.0.1:8099), nor one held on every address") {
    val loop = java.net.ServerSocket()
    loop.setReuseAddress(false)
    loop.bind(java.net.InetSocketAddress(java.net.InetAddress.getLoopbackAddress, 0), 1)
    try assert(!Desktop.free(loop.getLocalPort), s"${loop.getLocalPort} is held") finally loop.close()
    val all = java.net.ServerSocket(0)
    try assert(!Desktop.free(all.getLocalPort), s"${all.getLocalPort} is held") finally all.close()
  }

  test("the preferred port when free, another when something holds it") {
    val held = java.net.ServerSocket(0, 1, java.net.InetAddress.getByName("127.0.0.1"))
    try
      val p = held.getLocalPort
      val got = Desktop.freePort(p)
      note(s"held $p, got $got")
      assert(got != p && got > 0, s"$got")
    finally held.close()
  }
