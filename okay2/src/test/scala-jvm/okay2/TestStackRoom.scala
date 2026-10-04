package okay2

/**
 * okay2's stack reader, from the JAR (okay2-stackroom-jdk22, the Scala 3 core's TestStackRoom): on JDK 22+ with a
 * measured layout the Multi-Release variant must have been picked and read a pointer between the bounds; below, or
 * on musl, every reading is −1. The tests run against okay2's packaged jar (`multiRelease`), so a pass here is the
 * JVM's own version selection, not a classes directory.
 */
class TestStackRoom extends munit.FunSuite {

  private def deeper(n: Int): Long = if (n == 0) StackRoom.sp() else deeper(n - 1) + 0

  private val measured = Set(("Mac OS X", "aarch64"), ("Linux", "aarch64"), ("Linux", "amd64"))

  private val musl = {
    val lib = new java.io.File("/lib")
    lib.isDirectory && Option(lib.list()).exists(_.exists(_.startsWith("ld-musl-")))
  }

  private val readsHere =
    Runtime.version().feature() >= 22 && !musl && measured((System.getProperty("os.name"), System.getProperty("os.arch")))

  test("the reader answers what this JDK must: bounds and a pointer between them on 22+, -1 below") {
    val sp = StackRoom.sp()
    val top = StackRoom.top()
    val floor = StackRoom.floor()
    if (readsHere) {
      assert(sp > 0 && top > 0 && floor > 0, s"unreadable on a JVM and layout that can read: sp=$sp top=$top floor=$floor")
      assert(floor < sp && sp < top, s"pointer outside the bounds: $floor < $sp < $top")
      assert(top - sp < (8L << 20), s"more than 8 MB used on a fresh thread: ${top - sp}")
      val deep = deeper(200)
      assert(deep < sp, s"200 frames deeper read a higher pointer: $deep vs $sp")
      assert(sp - deep < 200L * 4096, s"200 frames took ${sp - deep} bytes")
    } else {
      assertEquals(sp, -1L)
      assertEquals(top, -1L)
      assertEquals(floor, -1L)
    }
  }

  test("a missing symbol falls through to the count instead of failing: musl has no getcontext") {
    assert(!StackRoom.readableWithout("getcontext"), "read with getcontext absent")
    assert(!StackRoom.readableWithout("pthread_self"), "read with pthread_self absent")
    assertEquals(StackRoom.readableWithout("no-such-symbol"), readsHere)
  }

  test("the class came out of okay2's jar, so the JVM chose its version") {
    val src = StackRoom.getClass.getProtectionDomain.getCodeSource.getLocation.toString
    assert(src.endsWith(".jar"), s"StackRoom loaded from $src, not a jar: the Multi-Release swap is untested")
  }
}
