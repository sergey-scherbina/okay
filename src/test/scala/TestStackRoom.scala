package okay

/**
 * specs/cont-stack.md Layer 3, the JVM reader. The core's tests run
 * against the PACKAGED jar with native access enabled (build.sbt), so
 * on JDK 22+ the class in play is the `META-INF/versions/22/` variant
 * and it reads; on 17 and 21 it is the root and answers −1. Both are
 * asserted, on the JDK the fork runs on.
 */
class TestStackRoom extends munit.FunSuite:

  private def deeper(n: Int): Long = if n == 0 then StackRoom.sp() else deeper(n - 1) + 0

  /** the `(os, arch)` pairs whose `ucontext_t` layout was MEASURED
   * (jdk22/StackRoom.scala, specs/cont-stack.md Decision 13) — the JVM
   * must read on these from 22 up, and nowhere else */
  private val measured = Set(("Mac OS X", "aarch64"), ("Linux", "aarch64"), ("Linux", "amd64"))

  /** musl (Alpine) has no `getcontext`, so a Linux JVM on it counts;
   * its dynamic loader is how the test tells it from glibc */
  private val musl =
    val lib = java.io.File("/lib")
    lib.isDirectory && Option(lib.list()).exists(_.exists(_.startsWith("ld-musl-")))

  private val readsHere =
    Runtime.version().feature() >= 22 && !musl && measured((System.getProperty("os.name"), System.getProperty("os.arch")))

  test("the reader answers what this JDK must: bounds and a pointer between them on 22+, −1 below") {
    val sp = StackRoom.sp()
    val top = StackRoom.top()
    val floor = StackRoom.floor()
    println(s"TestStackRoom: JDK ${Runtime.version()} sp=$sp top=$top floor=$floor")
    if readsHere then
      assert(sp > 0 && top > 0 && floor > 0, "unreadable on a JVM and layout that can read")
      assert(floor < sp && sp < top, s"pointer outside the bounds: $floor < $sp < $top")
      assert(top - sp < (8L << 20), s"more than 8 MB used on a fresh thread: ${top - sp}")
      val deep = deeper(200)
      assert(deep < sp, s"200 frames deeper read a higher pointer: $deep vs $sp")
      assert(sp - deep < 200L * 4096, s"200 frames took ${sp - deep} bytes")
    else
      assertEquals(sp, -1L)
      assertEquals(top, -1L)
      assertEquals(floor, -1L)
  }

  test("a missing symbol falls through to the count instead of failing: musl has no getcontext") {
    // the reader is built with the symbol hidden from the lookup — the
    // state an Alpine JVM is in — and must answer "cannot read", while
    // hiding nothing it needs still reads
    assert(!StackRoom.readableWithout("getcontext"), "read with getcontext absent")
    assert(!StackRoom.readableWithout("pthread_self"), "read with pthread_self absent")
    assertEquals(StackRoom.readableWithout("no-such-symbol"), readsHere)
  }
