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

  test("the reader answers what this JDK must: bounds and a pointer between them on 22+, −1 below") {
    val sp = StackRoom.sp()
    val top = StackRoom.top()
    val floor = StackRoom.floor()
    println(s"TestStackRoom: JDK ${Runtime.version()} sp=$sp top=$top floor=$floor")
    if Runtime.version().feature() >= 22 && System.getProperty("os.name").startsWith("Mac") && System.getProperty("os.arch") == "aarch64" then
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
