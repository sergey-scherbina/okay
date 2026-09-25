package okay.sql

/**
 * scalajs-arraydeque-null (2026-09-25): `java.util.ArrayDeque` on
 * Scala.js 1.22 LOSES elements when it grows while wrapped. Its
 * `ensureCapacityForAdd`, in the branch for a full buffer that wraps
 * (`startIndex == endIndex`, not 0), builds a NEW array and copies only
 * `[0, endIndex)` to its upper half. The segment `[startIndex,
 * oldCapacity)` is never copied, and those slots read back as null.
 * Found when `Typed.fits`' worklist failed on JS alone with
 * `MatchError: null`. Cross code here uses `scala.collection.mutable.Stack`.
 *
 * Pinned BOTH ways: the JDK's order on the JVM and Native, today's
 * nulls on Scala.js. When Scala.js fixes it, the JS arm goes red, and
 * that is the signal to drop the workaround's comments.
 */
class ProbeScalaJsArrayDeque extends munit.FunSuite:

  val onJs: Boolean = System.getProperty("java.vm.name") == "Scala.js"

  def ints(xs: Seq[Int]): List[Integer] = xs.map(Integer.valueOf).toList

  test("the smallest case: addFirst, 15 addLast (full and wrapped at 16), one more addLast, pollFirst") {
    val d = java.util.ArrayDeque[Integer]()
    d.addFirst(0)
    for i <- 1 to 15 do d.addLast(i)
    d.addLast(16)                                  // grows while wrapped
    val first = d.pollFirst()
    if onJs then assertEquals(first, null, "Scala.js fixed it: see this suite's comment")
    else assertEquals(first, Integer.valueOf(0))
  }

  test("as a stack: 33 pushes and 33 pops — sixteen nulls on Scala.js where the wrapped half was dropped") {
    val d = java.util.ArrayDeque[Integer]()
    for i <- 1 to 33 do d.push(i)
    val got = List.fill(33)(d.pop())
    if onJs then assertEquals(got, Integer.valueOf(33) :: List.fill[Integer](16)(null) ++ ints(16 to 1 by -1),
      "Scala.js fixed it: see this suite's comment")
    else assertEquals(got, ints(33 to 1 by -1))
  }

  test("no growth past a wrapped buffer, no loss: 32 pushes and pops are right everywhere") {
    val d = java.util.ArrayDeque[Integer]()
    for i <- 1 to 32 do d.push(i)
    assertEquals(List.fill(32)(d.pop()), ints(32 to 1 by -1))
  }
