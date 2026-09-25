package okay

import okay.Direct.*   // no `Direct.given`: the colouring of a program is Free's own (directColor)
import scala.language.implicitConversions

/**
 * Deep recursion inside `direct` (deep-recursive-direct,
 * specs/direct-macro.md): a self-call at the block's program type is
 * deferred into the tree wherever it is marked or auto-coloured, so
 * the recursion trampolines through `Free.resume` instead of the JVM
 * stack. The shapes are the ones "Deep recursion in Scala 3" (Kozak)
 * drives its `deepRecursive` macro with — fib, a non-tail countdown —
 * and what that macro refuses: a self-call in `match`, a real row
 * interleaving effects with the recursion, mutual recursion (one
 * tail-position call to another def), and a self-call under a lambda
 * left alone.
 */
class TestDirectDeep extends munit.FunSuite {

  // ---- zero annotation: `import Direct.given` colours the self-call, the macro defers it
  def fib(n: Int): Long ! okay.Pure = direct:
    if n < 2 then n.toLong else fib(n - 1) + fib(n - 2)

  /** `1 + sum(n - 1)`: the shape @tailrec cannot take */
  def sum(n: Int): Long ! okay.Pure = direct:
    if n == 0 then 0L else 1L + sum(n - 1)

  def count(xs: List[Int], acc: Long): Long ! okay.Pure = direct:
    xs match
      case Nil => acc
      case h :: t => count(t, acc + h)

  type W = Writer % String
  def told(n: Int): Long ! W = direct:
    if n == 0 then 0L
    else
      Writer(s"at $n"): Unit
      1L + told(n - 1)

  test("fib: two bare self-calls in one expression") {
    assertEquals(!.run(fib(25)), 75025L)
  }

  test("non-tail recursion a million deep does not touch the JVM stack") {
    assertEquals(!.run(sum(1_000_000)), 1_000_000L)
  }

  test("a self-call inside match, two parameters") {
    assertEquals(!.run(count(List.fill(300_000)(1), 0L)), 300_000L)
  }

  test("a real row: tells interleave with the recursion, in order") {
    val (ws, a) = !.run(Writer.run[String, Long, okay.Pure](told(3)))
    assertEquals(a, 3L)
    assertEquals(ws, Seq("at 3", "at 2", "at 1"))
  }

  // ---- the marked spelling, and what the marks still do
  def fibM(n: Int): Long ! okay.Pure = direct:
    if n < 2 then n.toLong else fibM(n - 1).reflect + fibM(n - 2).reflect

  // mutual recursion needs the tailcall — the deferral rule inside a
  // block covers a call to the ENCLOSING def, and these call each
  // other — but it needs NO mark: `!.tailcall(p)` is a program value
  // like any other, and `Free.directColor` colours it (mutual-mark-free,
  // 2026-09-16; a report that it did not was a probe compiled against
  // classes older than `directColor`).
  def isEven(n: Int): Boolean ! okay.Pure = direct:
    if n == 0 then true else !.tailcall(isOdd(n - 1))
  def isOdd(n: Int): Boolean ! okay.Pure = direct:
    if n == 0 then false else !.tailcall(isEven(n - 1))

  /** the same pair with the mark written out: both spellings must work */
  def isEvenM(n: Int): Boolean ! okay.Pure = direct:
    if n == 0 then true else !.tailcall(isOddM(n - 1)).reflect
  def isOddM(n: Int): Boolean ! okay.Pure = direct:
    if n == 0 then false else !.tailcall(isEvenM(n - 1)).reflect

  test("a marked self-call is deferred the same way") {
    assertEquals(!.run(fibM(25)), 75025L)
  }

  test("mutual recursion, one tailcall per hop and no mark") {
    assertEquals(!.run(isEven(1_000_001)), false)
    assertEquals(!.run(isOdd(1_000_001)), true)
  }

  // ---- mutual recursion with NO word at all (direct-tail-defer)
  def evenNW(n: Int): Boolean ! okay.Pure = direct:
    if n == 0 then true else oddNW(n - 1)
  def oddNW(n: Int): Boolean ! okay.Pure = direct:
    if n == 0 then false else evenNW(n - 1)

  test("a tail-position call to ANOTHER def is deferred too") {
    assertEquals(!.run(evenNW(1_000_001)), false)
    assertEquals(!.run(oddNW(1_000_001)), true)
  }

  test("the same pair written with the mark") {
    assertEquals(!.run(isEvenM(1_000_001)), false)
    assertEquals(!.run(isOddM(1_000_001)), true)
  }

  // ---- a NON-tail mutual call: deferred by default (direct-defer-default)
  def nonTailEven(n: Int): Long ! okay.Pure = direct:
    if n == 0 then 0L else 1L + nonTailOdd(n - 1)
  def nonTailOdd(n: Int): Long ! okay.Pure = direct:
    if n == 0 then 0L else 1L + nonTailEven(n - 1)

  test("a NON-tail mutual call is deferred too, by default") {
    assertEquals(!.run(nonTailEven(1_000_000)), 1_000_000L)
  }

  /** the opt-out: `import Direct.eagerCalls.given` gives up deferring a
   * call where it stands, and keeps the two shapes measured free */
  object Opted:
    import okay.Direct.eagerCalls.given    // this scope builds calls where they stand
    def fib(n: Int): Long ! okay.Pure = direct:
      if n < 2 then n.toLong else fib(n - 1) + fib(n - 2)      // self, non-tail: still deferred
    def even(n: Int): Boolean ! okay.Pure = direct:
      if n == 0 then true else odd(n - 1)                      // other def, tail: still deferred
    def odd(n: Int): Boolean ! okay.Pure = direct:
      if n == 0 then false else even(n - 1)

  test("the opt-out keeps the free rules: the enclosing def anywhere, another def in tail position") {
    assertEquals(!.run(Opted.fib(25)), 75025L)
    assertEquals(!.run(Opted.even(1_000_001)), false)
    assertEquals(!.run(Opted.odd(1_000_001)), true)
  }

  // ---- docs/direct-style.md, "Recursion in a block", verbatim
  object DocRecursion:
    def fib(n: Int): Long ! Pure = direct:
      if n < 2 then n.toLong else fib(n - 1) + fib(n - 2)     // coloured, deferred

    def sum(n: Int): Long ! Pure = direct:                      // 1 + sum(n - 1): not tail
      if n == 0 then 0L else 1L + sum(n - 1)

    def count(xs: List[Int], acc: Long): Long ! Pure = direct:
      xs match
        case Nil => acc
        case h :: t => count(t, acc + h)

    def isEven(n: Int): Boolean ! Pure = direct:
      if n == 0 then true else isOdd(n - 1)      // deferred: tail position
    def isOdd(n: Int): Boolean ! Pure = direct:
      if n == 0 then false else isEven(n - 1)

    def sumEven(n: Int): Long ! Pure = direct:
      if n == 0 then 0L else 1L + sumOdd(n - 1)     // NOT tail — deferred anyway
    def sumOdd(n: Int): Long ! Pure = direct:
      if n == 0 then 0L else 1L + sumEven(n - 1)

    // pinned verbatim, bare (REPL-echo style) — nowarn rather than
    // reshaping the pinned text (2026-09-25)
    @scala.annotation.nowarn("msg=unused value|discarded non-Unit value")
    def demoSum(): Unit =
      !.run(sum(1_000_000))   // 1000000, on the default stack

    @scala.annotation.nowarn("msg=unused value|discarded non-Unit value")
    def demoIsEven(): Unit =
      !.run(isEven(1_000_001))   // false, on the default stack

  test("docs/direct-style.md: fib, sum, count — the recursion section verbatim") {
    DocRecursion.demoSum()
    assertEquals(!.run(DocRecursion.fib(25)), 75025L)
    assertEquals(!.run(DocRecursion.sum(1_000_000)), 1_000_000L)
    assertEquals(!.run(DocRecursion.count(List.fill(300_000)(1), 0L)), 300_000L)
  }

  test("docs/direct-style.md: mutual recursion needs nothing either") {
    DocRecursion.demoIsEven()
    assertEquals(!.run(DocRecursion.isEven(1_000_001)), false)
  }

  test("docs/direct-style.md: a NOT-tail mutual call is deferred anyway") {
    assertEquals(!.run(DocRecursion.sumEven(1_000_000)), 1_000_000L)
    assertEquals(!.run(DocRecursion.sumOdd(1_000_000)), 1_000_000L)
  }

  test("a self-call under a lambda is a value, untouched") {
    def twice(n: Int): List[Long] ! okay.Pure = direct:
      if n == 0 then Nil
      else
        val below: List[Long] = twice(n - 1)
        val ps: List[Long ! okay.Pure] = List(1, 2).map(i => fib(i))   // a lambda: values
        ps.map(p => !.run(p)) ++ below
    assertEquals(!.run(twice(2)), List(1L, 1L, 1L, 1L))
  }
}
