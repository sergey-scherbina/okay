package okay

import okay.Direct.{*, given}
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
 * explicit tailcall), and a self-call under a lambda left alone.
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

  def isEven(n: Int): Boolean ! okay.Pure = direct:
    if n == 0 then true else !.tailcall(isOdd(n - 1)).reflect
  def isOdd(n: Int): Boolean ! okay.Pure = direct:
    if n == 0 then false else !.tailcall(isEven(n - 1)).reflect

  test("a marked self-call is deferred the same way") {
    assertEquals(!.run(fibM(25)), 75025L)
  }

  test("mutual recursion, one explicit tailcall per hop") {
    assertEquals(!.run(isEven(1_000_001)), false)
    assertEquals(!.run(isOdd(1_000_001)), true)
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
