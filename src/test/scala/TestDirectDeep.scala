package okay

import okay.Direct.*

/**
 * Deep recursion (deep-recursive-direct, specs/direct-macro.md), two
 * doors. `deepRecursive` is the article's: a def with a PLAIN result
 * type, self-calls written as plain calls, the macro generates the
 * program and runs it. Inside a `direct` block at the PROGRAM type a
 * marked or auto-coloured self-call is deferred into the tree. Both
 * trampoline through `Free.resume`; the shapes are the article's —
 * fib, a non-tail countdown — plus what its macro refuses: mutual
 * recursion, written with one explicit tailcall.
 */
class TestDirectDeep extends munit.FunSuite {

  // ---- deepRecursive: the article's API, verbatim shapes
  def fib(n: Int): Long = deepRecursive:
    if n < 2 then n.toLong else fib(n - 1) + fib(n - 2)

  /** `1 + sum(n - 1)`: the shape @tailrec cannot take */
  def sum(n: Int): Long = deepRecursive:
    if n == 0 then 0L else 1L + sum(n - 1)

  def count(xs: List[Int], acc: Long): Long = deepRecursive:
    xs match
      case Nil => acc
      case h :: t => count(t, acc + h)

  test("deepRecursive: fib, two self-calls in one expression") {
    assertEquals(fib(25), 75025L)
  }

  test("deepRecursive: non-tail recursion a million deep does not touch the JVM stack") {
    assertEquals(sum(1_000_000), 1_000_000L)
  }

  test("deepRecursive: a self-call inside match, two parameters") {
    assertEquals(count(List.fill(300_000)(1), 0L), 300_000L)
  }

  // ---- inside direct, at the program type
  def fibP(n: Int): Long ! okay.Pure = direct:
    if n < 2 then n.toLong else fibP(n - 1).reflect + fibP(n - 2).reflect

  /** `1 + sum(n - 1)` at the program type, the self-call marked */
  def sumP(n: Int): Long ! okay.Pure = direct:
    if n == 0 then 0L else 1L + sumP(n - 1).reflect

  def isEven(n: Int): Boolean ! okay.Pure = direct:
    if n == 0 then true else !.tailcall(isOdd(n - 1)).reflect
  def isOdd(n: Int): Boolean ! okay.Pure = direct:
    if n == 0 then false else !.tailcall(isEven(n - 1)).reflect

  test("direct: a marked self-call is deferred, not evaluated at construction") {
    assertEquals(!.run(fibP(25)), 75025L)
    assertEquals(!.run(sumP(1_000_000)), 1_000_000L)
  }

  test("direct: mutual recursion, one explicit tailcall per hop") {
    assertEquals(!.run(isEven(1_000_001)), false)
    assertEquals(!.run(isOdd(1_000_001)), true)
  }

  test("direct: a self-call under a lambda is a value, untouched") {
    def twice(n: Int): List[Long] ! okay.Pure = direct:
      if n == 0 then Nil
      else
        val below = twice(n - 1).reflect
        val ps: List[Long ! okay.Pure] = List(1, 2).map(i => fibP(i))   // a lambda: values
        ps.map(p => !.run(p)) ++ below
    assertEquals(!.run(twice(2)), List(1L, 1L, 1L, 1L))
  }
}
