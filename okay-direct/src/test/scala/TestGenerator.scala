package okay

import okay.Direct.*

/**
 * specs/generators.md, the direct side: a generator block, `yield`
 * inside `for` as emit, `stop` from inside a loop, and a generator
 * consumed by a `for … do` in an ordinary block — each read lazily.
 */
class TestGenerator extends munit.FunSuite:

  test("a generator block: for … yield emits each value; the block's value is dropped") {
    val g: Gen[Int] = generator[Int] {
      for x <- List(1, 2, 3) yield x * 10
    }
    assertEquals(g.toList, List(10, 20, 30))
  }

  test("statements between yields run when the NEXT value is asked for — the Python law, through the block") {
    var steps = 0
    val g: Gen[Int] = generator[Int] {
      var i = 0
      while i < 5 do
        i += 1
        steps += 1
        Gen.emit(i).!?
    }
    val it = g.iterator
    assertEquals(steps, 0)
    assertEquals(it.next(), 1); assertEquals(steps, 1)
    assertEquals(it.next(), 2); assertEquals(steps, 2)
    assertEquals(g.take(3).toList, List(1, 2, 3))
  }

  test("stop inside a while ends the generation; whatever follows never runs") {
    var after = 0
    val g: Gen[Int] = generator[Int] {
      var i = 0
      while true do
        i += 1
        if i > 3 then Gen.stop[Int].!?
        Gen.emit(i).!?
        after = i
    }
    assertEquals(g.toList, List(1, 2, 3))
    assertEquals(after, 3)
    assertEquals(g.map(_ * 2).take(10).toList, List(2, 4, 6))
  }

  test("two generators and a guard inside a generator block; yield emits the pairs as the block's value") {
    val g: Gen[(Int, Int)] = generator[(Int, Int)] {
      Gen.emit((0, 0)).!?
      for
        x <- List(1, 2)
        y <- List(10, 20) if x + y > 11
      yield (x, y)
    }
    assertEquals(g.toList, List((0, 0), (1, 20), (2, 10), (2, 20)))
  }

  test("mid-block, the emitting loop is spelled `for … do Gen.emit(e)` — a bare statement runs") {
    // a for-yield mid-block is a value Scala's own checker sees discarded
    // (E176) before the macro runs, so the mid-block spelling is `do`
    val g: Gen[Int] = generator[Int] {
      Gen.emit(0).!?
      for x <- List(1, 2) do Gen.emit(x * 10)
      Gen.emit(9).!?
    }
    assertEquals(g.toList, List(0, 10, 20, 9))
  }

  test("a yielded value may itself be an effect of the row: the mark inside the yield") {
    def twice(i: Int): Int ! Writer % Int + Stop = Gen.emit(i).program.flatMap(_ => pure(i * 2))
    val g: Gen[Int] = generator[Int] {
      for x <- List(1, 2) yield twice(x).!?     // emits x, then yields 2x
    }
    assertEquals(g.toList, List(1, 2, 2, 4))
  }

  test("a generator consumed by `for x <- gen do` in an ordinary block runs only as far as read") {
    var steps = 0
    val src: Gen[Int] = generator[Int] {
      var i = 0
      while true do
        i += 1
        steps += 1
        Gen.emit(i).!?
    }
    type W = Writer % String
    val prog: Unit ! W = direct {
      for x <- src.take(3) do Writer.tell(s"got $x").!?
    }
    val (log, _) = !.run(Writer.run[String, Unit, okay.Pure](prog))
    assertEquals(log, Seq("got 1", "got 2", "got 3"))
    assertEquals(steps, 3, "the source ran to its third yield and stopped")
  }

  test("a recursive generator: the block calls its own def") {
    def countdown(n: Int): Gen[Int] = generator[Int] {
      if n > 0 then
        Gen.emit(n).!?
        countdown(n - 1).!?
    }
    assertEquals(countdown(3).toList, List(3, 2, 1))
    assertEquals(countdown(100000).take(2).toList, List(100000, 99999))
  }

  test("docs/direct-style.md, Generators: the three ways, verbatim") {
    val evens: Gen[Int] =
      for x <- Gen.unfold(1)(i => Some((i, i + 1))) if x % 2 == 0 yield x * x
    assertEquals(evens.take(3).toList, List(4, 16, 36))

    val fib: Gen[Long] = generator[Long] {
      var (a, b) = (0L, 1L)
      while true do
        Gen.emit(a).!?
        val t = a; a = b; b = t + b
    }
    assertEquals(fib.drop(10).first, Some(55L))

    assertEquals(Gen.of(Writer.tell(1).flatMap(_ => Writer.tell(2))).toList, List(1, 2))
    assertEquals((Gen(1) ++ Gen(2, 3)).toList, List(1, 2, 3))
    assertEquals(Gen(1, 2, 3).iterator.toList, List(1, 2, 3))
    assertEquals(generator[Int] {
      var i = 0
      while true do { i += 1; if i > 3 then Gen.stop[Int].!?; Gen.emit(i).!? }
    }.toList, List(1, 2, 3))
  }
