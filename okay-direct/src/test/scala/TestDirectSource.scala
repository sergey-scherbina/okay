package okay

import okay.Direct.*

/**
 * specs/direct-loops.md v3 — `for x <- src do body` over a SOURCE: a
 * `Pull[A, G]`, whose step is a program. The loop is emitted as a
 * program that binds one step per element, marks in the body or not;
 * outside a block `Pull.foreach` is that program written by hand.
 */
class TestDirectSource extends munit.FunSuite:

  type W = Writer % String
  def run[A](p: A ! W): (Seq[String], A) = !.run(Writer.run[String, A, okay.Pure](p))
  def say(s: String): Unit ! W = Writer.tell(s)

  test("Pull.loop is a program: nothing runs until it is run, then every element in order") {
    val seen = scala.collection.mutable.Buffer[Int]()
    val prog: Unit ! Pure = Pull.of(LazyList(1, 2, 3)).loop(seen += _)
    assertEquals(seen.toList, Nil)
    !.run(prog)
    assertEquals(seen.toList, List(1, 2, 3))
  }

  test("for x <- Pull.of(xs) do body.? — the body's effects per element, in order") {
    val (log, _) = run(direct { for x <- Pull.of(List(1, 2, 3)) do say(s"x$x").? })
    assertEquals(log, Seq("x1", "x2", "x3"))
  }

  test("no marks in the body: the loop still runs as a program — the road fires on the receiver's type") {
    val seen = scala.collection.mutable.Buffer[Int]()
    val (log, _) = run(direct {
      say("before").?
      for x <- Pull.of(List(1, 2)) do seen += x
      say("after").?
    })
    assertEquals(seen.toList, List(1, 2))
    assertEquals(log, Seq("before", "after"))
  }

  test("a guard skips without consuming a bind") {
    val (log, _) = run(direct { for x <- Pull.of(List(1, 2, 3, 4)) if x % 2 == 0 do say(s"even $x").? })
    assertEquals(log, Seq("even 2", "even 4"))
  }

  test("docs/direct-style.md: a loop over a source (direct-loops v3), verbatim") {
    val producer: Unit ! Writer % Int + State % Int = direct[[A] =>> A ! Writer % Int + State % Int] {
      Writer.tell(1).?; val _ = State.modify[Int](_ + 1).?
      Writer.tell(2).?; val _ = State.modify[Int](_ + 1).?
      Writer.tell(3).?; val _ = State.modify[Int](_ + 1).?
    }
    @scala.annotation.nowarn("msg=unused value|discarded non-Unit value")
    def docSourceDemo(): Unit =
      direct[[A] =>> A ! State % Int + Writer % String] {
        for x <- Pull.toldIn(producer) do            // the producer performs State between tells
          say(s"got $x after ${State.get[Int].?} steps").?
      }
    docSourceDemo()
    val prog = direct[[A] =>> A ! State % Int + Writer % String] {
      for x <- Pull.toldIn(producer) do say(s"got $x after ${State.get[Int].?} steps").?
    }
    val (log, (cell, _)) = run(State.handle(0)(prog))
    assertEquals(log, Seq("got 1 after 0 steps", "got 2 after 1 steps", "got 3 after 2 steps"))
    assertEquals(cell, 3)
  }

  test("the source's own effects interleave with the body's, one step per element (Pull.toldIn)") {
    // a producer that counts its tells in a State cell between them
    val producer: Unit ! Writer % Int + State % Int = direct[[A] =>> A ! Writer % Int + State % Int] {
      Writer.tell(1).?; val _ = State.modify[Int](_ + 1).?
      Writer.tell(2).?; val _ = State.modify[Int](_ + 1).?
      Writer.tell(3).?; val _ = State.modify[Int](_ + 1).?
    }
    val prog: Unit ! State % Int + W = direct[[A] =>> A ! State % Int + W] {
      for x <- Pull.toldIn(producer) do say(s"got $x after ${State.get[Int].?} steps").?
    }
    // the reader's step runs the producer up to its next tell, so the
    // effect AFTER a tell has not happened when that element is read
    val (log, (cell, _)) = run(State.handle(0)(prog))
    assertEquals(log, Seq("got 1 after 0 steps", "got 2 after 1 steps", "got 3 after 2 steps"))
    assertEquals(cell, 3)
  }

  test("Pull.told over a pure writer program reads its told values") {
    val prod: Int ! Writer % Int = Writer.tell(10).flatMap(_ => Writer.tell(20)).map(_ => 99)
    val (log, _) = run(direct { for x <- Pull.told(prod) do say(s"v$x").? })
    assertEquals(log, Seq("v10", "v20"))
  }

  test("a Pull whose G is not in the block's row is refused at the loop; yield over a Pull does not parse to a loop") {
    val refused = compileErrors("""
      val p: Unit ! Writer % Int + Async = Writer.tell(1)
      run(direct { for x <- Pull.toldIn(p) do say(s"$x").? })
    """)
    assert(refused.nonEmpty, "an Async source in a Writer-only block must be refused")
    val noMap = compileErrors("""run(direct { for x <- Pull.of(List(1)) yield say("y").? })""")
    assert(noMap.nonEmpty, "a Pull has no map")
    val outside = compileErrors("""for x <- Pull.of(List(1)) do println(x)""")
    assert(outside.nonEmpty, "outside a block the for over a source does not typecheck — loop(f) is the program")
  }
