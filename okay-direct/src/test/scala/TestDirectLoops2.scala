package okay

import okay.Direct.*

/**
 * specs/direct-loops.md v2: guards, several generators, the other
 * combinators, the other collections. Every test is the direct block
 * against the same program written with the monad's own combinators
 * or a hand-rolled walk, over Option (short-circuit) or a Writer row
 * (order and laziness observable in the log).
 */
class TestDirectLoops2 extends munit.FunSuite:

  type W = Writer % String
  def run[A](p: A ! W): (Seq[String], A) = !.run(Writer.run[String, A, okay.Pure](p))
  def say(s: String): Unit ! W = Writer.tell(s)
  def look(i: Int): Int ! W = Writer.tell(s"look $i").flatMap(_ => pure(i * 10))

  test("a guard: for x <- xs if p(x) do — the guard runs per element, the body only when it holds") {
    val (log, _) = run(direct {
      for x <- List(1, 2, 3, 4) if x % 2 == 0 do say(s"even $x").!?
    })
    assertEquals(log, Seq("even 2", "even 4"))
  }

  test("a guard with a MARK in the condition binds before the body") {
    def isEven(x: Int): Boolean ! W = Writer.tell(s"test $x").flatMap(_ => pure(x % 2 == 0))
    val (log, _) = run(direct {
      for x <- List(1, 2, 3) if isEven(x).!? do say(s"body $x").!?
    })
    assertEquals(log, Seq("test 1", "test 2", "body 2", "test 3"))
  }

  test("two guards, in source order, both before the body") {
    val (log, r) = run(direct {
      for x <- List(1, 2, 3, 4, 5, 6) if x % 2 == 0 if x > 2 yield look(x).!?
    })
    assertEquals(r, List(40, 60))
    assertEquals(log, Seq("look 4", "look 6"))
  }

  test("two generators: for x <- xs; y <- ys yield — the flatMap shape, results in order") {
    val (log, r) = run(direct {
      for
        x <- List(1, 2)
        y <- List(10, 20)
      yield look(x + y).!?
    })
    assertEquals(r, List(110, 210, 120, 220))
    assertEquals(log, Seq("look 11", "look 21", "look 12", "look 22"))
  }

  test("two generators with a guard between them") {
    val r = direct[Option] {
      for
        x <- List(1, 2, 3)
        y <- List(x, x * 2) if y > 2
      yield Some(y).!?
    }
    assertEquals(r, Some(List(4, 3, 6)))
  }

  test("a None in the inner generator short-circuits the whole comprehension") {
    var hits = 0
    def eff(i: Int): Option[Int] = { hits += 1; if i == 20 then None else Some(i) }
    val r = direct[Option] {
      for
        x <- List(1, 2, 3)
        y <- List(10, 20, 30)
      yield eff(x * y).!?
    }
    assertEquals(r, None)
    assertEquals(hits, 2, "1*10 then 1*20 = None: nothing after it ran")
  }

  test("yield into a Vector, a Set and a Map") {
    val v: Option[Vector[Int]] = direct[Option] { for i <- Vector(1, 2, 3) yield Some(i * 2).!? }
    assertEquals(v, Some(Vector(2, 4, 6)))
    val s: Option[Set[Int]] = direct[Option] { for i <- Set(1, 2, 2, 3) yield Some(i % 2).!? }
    assertEquals(s, Some(Set(0, 1)))
    val m: Option[Map[String, Int]] = direct[Option] {
      for (k, n) <- Map("a" -> 1, "b" -> 2) yield (k * 2, Some(n * 10).!?)
    }
    assertEquals(m, Some(Map("aa" -> 10, "bb" -> 20)))
  }

  test("exists stops at the first true; forall at the first false") {
    val (log1, e) = run(direct { List(1, 2, 3, 4).exists(x => look(x).!? > 15) })
    assertEquals(e, true)
    assertEquals(log1, Seq("look 1", "look 2"))
    val (log2, a) = run(direct { List(1, 2, 3).forall(x => look(x).!? < 15) })
    assertEquals(a, false)
    assertEquals(log2, Seq("look 1", "look 2"))
    val (log3, none) = run(direct { List(1, 2).exists(x => look(x).!? > 100) })
    assertEquals(none, false)
    assertEquals(log3, Seq("look 1", "look 2"))
  }

  test("find answers the first match and stops; filter keeps the matches") {
    val (log, f) = run(direct { List(1, 2, 3, 4).find(x => look(x).!? >= 20) })
    assertEquals(f, Some(2))
    assertEquals(log, Seq("look 1", "look 2"))
    val (_, kept) = run(direct { List(1, 2, 3, 4).filter(x => look(x).!? % 20 == 0) })
    assertEquals(kept, List(2, 4))
  }

  test("foldLeft threads the accumulator; a marked step per element") {
    val (log, total) = run(direct { List(1, 2, 3).foldLeft(0)((acc, x) => acc + look(x).!?) })
    assertEquals(total, 60)
    assertEquals(log, Seq("look 1", "look 2", "look 3"))
    // a marked zero binds first
    val (log2, t2) = run(direct { List(1, 2).foldLeft(look(100).!?)((acc, x) => acc + look(x).!?) })
    assertEquals(t2, 1030)
    assertEquals(log2, Seq("look 100", "look 1", "look 2"))
  }

  test("a filtered foldLeft receiver, and a marked receiver") {
    val (_, t) = run(direct { List(1, 2, 3, 4).filter(_ % 2 == 0).foldLeft(0)((acc, x) => acc + look(x).!?) })
    assertEquals(t, 60)
    def xs: List[Int] ! W = Writer.tell("xs").flatMap(_ => pure(List(1, 2)))
    val (log, r) = run(direct { for x <- xs.!? if x > 1 yield look(x).!? })
    assertEquals(r, List(20))
    assertEquals(log, Seq("xs", "look 2"))
  }

  test("a for-yield into an unsupported collection is refused, naming the workaround") {
    val e = compileErrors("""
      import okay.Direct.*
      direct[Option] { for i <- LazyList(1, 2) yield Some(i).!? }
    """)
    assert(e.contains("collection type"), e)
  }

  test("docs/direct-style.md: Loops and comprehensions in full, verbatim") {
    val xs = List(1, 2, 3)
    def isEven(x: Int): Boolean ! W = Writer.tell(s"test $x").flatMap(_ => pure(x % 2 == 0))

    // a guard — `xs.withFilter(x => p)` — runs per element, in source
    // order; a MARKED guard binds before the body runs
    val (guardLog, _) = run(direct {
      for x <- xs if isEven(x).!? do say(s"body $x").!?
    })
    assertEquals(guardLog, Seq("test 1", "test 2", "body 2", "test 3"))

    // two generators — `xs.flatMap(x => ys.map(y => …))` — results in
    // the comprehension's order; a guard between them is honoured
    val r: List[Int] ! W = direct {
      for
        x <- List(1, 2)
        y <- List(10, 20) if y > 10
      yield look(x + y).!?
    }                                    // List(210, 220); log: look 21, look 22
    val (rLog, rOut) = run(r)
    assertEquals(rOut, List(210, 220))
    assertEquals(rLog, Seq("look 21", "look 22"))

    // the yield answers the node's own collection: Vector, Set, Map of pairs
    val m: Option[Map[String, Int]] = direct[Option] {
      for (k, n) <- Map("a" -> 1, "b" -> 2) yield (k * 2, Some(n * 10).!?)
    }                                    // Some(Map("aa" -> 10, "bb" -> 20))
    assertEquals(m, Some(Map("aa" -> 10, "bb" -> 20)))

    // the HOFs: exists/forall/find STOP at the element that decides
    val (log, e) = run(direct { List(1, 2, 3, 4).exists(x => look(x).!? > 15) })
    // e == true, log == Seq("look 1", "look 2") — 3 and 4 never looked at
    assertEquals(e, true)
    assertEquals(log, Seq("look 1", "look 2"))

    // filter keeps the matches; foldLeft threads the accumulator
    docFoldDemo()
    val (foldLog, foldOut) = run(direct { List(1, 2, 3).foldLeft(0)((acc, x) => acc + look(x).!?) })
    assertEquals(foldOut, 60)
    assertEquals(foldLog, Seq("look 1", "look 2", "look 3"))
  }

  @scala.annotation.nowarn("msg=unused value|discarded non-Unit value")
  private def docFoldDemo(): Unit =
    val _: Int ! W =
      direct { List(1, 2, 3).foldLeft(0)((acc, x) => acc + look(x).!?) }   // 60
