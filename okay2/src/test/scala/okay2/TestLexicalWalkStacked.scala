package okay2

import Delim.Stacked
import Layered._

/**
 * okay2-lexical-walk-stacked: the twins of the Scala 3 core's
 * TestLexicalWalk, TestLexicalStacked and TestLayeredStacked. Where the
 * Scala 3 suites compare by `Bisim`, these compare values; where they
 * reach multi-shot through `Layered`, these use a raw outer shift.
 */
class TestLexicalWalk extends munit.FunSuite {

  type P = Pure
  type W = Writer[String]
  /** the one member every walk of State[Int] puts in the row */
  type SI = Instances[State[Int]]

  /** the top: every walk's operations answered, or an escaped one named */
  def done[A, G <: Row](p: A ! (SI + G))(implicit d: Distinct[SI with G]): A ! G = Instances.exhausted[State[Int], A, G](p)

  def told(p: (Int, Int) ! W): (Seq[String], (Int, Int)) = !.run(Writer.run[String, (Int, Int), P](p))

  test("walk answers as State.handleAt does on the Writer row, after exhausted") {
    val t = new TestLexical
    val walked: (Int, Int) ! W = done[(Int, Int), W](Lexical.State.walk[Int, Int, W](1) { s =>
      for {
        a <- s.get
        _ <- Writer.tell(s"a=$a").plus[SI]
        _ <- s.set(a + 10)
        b <- s.get
        _ <- Writer.tell(s"b=$b").plus[SI]
      } yield a + b
    })
    assertEquals(told(walked), told(State.handleAt[Int, Int, W](1)(t.counterRow)))
  }

  test("walk on the pure row: exhausted and !.run, no Delim anywhere") {
    assertEquals(!.run(done[(Int, Int), P](Lexical.State.walk[Int, Int, P](5)(s => s.get.flatMap(v => s.set(v * 3))))), (15, 15))
  }

  test("two walk instances of one effect: the outer's get passes the inner walk and reaches its own") {
    val r = !.run(done[(Int, Int), P](Lexical.State.walk[Int, Int, P](0) { outer =>
      Lexical.State.walk[Int, Int, P](10) { inner =>
        outer.get.flatMap(o => inner.get.map(i => o * 100 + i))
      }.map(_._2)
    }))
    assertEquals(r, (0, 10))
  }

  test("an instance used after its walk returned escapes to exhausted, which names it") {
    var leaked: Lexical.State.Inst[Int, SI + P] = null
    val p = Lexical.State.walk[Int, Int, P](0)(s => { leaked = s; s.get })
      .flatMap(_ => leaked.get.map(v => (v, v)))
    val e = intercept[IllegalStateException](!.run(done[(Int, Int), P](p)))
    assert(e.getMessage.contains("survived"), e.getMessage)
  }

  test("multi-shot ACROSS the walk (the prompt outside): each branch resumes the walk at its captured state — deep's answer") {
    val p0 = Delim.prompt[List[(Int, Int)]]
    def pick(s: Lexical.State.Inst[Int, SI + Delim + P]): Int ! (SI + Delim + P) =
      for {
        x <- Delim.shift[List[(Int, Int)], Int, SI + P](p0)(k => k(1).flatMap(a => k(2).flatMap(b => k(3).map(c => a ++ b ++ c))))
        v <- s.get
        _ <- s.set(v + x)
      } yield v
    val r = !.run(done[List[(Int, Int)], P](Delim.run[List[(Int, Int)], SI + P](
      Delim.push[List[(Int, Int)], SI + P](p0)(Lexical.State.walk[Int, Int, Delim + P](0)(pick).map(List(_))))))
    assertEquals(r, List((1, 0), (2, 0), (3, 0)))
  }

  test("multi-shot INSIDE the walk with the machine INSIDE it: the walk threads the state through the branches — deep's answer") {
    val p0 = Delim.prompt[List[Int]]
    def pick(s: Lexical.State.Inst[Int, SI + P]): List[Int] ! (Delim + SI + P) =
      for {
        x <- Delim.shift[List[Int], Int, SI + P](p0)(k => k(1).flatMap(a => k(2).flatMap(b => k(3).map(c => a ++ b ++ c))))
        v <- s.get.plus[Delim]
        _ <- s.set(v + x).plus[Delim]
      } yield List(v)
    val r = !.run(done[(Int, List[Int]), P](Lexical.State.walk[Int, List[Int], P](0)(s =>
      Delim.run[List[Int], SI + P](Delim.push[List[Int], SI + P](p0)(pick(s))))))
    assertEquals(r, (6, List(0, 1, 3)))
  }

  test("multi-shot INSIDE the walk with the machine OUTSIDE it: the operation inside the delimiter escapes, loudly") {
    val p0 = Delim.prompt[List[Int]]
    def pick(s: Lexical.State.Inst[Int, SI + Delim + P]): List[Int] ! (SI + Delim + P) =
      for {
        x <- Delim.shift[List[Int], Int, SI + P](p0)(k => k(1).flatMap(a => k(2).flatMap(b => k(3).map(c => a ++ b ++ c))))
        v <- s.get
        _ <- s.set(v + x)
      } yield List(v)
    val e = intercept[IllegalStateException](!.run(done[(Int, List[Int]), P](Delim.run[(Int, List[Int]), SI + P](
      Lexical.State.walk[Int, List[Int], Delim + P](0)(s => Delim.push[List[Int], SI + P](p0)(pick(s)))))))
    assert(e.getMessage.contains("survived"), e.getMessage)
  }
}

/** the stacked instances: an instance used outside its installation does not compile */
class TestLexicalStacked extends munit.FunSuite {

  type P = Pure

  test("stacked tail and deep instances, one inside the other: each operation reaches its own") {
    val r = !.run(Stacked.delimited[(Int, (Int, Int)), P] { root =>
      Lexical.Stacked.State.tail[(Int, Int), P](root.stack)(0) { a =>
        Lexical.Stacked.State.deep[Int, P](a.in.stack)(10) { b =>
          val st = b.in.stack
          for {
            x <- a.get(st)
            y <- b.get(st)
            _ <- a.set(st)(x + y)
            _ <- b.set(st)(y * 2)
          } yield x + y
        }.map(_._2).flatMap(r => a.get(a.in.stack).map(sa => (r, sa)))
      }
    })
    assertEquals(r, (10, (10, 10)))
  }

  test("a stacked instance used AFTER its installation returned does not compile") {
    val e = compileErrors("""
      okay2.Delim.Stacked.delimited[(Int, Int), okay2.Pure] { root =>
        var leaked: okay2.Lexical.Stacked.State.Tail[Int, Int, okay2.Pure, okay2.Delim.Stacked.Cons[root.p.type, okay2.Delim.Stacked.Empty]] = null
        okay2.Lexical.Stacked.State.tail[Int, okay2.Pure](root.stack)(0) { a =>
          leaked = a
          okay2.pure[okay2.Delim + okay2.Pure, Int](1)
        }.flatMap(_ => leaked.get(root.stack).map(v => (v, v)))
      }""")
    assert(e.contains("is not on the prompt stack"), s"compiled, or not our message: $e")
  }
}

/** the stacked layers */
class TestLayeredStacked extends munit.FunSuite {

  type P = Pure

  test("stacked: List outside Option, each reflect reaching its own layer — the stage-0 answer") {
    val r = !.run(Stacked.delimited[List[Option[Int]], P] { root =>
      Layered.Stacked.reify[List, Option[Int], P](root.stack) { lst =>
        Layered.Stacked.reify[Option, Int, P](lst.stack) { opt =>
          val st = opt.stack
          for {
            x <- List(1, 2, 3).reflectAt[P](st, lst)
            y <- (if (x == 2) None else Some(x * 10)).reflectAt[P](st, opt)
          } yield x + y
        }
      }
    })
    assertEquals(r, List(Some(11), None, Some(33)))
  }

  test("stacked: Option outside List — None empties everything") {
    val r = !.run(Stacked.delimited[Option[List[Int]], P] { root =>
      Layered.Stacked.reify[Option, List[Int], P](root.stack) { opt =>
        Layered.Stacked.reify[List, Int, P](opt.stack) { lst =>
          val st = lst.stack
          for {
            x <- List(1, 2, 3).reflectAt[P](st, lst)
            y <- (if (x == 2) None else Some(x * 10)).reflectAt[P](st, opt)
          } yield x + y
        }
      }
    })
    assertEquals(r, None)
  }

  test("stacked: a layer used AFTER its reify returned does not compile") {
    val e = compileErrors("""
      okay2.Delim.Stacked.delimited[Option[Int], okay2.Pure] { root =>
        var leaked: okay2.Delim.Stacked.In[Option[Int], okay2.Delim.Stacked.Cons[root.p.type, okay2.Delim.Stacked.Empty]] = null
        okay2.Layered.Stacked.reify[Option, Int, okay2.Pure](root.stack) { opt =>
          leaked = opt
          okay2.pure[okay2.Delim + okay2.Pure, Int](1)
        }.flatMap(_ => okay2.Layered.ReflectOps(Option(2)).reflectAt[okay2.Pure](root.stack, leaked).map(Option(_)))
      }""")
    assert(e.contains("is not on the prompt stack"), s"compiled, or not our message: $e")
  }
}
