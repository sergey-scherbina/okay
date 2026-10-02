package okay2

import Shift.push

/**
 * Delimited control as an effect: the classic shift/reset laws, and
 * the two things a single-prompt design cannot do — escaping PAST an
 * intervening delimiter, and two delimiters with different answer
 * types in one row.
 */
class TestDelim extends munit.FunSuite {

  type P = Pure
  type Row = Shift[Any] + P

  test("shift/reset: the continuation comes back as a value") {
    val r = !.run(Shift.reset[Int, P] { p =>
      Shift.shift[Int, Int, P](p)(k => k(5).map(_ * 2))
    })
    assertEquals(r, 10)
  }

  test("the captured continuation includes what follows the shift") {
    val r = !.run(Shift.reset[Int, P] { p =>
      Shift.shift[Int, Int, P](p)(k => k(1)).map(_ + 10)
    })
    assertEquals(r, 11)
  }

  test("dropping the continuation is an early exit") {
    var reached = false
    val r = !.run(Shift.reset[Int, P] { p =>
      Shift.shift[Int, Int, P](p)(_ => pure[Row, Int](42))
        .map { x => reached = true; x + 1 }
    })
    assertEquals(r, 42)
    assert(!reached, "the abandoned continuation ran anyway")
  }

  test("abort: the same thing, named") {
    val r = !.run(Shift.reset[Int, P] { p =>
      Shift.abort[Int, Int, P](p)(7).map(_ + 100)
    })
    assertEquals(r, 7)
  }

  test("multi-shot: the continuation is a value, so invoke it twice") {
    val r = !.run(Shift.reset[Int, P] { p =>
      Shift.shift[Int, Int, P](p) { k =>
        k(1).flatMap(a => k(2).map(b => a + b))
      }.map(_ * 10)
    })
    assertEquals(r, 30)
  }

  test("MULTI-PROMPT: a shift escapes past an intervening delimiter") {
    val outer = Shift.prompt[Int]
    val inner = Shift.prompt[Int]
    var innerFinished = false

    val prog: Int ! Row =
      push[Int, P](outer) {
        push[Int, P](inner) {
          Shift.shift[Int, Int, P](outer)(_ => pure[Row, Int](99))
        }.map { x => innerFinished = true; x + 1 }
      }.map(_ + 1000)

    assertEquals(!.run(Shift.run[Int, P](prog)), 1099)
    assert(!innerFinished, "the intervening delimiter's tail ran")
  }

  test("two prompts of DIFFERENT answer types live in one row") {
    val num = Shift.prompt[Int]
    val str = Shift.prompt[String]

    val prog: String ! Row =
      push[String, P](str) {
        push[Int, P](num) {
          Shift.shift[Int, Int, P](num)(k => k(21).map(_ * 2))
        }.flatMap(n => Shift.shift[String, String, P](str)(_ => pure[Row, String](s"n=$n")))
      }

    assertEquals(!.run(Shift.run[String, P](prog)), "n=42")
  }

  test("the captured continuation re-installs its own prompt") {
    val r = !.run(Shift.reset[Int, P] { p =>
      Shift.shift[Int, Int, P](p) { k =>
        k(1).flatMap(a =>
          if (a < 5) k(a + 1).map(_ + 100) else pure[Row, Int](a))
      }.map(_ * 2)
    })
    assertEquals(r, 106)
  }

  test("other effects pass through the machine untouched") {
    type F = Writer[String]
    val told = Shift.run[Int, F] {
      push[Int, F](Shift.prompt[Int]) {
        Writer.tell("before").at[Shift[Any] + F].flatMap(_ => pure[Shift[Any] + F, Int](1))
      }.flatMap(x => Writer.tell("after").at[Shift[Any] + F].map(_ => x + 1))
    }
    val (ws, a) = !.run(Writer.run(told))
    assertEquals(a, 2)
    assertEquals(ws, Seq("before", "after"))
  }

  test("effects inside an abandoned continuation do NOT run") {
    type F = Writer[String]
    val p = Shift.prompt[Int]
    val prog = Shift.run[Int, F] {
      push[Int, F](p) {
        Shift.shift[Int, Int, F](p)(_ => pure[Shift[Any] + F, Int](5)).flatMap(x =>
          Writer.tell("never").at[Shift[Any] + F].map(_ => x))
      }
    }
    val (ws, a) = !.run(Writer.run(prog))
    assertEquals(a, 5)
    assertEquals(ws, Seq.empty, "the dropped continuation told anyway")
  }

  test("shift vs shift0: does the body keep the delimiter?") {
    val nested = !.run(Shift.reset[Int, P] { p =>
      Shift.shift[Int, Int, P](p)(_ =>
        Shift.shift[Int, Int, P](p)(_ => pure[Row, Int](1)))
    })
    assertEquals(nested, 1)

    val _ = intercept[NoPrompt] {
      !.run(Shift.reset[Int, P] { p =>
        Shift.shift0[Int, Int, P](p)(_ =>
          Shift.shift0[Int, Int, P](p)(_ => pure[Row, Int](1)))
      })
    }
  }

  test("shift0 vs control0: does the continuation re-install it?") {
    val ok = !.run(Shift.reset[Int, P] { p =>
      Shift.shift0[Int, Int, P](p)(k => k(1))
        .flatMap(x => Shift.shift0[Int, Int, P](p)(_ => pure[Row, Int](x + 40)))
    })
    assertEquals(ok, 41)

    val _ = intercept[NoPrompt] {
      !.run(Shift.reset[Int, P] { p =>
        Shift.control0[Int, Int, P](p)(k => k(1))
          .flatMap(x => Shift.control0[Int, Int, P](p)(_ => pure[Row, Int](x)))
      })
    }
  }

  test("control: the body keeps the delimiter, the continuation does not") {
    val r = !.run(Shift.reset[Int, P] { p =>
      Shift.control[Int, Int, P](p)(k => k(1)).map(_ + 10)
    })
    assertEquals(r, 11)
  }

  test("a new effect defined in USER code: yield, with no signature") {
    def emit[A](p: Prompt[List[A]])(a: A): Unit ! Row =
      Shift.shift[List[A], Unit, P](p)(k => k(()).map(a :: _))

    def collect[A](body: Prompt[List[A]] => Unit ! Row): List[A] =
      !.run(Shift.reset[List[A], P](p => body(p).map(_ => Nil)))

    assertEquals(
      collect[Int](p => emit(p)(1).flatMap(_ => emit(p)(2)).flatMap(_ => emit(p)(3))),
      List(1, 2, 3))

    assertEquals(
      collect[Int] { p =>
        (1 to 4).foldLeft(pure[Row, Unit](())) { (acc, i) =>
          acc.flatMap(_ => if (i % 2 == 0) emit(p)(i) else pure[Row, Unit](()))
        }
      },
      List(2, 4))
  }

  test("a shift to an uninstalled prompt fails loudly, naming what IS installed") {
    val stray = Shift.prompt[Int]
    val e = intercept[NoPrompt] {
      !.run(Shift.run[Int, P](push[Int, P](Shift.prompt[Int])(Shift.shift[Int, Int, P](stray)(k => k(1)))))
    }
    assertEquals(e.installed.size, 1)
    assert(e.installed.head.startsWith("prompt @ TestDelim.scala:"), e.installed.head)
    assert(e.getMessage.contains("ONE `Shift.run` PER PROGRAM"), e.getMessage)
  }

  test("At.here names this file and this line, and two lines differ") {
    val a = At.here
    val b = At.here
    assert(a.where.startsWith("TestDelim.scala:"), a.where)
    assertNotEquals(a.where, b.where, "two call sites got the same position")
    assertEquals(a.where.split(':')(1).toInt + 1, b.where.split(':')(1).toInt)
  }

  test("At.here is the CALLER's position, not the library's") {
    def door(implicit at: At): String = at.where
    assert(door.startsWith("TestDelim.scala:"), door)
    assert(Shift.prompt[Int].label.startsWith("prompt @ TestDelim.scala:"), Shift.prompt[Int].label)
  }

  test("At: a lexical At overrides the caller's position") {
    def here: String = {
      implicit val at: At = At("Booking.scala:31")
      Shift.prompt[Int].label
    }
    assertEquals(here, "prompt @ Booking.scala:31")
  }

  test("stack safety: a thousand nested captures, and 100k pushes") {
    def deep(p: Prompt[Int], n: Int): Int ! Row =
      if (n == 0) pure[Row, Int](0)
      else Shift.shift[Int, Int, P](p)(k => k(n)).flatMap(x => deep(p, n - 1).map(_ + x))
    assertEquals(!.run(Shift.reset[Int, P](p => deep(p, 1000))), 500500)

    def pushes(n: Int): Int ! Row =
      if (n == 0) pure[Row, Int](0)
      else push[Int, P](Shift.prompt[Int])(pure[Row, Int](1)).flatMap(x => pushes(n - 1).map(_ + x))
    assertEquals(!.run(Shift.run[Int, P](pushes(100000))), 100000)
  }

  // ---- the typed door: the evidence, not the prompt

  test("Prompted: a function that captures is written apart, and runs only inside `delimited`") {
    type W = Writer[String]
    def banner(in: Shift.Prompted.Aux[Int, W]): Int ! (Shift[Any] + W) =
      Writer.tell("hello").at[Shift[Any] + W].flatMap(_ => Shift.shift[Int, Int](in)(k => k(5)).map(_ + 1))
    assertEquals(!.run(Writer.run(Shift.delimited[Int, W](banner))), (Seq("hello"), 6))
  }

  test("Prompted: the evidence cannot be forged") {
    val e = compileErrors("new okay2.Shift.Prompted[Int](okay2.Shift.prompt[Int]) { type Rest = okay2.Pure }")
    assert(e.nonEmpty, "the evidence was constructible outside the object")
  }

  test("Prompted: nested delimiters, the inner one in force") {
    val prog: Int ! P = Shift.delimited[Int, P] { _ =>
      Shift.scope[Int, P] { in =>
        Shift.shift[Int, Int](in)(k => k(5)).map(_ + 1)
      }.map(_ + 10)
    }
    assertEquals(!.run(prog), 16)
  }

  test("scope: WHICH delimiter a capture names decides how much it skips") {
    def prog(f: (Shift.Prompted.Aux[Int, P], Shift.Prompted.Aux[Int, P]) => Int ! Row): Int =
      !.run(Shift.delimited[Int, P] { outer =>
        Shift.scope[Int, P](in => f(outer, in).map(_ + 1)).map(_ + 1000)
      })
    // invoking k: the OUTER capture holds BOTH tails, and the whole thing re-runs
    assertEquals(prog((o, _) => Shift.shift[Int, Int](o)(k => k(5))), 1006)
    // dropping it, named OUTER: both tails go, the block answers 5
    assertEquals(prog((o, _) => Shift.shift[Int, Int](o)(_ => pure[Row, Int](5))), 5)
    // dropping it, named INNER: only the inner tail goes
    assertEquals(prog((_, i) => Shift.shift[Int, Int](i)(_ => pure[Row, Int](5))), 1005)
  }

  test("a second machine NESTS on the first (shift-merge-guard): collect inside a running machine is collecting") {
    // until okay2-shift-merge-guard this shape was a compile error naming `collecting`; the row says a machine
    // runs, so the door pushes its delimiter on it instead
    val r: List[Int] ! P = Shift.delimited[List[Int], P] { _ =>
      Shift.collect[Int, Shift[Any] + P](em => Shift.emit(em)(1).flatMap(_ => Shift.emit(em)(2)))
    }
    assertEquals(!.run(r), List(1, 2))
  }
}

/**
 * THE MACHINE THAT FORWARDS: a machine that meets a capture for a
 * prompt it does not hold re-emits it outward instead of throwing.
 * Each of the three claims is a separate test: the inner machine's
 * frames end up INSIDE the outer capture; multi-shot survives; the
 * inner delimiter is re-installed on resume.
 */
class TestDelimForward extends munit.FunSuite {

  type P = Pure

  /** an inner machine over a row that still has a Shift[Any] in it: the
   * shape `Shift.run` refuses and `runNested` is for */
  def inner[A](p: A ! (Shift[Any] + (Shift[Any] + P))): A ! (Shift[Any] + P) = Shift.runNested[A, Shift[Any] + P](p)

  test("1 · the inner machine's frames are INSIDE the outer capture") {
    val outer = Shift.prompt[Int]
    val prog: Int ! P = Shift.run[Int, P](Shift.push[Int, P](outer)(
      inner[Int](
        Shift.shift[Int, Int, Shift[Any] + P](outer)(k => k(5)).map(_ + 1)
      ).map(_ + 100)))
    assertEquals(!.run(prog), 106)
  }

  test("1b · dropping the forwarded continuation skips BOTH tails") {
    val outer = Shift.prompt[Int]
    var ranInner = false
    var ranOuter = false
    val prog: Int ! P = Shift.run[Int, P](Shift.push[Int, P](outer)(
      inner[Int](
        Shift.shift[Int, Int, Shift[Any] + P](outer)(_ => pure[Shift[Any] + (Shift[Any] + P), Int](5))
          .map { x => ranInner = true; x + 1 }
      ).map { x => ranOuter = true; x + 100 }))
    assertEquals(!.run(prog), 5)
    assert(!ranInner, "the inner tail ran after its continuation was dropped")
    assert(!ranOuter, "the outer tail ran after the continuation was dropped")
  }

  test("2 · MULTI-SHOT survives forwarding: the inner machine is re-entered") {
    val outer = Shift.prompt[Int]
    val prog: Int ! P = Shift.run[Int, P](Shift.push[Int, P](outer)(
      inner[Int](
        Shift.shift[Int, Int, Shift[Any] + P](outer)(k =>
          k(1).flatMap(a => k(2).map(b => a + b))
        ).map(_ * 10)
      )))
    assertEquals(!.run(prog), 30)
  }

  test("3 · the inner delimiter is still installed when the continuation resumes") {
    val outer = Shift.prompt[Int]
    val innerP = Shift.prompt[Int]
    val prog: Int ! P = Shift.run[Int, P](Shift.push[Int, P](outer)(
      inner[Int](Shift.push[Int, Shift[Any] + P](innerP)(
        Shift.shift[Int, Int, Shift[Any] + P](outer)(k => k(5)).flatMap(x =>
          Shift.shift[Int, Int, Shift[Any] + P](innerP)(k2 => k2(x * 2))).map(_ + 1)))))
    assertEquals(!.run(prog), 11)
  }

  test("a capture the OUTER machine cannot place either still throws, and says so") {
    val stray = Shift.prompt[Int]
    val outer = Shift.prompt[Int]
    def prog: Int ! P = Shift.run[Int, P](Shift.push[Int, P](outer)(
      inner[Int](Shift.shift[Int, Int, Shift[Any] + P](stray)(k => k(5)))))
    val e = intercept[NoPrompt](!.run(prog))
    assert(e.installed.nonEmpty, "the outer machine reported no delimiters")
  }

  test("`run` still throws: forwarding is opt-in") {
    val stray = Shift.prompt[Int]
    val _ = intercept[NoPrompt](
      !.run(Shift.run[Int, P](Shift.shift[Int, Int, P](stray)(k => k(1)))))
  }
}

/** a tree to walk (top-level: a case class nested in a suite trips
 * -Xlint's outer-reference check on every type test) */
object Trees {
  sealed trait Tree
  final case class Leaf(a: Int) extends Tree
  final case class Node(l: Tree, r: Tree) extends Tree
}

/**
 * THE FOUR PATTERNS: each test is a shape that shows up in ordinary
 * code, written with the named combinator rather than a raw `shift`,
 * and where it is short enough to be fair, next to the usual way.
 */
class TestDelimPatterns extends munit.FunSuite {

  type P = Pure
  type R = Shift[Any] + P

  // ---- 1 · leave early with an answer

  val txs = List(10, 20, 30, 40)
  val rules: List[Int => Boolean] = List(_ > 25, _ % 7 == 0)

  def firstHit: Option[Int] ! P = Shift.delimited[Option[Int], P] { in =>
    txs.foldLeft(pure[R, Unit](())) { (acc, t) =>
      acc.flatMap(_ => rules.foldLeft(pure[R, Unit](())) { (acc2, r) =>
        acc2.flatMap(_ => if (r(t)) Shift.exit(in)(Some(t)) else pure[R, Unit](()))   // out of BOTH loops
      })
    }.map(_ => None)
  }

  test("exit: leaving two nested loops with an answer") {
    assertEquals(!.run(firstHit), Some(30))
    val usual = txs.foldLeft(Option.empty[Int]) { (acc, t) =>
      if (acc.isDefined) acc
      else if (rules.exists(_(t))) Some(t) else acc
    }
    assertEquals(usual, Some(30))
  }

  test("exit: nothing matches, so the block runs to its end") {
    val none = Shift.delimited[Option[Int], P] { in =>
      List(1, 2).foldLeft(pure[R, Unit](())) { (acc, t) =>
        acc.flatMap(_ => if (t > 100) Shift.exit(in)(Some(t)) else pure[R, Unit](()))
      }.map(_ => None)
    }
    assertEquals(!.run(none), None)
  }

  // ---- 2 · a push producer read as a pull

  import Trees._

  /** an ordinary recursive walk: it emits, and knows nothing else */
  def walk(t: Tree)(e: Shift.Emitting.Aux[Int, P]): Unit ! R = t match {
    case Leaf(a) => Shift.emit(e)(a)
    case Node(l, r) => walk(l)(e).flatMap(_ => walk(r)(e))
  }

  test("collect/emit: the producer stays a walk, the caller gets a list") {
    val t = Node(Node(Leaf(1), Leaf(2)), Leaf(3))
    assertEquals(!.run(Shift.collect[Int, P](walk(t))), List(1, 2, 3))
    assertEquals(!.run(Shift.collect[Int, P](walk(Leaf(7)))), List(7))
    assertEquals(!.run(Shift.collect[Int, P](_ => pure[R, Unit](()))), List.empty[Int])
  }

  test("collectUntil: the SAME producer, stopped at the third leaf — the walk goes no further") {
    var visited = 0
    def counting(t: Tree)(e: Shift.Emitting.Aux[Int, P]): Unit ! R = t match {
      case Leaf(a) => visited += 1; Shift.emit(e)(a)
      case Node(l, r) => counting(l)(e).flatMap(_ => counting(r)(e))
    }
    val t = Node(Node(Leaf(1), Leaf(2)), Node(Leaf(3), Node(Leaf(4), Leaf(5))))
    assertEquals(!.run(Shift.collectUntil[Int, Vector[Int], Vector[Int], P](FoldUntil.take(3))(counting(t))), Vector(1, 2, 3))
    assertEquals(visited, 3)
    // an endless producer ends where the fold says
    def nat(i: Int)(e: Shift.Emitting.Aux[Int, P]): Unit ! R = Shift.emit(e)(i).flatMap(_ => nat(i + 1)(e))
    assertEquals(!.run(Shift.collectUntil[Int, Option[Int], Option[Int], P](FoldUntil.find[Int](_ > 10))(nat(0))), Some(11))
    // done(init) runs no body at all
    assertEquals(!.run(Shift.collectUntil[Int, Vector[Int], Vector[Int], P](FoldUntil.take(0))(nat(0))), Vector.empty[Int])
  }

  // ---- 3 · stop in the middle, carry on later

  def booking(s: Shift.Asking.Aux[String, String, String, P]): String ! R = for {
    city <- Shift.pause(s)("Which city?")
    nights <- Shift.pause(s)(s"How many nights in $city?")
    pay <- Shift.pause(s)(s"Pay ${nights.toInt * 90} for $city?")
  } yield if (pay == "yes") s"Booked $city for $nights nights" else "Cancelled"

  def answering(as: List[String]): String => String ! P = {
    var left = as
    _ => { val a = left.head; left = left.tail; pure[P, String](a) }
  }

  test("pause/resumable: the rest of the dialogue is a value") {
    val start = !.run(Shift.resumable[String, String, String, P](booking))
    assertEquals(start.asking, Some("Which city?"))
    assert(start.where.exists(_.startsWith("TestDelim.scala:")), start.where.toString)
    assertEquals(!.run(Shift.drive(start)(answering(List("Kyiv", "3", "yes")))), "Booked Kyiv for 3 nights")
    // the SAME paused dialogue, answered again and differently
    assertEquals(!.run(Shift.drive(start)(answering(List("Lviv", "2", "no")))), "Cancelled")
  }

  test("pause: a program that never asks is Done already") {
    val p = !.run(Shift.resumable[String, String, Int, P](_ => pure[R, Int](41 + 1)))
    assertEquals(p.finished, Some(42))
    assertEquals(!.run(Shift.drive(p)(_ => pure[P, String]("unused"))), 42)
  }

  // ---- 4 · do something on the way back

  test("onReturn: the rest of the block is a value you can act on") {
    val r = !.run(Shift.delimited[Int, P] { in =>
      Shift.onReturn(in)(n => n * 10).map(_ => 1 + 2)   // runs LAST, on whatever comes back
    })
    assertEquals(r, 30)
  }

  test("onReturn: a compensation folded into the answer from the middle") {
    def charge(amount: Int, ok: Boolean): String ! P =
      Shift.delimited[String, P] { in =>
        Shift.onReturn(in)(s => if (s.startsWith("failed")) s"$s; refunded $amount" else s)
          .map(_ => if (ok) s"charged $amount" else "failed: card declined")
      }
    assertEquals(!.run(charge(90, true)), "charged 90")
    assertEquals(!.run(charge(90, false)), "failed: card declined; refunded 90")
  }
}

/**
 * ONE MACHINE, MANY DELIMITERS: `scope`/`collecting`/`pausing` install
 * a delimiter and leave the machine alone, so the outermost combinator
 * is the only one that runs, and a capture crosses the nested ones.
 */
class TestDelimNesting extends munit.FunSuite {

  type P = Pure
  type R = Shift[Any] + P

  /** a producer that PAUSES in the middle of producing */
  def half(s: Shift.Asking.Aux[String, Int, List[Int], P]): List[Int] ! R =
    Shift.collecting[Int, P] { e =>
      for {
        _ <- Shift.emit(e)(1)
        more <- Shift.pause(s)("more?")     // crosses the collect's delimiter
        _ <- Shift.emit(e)(more)
        _ <- Shift.emit(e)(3)
      } yield ()
    }

  test("a pause crosses an intervening collect, and the list survives it") {
    val start = !.run(Shift.resumable[String, Int, List[Int], P](half))
    assertEquals(start.asking, Some("more?"))
    assertEquals(!.run(Shift.drive(start)(_ => pure[P, Int](2))), List(1, 2, 3))
    assertEquals(!.run(Shift.drive(start)(_ => pure[P, Int](7))), List(1, 7, 3))
  }

  test("the same dialogue replays from its journal, producer and all") {
    val back = !.run(Shift.replay[String, Int, List[Int], P](half)(List(5)))
    assertEquals(back.finished, Some(List(1, 5, 3)))
  }

  test("exit crosses a nested producer: out of the whole block, with an answer") {
    def run(stopAt: Int): String ! P = Shift.delimited[String, P] { out =>
      Shift.collecting[Int, P] { e =>
        def go(i: Int): Unit ! R =
          if (i >= 5) pure[R, Unit](())
          else (if (i == stopAt) Shift.exit(out)(s"stopped at $i") else pure[R, Unit](()))
            .flatMap(_ => Shift.emit(e)(i)).flatMap(_ => go(i + 1))
        go(0)
      }.map(xs => s"collected $xs")
    }
    assertEquals(!.run(run(3)), "stopped at 3")
    assertEquals(!.run(run(9)), "collected List(0, 1, 2, 3, 4)")
  }

  test("pausing nests inside delimited: the dialogue's delimiter goes on the running machine") {
    val prog: Shift.Dialogue[String, Int, Int, P] ! P = Shift.delimited[Shift.Dialogue[String, Int, Int, P], P] { _ =>
      Shift.pausing[String, Int, Int, P](s => Shift.pause(s)("n?").map(_ * 2))
    }
    val d = !.run(prog)
    assertEquals(d.asking, Some("n?"))
    assertEquals(!.run(Shift.drive(d)(_ => pure[P, Int](21))), 42)
  }
}

/**
 * A PAUSED DIALOGUE THAT OUTLIVES THE PROCESS: the continuation is a
 * closure and cannot be written down; the JOURNAL can, and the paused
 * state is re-derived from the program plus the journal. Including the
 * limit, measured: what replay re-runs, and the discipline under which
 * it re-runs nothing.
 */
class TestDelimPersist extends munit.FunSuite {

  type P = Pure
  type Row = Shift[Any] + P

  def booking(s: Shift.Asking.Aux[String, String, String, P]): String ! Row = for {
    city <- Shift.pause(s)("Which city?")
    nights <- Shift.pause(s)(s"How many nights in $city?")
    pay <- Shift.pause(s)(s"Pay ${nights.toInt * 90} for $city?")
  } yield if (pay == "yes") s"Booked $city for $nights nights" else "Cancelled"

  test("replay: a dialogue survives a restart, because its journal does") {
    val p0 = !.run(Shift.resumable[String, String, String, P](booking))
    assertEquals(p0.asking, Some("Which city?"))
    val (p1, j1) = !.run(Shift.answer(p0, List.empty[String])("Kyiv"))
    val (p2, j2) = !.run(Shift.answer(p1, j1)("3"))
    assertEquals(j2, List("Kyiv", "3"))
    assertEquals(p2.asking, Some("Pay 270 for Kyiv?"))

    // the process dies; the only thing written down is j2
    val back = !.run(Shift.replay[String, String, String, P](booking)(j2))
    assertEquals(back.asking, Some("Pay 270 for Kyiv?"))
    val (end, j3) = !.run(Shift.answer(back, j2)("yes"))
    assertEquals(end.finished, Some("Booked Kyiv for 3 nights"))
    assertEquals(j3, List("Kyiv", "3", "yes"))
  }

  test("replay: the empty journal is where it started; a full one comes back finished") {
    val fresh = !.run(Shift.replay[String, String, String, P](booking)(Nil))
    assertEquals(fresh.asking, Some("Which city?"))
    val done = !.run(Shift.replay[String, String, String, P](booking)(List("Lviv", "2", "no")))
    assertEquals(done.finished, Some("Cancelled"))
    val over = !.run(Shift.replay[String, String, String, P](booking)(List("Lviv", "2", "no", "stray")))
    assertEquals(over.finished, Some("Cancelled"))
  }

  // ---- the limit, measured

  type Log = Writer[String] + P
  type Logged = Shift[Any] + Log
  type Where = Shift.Dialogue[String, String, String, Log]

  /** an effect OUTSIDE pause: this is what replay re-runs */
  def chatty(s: Shift.Asking.Aux[String, String, String, Log]): String ! Logged = for {
    _ <- Writer.tell("asking for the city").at[Logged]
    city <- Shift.pause(s)("Which city?")
    _ <- Writer.tell(s"got $city").at[Logged]
    nights <- Shift.pause(s)("How many nights?")
    _ <- Writer.tell(s"booking $city for $nights").at[Logged]
  } yield s"$city/$nights"

  test("the discipline is a TYPE: a row with a Writer in it is refused as replayable") {
    val e = compileErrors("okay2.Shift.replay[String, String, String, Log](chatty)(Nil)")
    assert(e.contains("REPLAY WOULD PERFORM AGAIN"), e)
  }

  test("the limit: replay re-runs what did not come through pause") {
    implicit val breach: Replayable[Logged] = Replayable.unchecked

    def go(j: List[String]) = Writer.run(
      Shift.replay[String, String, String, Log](chatty)(j))

    val (log1, p1) = !.run(go(List("Kyiv")))
    assertEquals(log1.toList, List("asking for the city", "got Kyiv"))
    assertEquals(p1.asking, Some("How many nights?"))

    val (log2, p2) = !.run(go(List("Kyiv", "3")))
    assertEquals(log2.toList, List("asking for the city", "got Kyiv", "booking Kyiv for 3"))
    assertEquals(p2.finished, Some("Kyiv/3"))
    val said = log1.count(_ == "asking for the city") + log2.count(_ == "asking for the city")
    assertEquals(said, 2)
  }

  test("the discipline: what goes through pause is performed once") {
    var performed = List.empty[String]
    def perform(q: String): String = {
      performed = performed :+ q
      q match {
        case "rate:EUR" => "42"
        case other => s"?$other"
      }
    }

    def priced(s: Shift.Asking.Aux[String, String, String, P]): String ! Row = for {
      rate <- Shift.pause(s)("rate:EUR")
      fee <- Shift.pause(s)("fee:standard")
    } yield s"$rate/$fee"

    val p0 = !.run(Shift.resumable[String, String, String, P](priced))
    val (p1, j1) = !.run(Shift.answer(p0, List.empty[String])(perform(p0.asking.get)))
    assertEquals(performed, List("rate:EUR"))

    val back = !.run(Shift.replay[String, String, String, P](priced)(j1))
    assertEquals(back.asking, Some("fee:standard"))
    val (end, _) = !.run(Shift.answer(back, j1)(perform(back.asking.get)))
    assertEquals(end.finished, Some("42/?fee:standard"))
    assertEquals(performed, List("rate:EUR", "fee:standard"))
    val _ = p1
  }

  test("Replayable over the row: State, Reader and Throws beside Shift[Any] pass, in any nesting; Once and a user effect are refused") {
    type Ok = Shift[Any] + (State[Int] + (Reader[String] + Throws[String]))
    val _ = implicitly[Replayable[Ok]]
    val _ = implicitly[Replayable[Throws[String] + Shift[Any] + State[Int]]]
    val once = compileErrors("implicitly[okay2.Replayable[okay2.Shift[Any] + (okay2.State[Int] + okay2.Once)]]")
    assert(once.contains("REPLAY WOULD PERFORM AGAIN") && once.contains("Once"), once)
    val own = compileErrors("implicitly[okay2.Replayable[okay2.Shift[Any] + okay2.Produce]]")
    assert(own.contains("REPLAY WOULD PERFORM AGAIN") && own.contains("Produce"), own)
  }
}
