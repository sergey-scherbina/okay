package okay

import !.*
import Delim.{abort, push, reset, shift}

/**
 * Delimited control as an effect: the classic shift/reset laws, and
 * the two things a single-prompt design cannot do — escaping PAST an
 * intervening delimiter, and two delimiters with different answer
 * types in one row.
 */
class TestDelim extends munit.FunSuite {

  type Row = Delim + okay.Pure

  test("shift/reset: the continuation comes back as a value") {
    // reset { shift(k => k(5) * 2) } == 10
    val r = !.run(reset[Int, okay.Pure] { p =>
      shift[Int, Int, okay.Pure](p)(k => k(5).map(_ * 2))
    })
    assertEquals(r, 10)
  }

  test("the captured continuation includes what follows the shift") {
    // reset { shift(k => k(1)) + 10 }  — the +10 is inside k
    val r = !.run(reset[Int, okay.Pure] { p =>
      shift[Int, Int, okay.Pure](p)(k => k(1)).map(_ + 10)
    })
    assertEquals(r, 11)
  }

  test("dropping the continuation is an early exit") {
    var reached = false
    val r = !.run(reset[Int, okay.Pure] { p =>
      shift[Int, Int, okay.Pure](p)(_ => okay.pure(42))
        .map { x => reached = true; x + 1 }
    })
    assertEquals(r, 42)
    assert(!reached, "the abandoned continuation ran anyway")
  }

  test("abort: the same thing, named") {
    val r = !.run(reset[Int, okay.Pure] { p =>
      abort[Int, Int, okay.Pure](p)(7).map(_ + 100)
    })
    assertEquals(r, 7)
  }

  test("multi-shot: the continuation is a value, so invoke it twice") {
    // reset { (shift(k => k(1) + k(2))) * 10 } == 10 + 20 == 30
    val r = !.run(reset[Int, okay.Pure] { p =>
      shift[Int, Int, okay.Pure](p) { k =>
        k(1).flatMap(a => k(2).map(b => a + b))
      }.map(_ * 10)
    })
    assertEquals(r, 30)
  }

  test("MULTI-PROMPT: a shift escapes past an intervening delimiter") {
    val outer = Delim.prompt[Int]
    val inner = Delim.prompt[Int]
    var innerFinished = false

    val prog: Int ! Row =
      push[Int, okay.Pure](outer) {
        push[Int, okay.Pure](inner) {
          // jumps over `inner` straight to `outer`
          shift[Int, Int, okay.Pure](outer)(_ => okay.pure(99))
        }.map { x => innerFinished = true; x + 1 }
      }.map(_ + 1000)

    // 99 becomes the value of the OUTER push, so what follows the
    // push — outside the delimiter, not captured — still runs: 1099.
    // What was skipped is everything between the shift and that
    // prompt, the inner delimiter's tail included.
    assertEquals(!.run(Delim.run[Int, okay.Pure](prog)), 1099)
    assert(!innerFinished, "the intervening delimiter's tail ran")
  }

  test("two prompts of DIFFERENT answer types live in one row") {
    val num = Delim.prompt[Int]
    val str = Delim.prompt[String]

    val prog: String ! Row =
      push[String, okay.Pure](str) {
        push[Int, okay.Pure](num) {
          shift[Int, Int, okay.Pure](num)(k => k(21).map(_ * 2))
        }.flatMap(n => shift[String, String, okay.Pure](str)(_ => okay.pure(s"n=$n")))
      }

    assertEquals(!.run(Delim.run[String, okay.Pure](prog)), "n=42")
  }

  test("the captured continuation re-installs its own prompt") {
    // k invoked twice, and each invocation can shift again
    val r = !.run(reset[Int, okay.Pure] { p =>
      shift[Int, Int, okay.Pure](p) { k =>
        k(1).flatMap(a =>
          if a < 5 then k(a + 1).map(_ + 100) else okay.pure(a))
      }.map(_ * 2)
    })
    // k(1) = 2; 2 < 5 so k(3) = 6, +100 = 106
    assertEquals(r, 106)
  }

  test("other effects pass through the machine untouched") {
    type F = Writer % String
    val told = Delim.run[Int, F] {
      push[Int, F](Delim.prompt[Int]) {
        okay.effect[Delim + F, String](Writer("before")).flatMap(_ =>
          okay.pure(1))
      }.flatMap(x =>
        okay.effect[Delim + F, String](Writer("after")).map(_ => x + 1))
    }
    val (ws, a) = !.run(Writer.run[String, Int, okay.Pure](told))
    assertEquals(a, 2)
    assertEquals(ws, Seq("before", "after"))
  }

  test("effects inside an abandoned continuation do NOT run") {
    type F = Writer % String
    val prog = Delim.run[Int, F] {
      Delim.prompt[Int] match
        case p => push[Int, F](p) {
          shift[Int, Int, F](p)(_ => okay.pure(5)).flatMap(x =>
            okay.effect[Delim + F, String](Writer("never")).map(_ => x))
        }
    }
    val (ws, a) = !.run(Writer.run[String, Int, okay.Pure](prog))
    assertEquals(a, 5)
    assertEquals(ws, Seq.empty, "the dropped continuation told anyway")
  }

  test("shift vs shift0: does the body keep the delimiter?") {
    // shift puts the body back under the prompt, so a SECOND shift to
    // the same prompt from inside f finds it
    val nested = !.run(reset[Int, okay.Pure] { p =>
      Delim.shift[Int, Int, okay.Pure](p)(_ =>
        Delim.shift[Int, Int, okay.Pure](p)(_ => okay.pure(1)))
    })
    assertEquals(nested, 1)

    // shift0 CONSUMES it, so the same program escapes past the
    // delimiter and finds nothing
    intercept[NoPrompt] {
      !.run(reset[Int, okay.Pure] { p =>
        Delim.shift0[Int, Int, okay.Pure](p)(_ =>
          Delim.shift0[Int, Int, okay.Pure](p)(_ => okay.pure(1)))
      })
    }
  }

  test("shift0 vs control0: does the continuation re-install it?") {
    // the captured continuation contains a shift to the same prompt.
    // shift0's k re-installs the delimiter, so that shift finds one
    val ok = !.run(reset[Int, okay.Pure] { p =>
      Delim.shift0[Int, Int, okay.Pure](p)(k => k(1))
        .flatMap(x => Delim.shift0[Int, Int, okay.Pure](p)(_ => okay.pure(x + 40)))
    })
    assertEquals(ok, 41)

    // control0 hands back a BARE segment: nothing re-installs it
    intercept[NoPrompt] {
      !.run(reset[Int, okay.Pure] { p =>
        Delim.control0[Int, Int, okay.Pure](p)(k => k(1))
          .flatMap(x => Delim.control0[Int, Int, okay.Pure](p)(_ => okay.pure(x)))
      })
    }
  }

  test("control: the body keeps the delimiter, the continuation does not") {
    val r = !.run(reset[Int, okay.Pure] { p =>
      Delim.control[Int, Int, okay.Pure](p)(k => k(1)).map(_ + 10)
    })
    assertEquals(r, 11)
  }

  test("a new effect defined in USER code: yield, with no signature") {
    // the payoff of having delimited control as an effect — a
    // generator needs no new operation and no new handler, only a
    // prompt whose answer type is the list being built
    def emit[A](p: Prompt[List[A]])(a: A): Unit ! Row =
      Delim.shift[List[A], Unit, okay.Pure](p)(k => k(()).map(a :: _))

    def collect[A](body: Prompt[List[A]] => Unit ! Row): List[A] =
      !.run(reset[List[A], okay.Pure](p => body(p).map(_ => Nil)))

    assertEquals(
      collect[Int](p => emit(p)(1).flatMap(_ => emit(p)(2)).flatMap(_ => emit(p)(3))),
      List(1, 2, 3))

    // and it composes with ordinary control flow
    assertEquals(
      collect[Int] { p =>
        (1 to 4).foldLeft(okay.pure[Delim + okay.Pure, Unit](())) { (acc, i) =>
          acc.flatMap(_ => if i % 2 == 0 then emit(p)(i) else okay.pure(()))
        }
      },
      List(2, 4))
  }

  test("a shift to an uninstalled prompt fails loudly") {
    val stray = Delim.prompt[Int]
    intercept[NoPrompt] {
      !.run(Delim.run[Int, okay.Pure](
        shift[Int, Int, okay.Pure](stray)(k => k(1))))
    }
  }
}
