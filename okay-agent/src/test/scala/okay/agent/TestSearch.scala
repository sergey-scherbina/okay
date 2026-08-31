package okay.agent

import okay.{!, +, Choose, Handler, effect, guard, pure, runChoice}
import okay.given

/**
 * Search over completions, and the thing that makes it correct: the
 * context handler THREADS its state, so a multi-shot branch cannot
 * see a sibling's turns.
 */
class TestSearch extends munit.FunSuite {

  /** a model whose replies cycle, so N samples differ */
  def cycling(replies: Seq[String]): Handler[Model] = new:
    private var i = 0
    def handle[A](e: Model[A]): A = e match
      case Model.Complete(_, _) =>
        val r = replies(i % replies.length)
        i += 1
        Reply(r, Nil)
      case Model.Count(t) => t.length / 4

  /** the searching row: Choose outermost, so Logic's shape fits */
  type Rest = Model + Context
  type Row[A] = A ! (Choose + Rest)

  def complete: Row[String] =
    effect[Choose + Rest, Reply](Model.Complete(Nil, Nil)).map(_.text)

  def ok(p: Boolean): Row[Unit] = guard[[X] =>> X ! (Choose + Rest)](p)

  /**
   * Memory INSIDE the search: the context handler runs first, so it
   * captures its state at each choice point and every branch that
   * the multi-shot Choose handler explores resumes from THAT state,
   * not from a sibling's.
   */
  def perBranch[A](prog: Row[A])(model: Handler[Model]): Seq[A] =
    given Handler[Model] = model
    val threaded: A ! (Choose + Model) =
      Memory.run[Vector[Turn], A, Choose + Model](Compact.all)(prog)
    runChoice[A, Model](threaded).runWith

  test("best-of-N: sample until one validates, then commit") {
    val model = cycling(Seq("bad", "also bad", "42", "43"))
    val found = perBranch(Search.bestOf[String, Rest](4)(complete)(_ == "42"))(model)
    assertEquals(found, Seq("42"))
  }

  test("best-of-N with nothing valid answers nothing (not an exception)") {
    val model = cycling(Seq("bad", "worse"))
    val found = perBranch(Search.bestOf[String, Rest](2)(complete)(_ == "42"))(model)
    assertEquals(found, Seq.empty)
  }

  test("the soft cut: the fallback runs ONLY when no sample validates") {
    def attempt(want: String): Row[String] =
      Search.samples[String, Rest](2)(complete).flatMap(a => ok(a == want).map(_ => a))

    val used = perBranch(Search.validated[String, String, Rest](
      attempt("ok"))(a => pure(s"used $a"))(pure("reprompted")))(cycling(Seq("ok")))
    assertEquals(used, Seq("used ok", "used ok"))   // BOTH good samples used

    val fell = perBranch(Search.validated[String, String, Rest](
      attempt("ok"))(a => pure(s"used $a"))(pure("reprompted")))(cycling(Seq("nope")))
    assertEquals(fell, Seq("reprompted"))
  }

  test("branches do NOT leak context into each other (threaded state)") {
    // each branch remembers its own sample, then reads the context back
    val prog: Row[Int] =
      Search.samples[String, Rest](2)(complete).flatMap { s =>
        effect[Choose + Rest, Unit](Context.Remember(Turn.Assistant(s))).flatMap(_ =>
          effect[Choose + Rest, Seq[Turn]](Context.Recall()).map(_.length))
      }
    assertEquals(perBranch(prog)(cycling(Seq("a", "b"))), Seq(1, 1))
  }

  test("handled the other way round, the transcript is SHARED") {
    // Memory OUTSIDE the search: one conversation records every branch
    given Handler[Model] = cycling(Seq("a", "b"))
    val prog: Row[Int] =
      Search.samples[String, Rest](2)(complete).flatMap { s =>
        effect[Choose + Rest, Unit](Context.Remember(Turn.Assistant(s))).flatMap(_ =>
          effect[Choose + Rest, Seq[Turn]](Context.Recall()).map(_.length))
      }
    val shared: Seq[Int] ! (Model + Context) = runChoice[Int, Model + Context](prog)
    val (state, lengths) =
      Memory.runWithState[Vector[Turn], Seq[Int], Model](Compact.all)(
        shared.asInstanceOf[Seq[Int] ! (Context + Model)]).runWith
    assertEquals(lengths, Seq(1, 2))     // the second branch sees the first
    assertEquals(state.length, 2)        // one transcript, both attempts
  }

  test("majority vote over samples is a plain fold over the answers") {
    given Handler[Model] = cycling(Seq("7", "7", "8"))
    val answers: Seq[String] ! (Model + Context) =
      Search.all[String, Rest](3)(complete)(_ => true)
    val got = Memory.run[Vector[Turn], Seq[String], Model](Compact.all)(
      answers.asInstanceOf[Seq[String] ! (Context + Model)]).runWith
    assertEquals(got.length, 3)
    assertEquals(Search.majority(got), Some("7"))
  }
}
