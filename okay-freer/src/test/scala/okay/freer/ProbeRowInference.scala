package okay.freer

import okay.*
import okay.given

import okay.freer.Row.at

/**
 * row-inference-ergonomics: verified probes for the real friction
 * points hit today, rather than a repeat of memory. One claim in the
 * backlog entry that filed this lane ("Once + Async + Pure written
 * out") does not appear literally anywhere in the tree — checked by
 * grep before writing anything else — so this file tests only the
 * shapes with direct evidence in today's own tool history.
 */
class ProbeRowInference extends munit.FunSuite:

  // ---------------------------------------------------------- shape 1: X vs X + Pure

  test("a bare-X ascription DOES satisfy an X + Pure slot directly — no .at needed") {
    val e = compileErrors("""
      val p: Int ! okay.freer.Writer % String = okay.freer.Writer.tell("x").map(_ => 1)
      val q: Int ! okay.freer.Writer % String + okay.Pure = p
      q
    """)
    assert(e.isEmpty, s"a bare row did not satisfy its own row + Pure: $e")
  }

  test("and the ambient row's OWN operations still resolve through it — the ascription is not a coercion, it typechecks as the SAME value") {
    val p: Int ! Writer % String = Writer.tell("x").map(_ => 1)
    val q: Int ! Writer % String + okay.Pure = p
    assert(q eq p, "the ascription built a new value where none was needed")
  }

  // ---------------------------------------------------------- shape 2: flatMap needs BOTH sides at the SAME row

  test("flatMap between TWO different effects needs BOTH operands widened — the most common trap today") {
    // one side widened, the other bare: does not compile
    val e = compileErrors("""
      type Row = okay.freer.State % Int + okay.freer.Throws % String
      okay.freer.State.set[Int](5).at[Row].flatMap(_ => okay.freer.raise[String, Int]("x"))
    """)
    assert(e.nonEmpty, "flatMap accepted a bare-row continuation against a widened receiver")
  }

  test("...both sides widened, it composes") {
    type Row = State % Int + Throws % String
    val p: Int ! Row = State.set[Int](5).at[Row].flatMap(_ => raise[String, Int]("x").at[Row])
    assertEquals(State.run(0)(runEither[Int, State % Int, String](p)), (5, Left("x")))
  }

  // ---------------------------------------------------------- shape 3: a union-typed argument DOES recover F + G now

  test("a method taking `R ! Shift % ? + F` recovers F from an argument ALREADY typed as the expanded union, since the indexed base") {
    // Shift.Stacked.delimited's own construction (Shift.scala, until shift-prompt-key) needed
    // `push[R, F](...)`/`run[R, F](...)` spelled explicitly for exactly
    // this reason, and until freer-base-step-extractor (2026-09-29) this
    // probe pinned the refusal. The base's row is `Lifted[F]#L`, a class
    // projection, and the two rows now meet as `Lifted[Shift % ? + F]` against
    // `Lifted[[A] =>> Shift[?, A] | F[A]]`, which dotty solves; on the old
    // enum `Free[Shift % ? + F, R]` against the expanded union did not. The
    // explicit spellings still compile (the next probe) and may stay.
    val e = compileErrors("""
      def wants[R, F[+_]](p: R ! okay.freer.Shift % ? + F): Unit = ()
      def has[R, F[+_]](p: R ! ([A] =>> okay.freer.Shift[?, A] | F[A])): Unit = wants(p)
    """)
    assert(e.isEmpty, s"an argument typed as the expanded union no longer satisfies Shift % ? + F without type args — the base changed under this probe:\n$e")
  }

  test("...spelling the type arguments at the call site fixes it") {
    def wants[R, F[+_]](p: R ! Shift % ? + F): Unit = ()
    def has[R, F[+_]](p: R ! ([A] =>> Shift[?, A] | F[A])): Unit = wants[R, F](p)
    has[Int, okay.Pure](Shift.push(Shift.prompt[Int])(pure(1)))
  }

  // ---------------------------------------------------------- shape 4: ACI, precisely

  test("re-parenthesization (associativity) satisfies an ascription with no .at") {
    val e = compileErrors("""
      type A = okay.freer.State % Int + (okay.freer.Writer % Int + okay.freer.Reader % Long)
      val p: Unit ! ((okay.freer.State % Int + okay.freer.Writer % Int) + okay.freer.Reader % Long) = okay.freer.pure(())
      val q: Unit ! A = p
      q
    """)
    assert(e.isEmpty, s"re-parenthesizing the same left-to-right union needed help: $e")
  }

  test("TRUE reordering (commutativity, swapping two members) ALSO satisfies an ascription with no .at — dotty's | normalizes member order for type equality") {
    // the first draft of this probe assumed the opposite and was
    // wrong: this is a stronger positive fact than "union ACI lets
    // the ascription" (TestPipe's comment) states on its own
    val e = compileErrors("""
      val p: Unit ! okay.freer.Writer % Int + okay.freer.State % Int = okay.freer.pure(())
      val q: Unit ! okay.freer.State % Int + okay.freer.Writer % Int = p
      q
    """)
    assert(e.isEmpty, s"swapping the order of two different union members needed help after all: $e")
  }

  test("and the REVERSE: X + Pure satisfies a bare X slot too — the spike's answer, no new given needed") {
    val e = compileErrors("""
      val p: Int ! okay.freer.Writer % String + okay.Pure = okay.freer.Writer.tell("x").map(_ => 1)
      val q: Int ! okay.freer.Writer % String = p
      q
    """)
    assert(e.isEmpty, s"X + Pure did not satisfy a bare X ascription: $e")
  }
