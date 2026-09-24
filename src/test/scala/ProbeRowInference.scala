package okay

import okay.Row.at

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
      val p: Int ! okay.Writer % String = okay.Writer.tell("x").map(_ => 1)
      val q: Int ! okay.Writer % String + okay.Pure = p
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
      type Row = okay.State % Int + okay.Throws % String
      okay.State.set[Int](5).at[Row].flatMap(_ => okay.raise[String, Int]("x"))
    """)
    assert(e.nonEmpty, "flatMap accepted a bare-row continuation against a widened receiver")
  }

  test("...both sides widened, it composes") {
    type Row = State % Int + Throws % String
    val p: Int ! Row = State.set[Int](5).at[Row].flatMap(_ => raise[String, Int]("x").at[Row])
    assertEquals(State.run(0)(runEither[Int, State % Int, String](p)), (5, Left("x")))
  }

  // ---------------------------------------------------------- shape 3: a union-typed argument does not recover F + G

  test("a method taking `R ! Delim + F` does not recover F from an argument ALREADY typed as the expanded union, without explicit type args") {
    // Delim.Stacked.delimited's own construction (Delim.scala) needed
    // `push[R, F](...)`/`run[R, F](...)` spelled explicitly for exactly
    // this reason; reproduced minimally here rather than asserted from
    // memory
    val e = compileErrors("""
      def wants[R, F[+_]](p: R ! okay.Delim + F): Unit = ()
      def has[R, F[+_]](p: R ! ([A] =>> okay.Delim[A] | F[A])): Unit = wants(p)
    """)
    assert(e.nonEmpty, "an argument typed as the expanded union satisfied Delim + F with no explicit type args")
  }

  test("...spelling the type arguments at the call site fixes it") {
    def wants[R, F[+_]](p: R ! Delim + F): Unit = ()
    def has[R, F[+_]](p: R ! ([A] =>> Delim[A] | F[A])): Unit = wants[R, F](p)
    has[Int, okay.Pure](Delim.push(Delim.prompt[Int])(pure(1)))
  }

  // ---------------------------------------------------------- shape 4: ACI, precisely

  test("re-parenthesization (associativity) satisfies an ascription with no .at") {
    val e = compileErrors("""
      type A = okay.State % Int + (okay.Writer % Int + okay.Reader % Long)
      val p: Unit ! ((okay.State % Int + okay.Writer % Int) + okay.Reader % Long) = okay.pure(())
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
      val p: Unit ! okay.Writer % Int + okay.State % Int = okay.pure(())
      val q: Unit ! okay.State % Int + okay.Writer % Int = p
      q
    """)
    assert(e.isEmpty, s"swapping the order of two different union members needed help after all: $e")
  }

  test("and the REVERSE: X + Pure satisfies a bare X slot too — the spike's answer, no new given needed") {
    val e = compileErrors("""
      val p: Int ! okay.Writer % String + okay.Pure = okay.Writer.tell("x").map(_ => 1)
      val q: Int ! okay.Writer % String = p
      q
    """)
    assert(e.isEmpty, s"X + Pure did not satisfy a bare X ascription: $e")
  }
