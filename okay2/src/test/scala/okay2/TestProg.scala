package okay2

/**
 * The indexed program (the Scala 3 core's TestProg, its facade half):
 * sequencing checked by the compiler at zero cost. The Delim.Stacked
 * half is not ported — it needs a dependent function type for its body
 * (specs/okay2.md, stage 7).
 */
class TestProg extends munit.FunSuite {
  import TestProg._

  test("a protocol as smart constructors: the right order runs as the tree it is") {
    val p: Prog[Writer[String], Unit, Idle, Idle] =
      for {
        _ <- Tx.begin
        _ <- Tx.write("x")
        _ <- Tx.write("y")
        _ <- Tx.commit
      } yield ()
    assertEquals(!.run(Writer.collect[String, Unit, Pure](p.free)), (Vector("begin", "x", "y", "commit"), ()))
  }

  test("out of order, doubled or left half done: each is a compile error") {
    // a write before begin: write starts at Open, and the program is to start at Idle
    assert(compileErrors("val p: okay2.Prog[okay2.Writer[String], Unit, okay2.TestProg.Idle, okay2.TestProg.Idle] = " +
      "okay2.TestProg.Tx.write(\"x\").flatMap(_ => okay2.TestProg.Tx.commit)").nonEmpty, "a write outside a transaction compiled")
    assert(compileErrors("okay2.TestProg.Tx.commit.flatMap(_ => okay2.TestProg.Tx.begin)").isEmpty,
      "commit then begin is a legitimate Open -> Open shape")
    // begin twice: the second begin starts at Idle, the first ended at Open
    assert(compileErrors("okay2.TestProg.Tx.begin.flatMap(_ => okay2.TestProg.Tx.begin)").nonEmpty, "begin twice compiled")
    // begin without commit: the move is open, so there is no `free`
    val open = compileErrors("okay2.TestProg.Tx.begin.flatMap(_ => okay2.TestProg.Tx.write(\"x\")).free")
    assert(open.contains("free"), open)
  }

  test("the caveat: an abort inside a block promising a transition drops it — the type is not a run-time guarantee") {
    var moved = false
    val move: Prog[Throws[String], Unit, A, B] =
      Prog.transition[A, B, Throws[String], Unit](Free.delay[Throws[String], Unit] { () => moved = true; pure(()) })
    val p: Prog[Throws[String], Unit, A, B] =
      Prog.diag[A, Throws[String], Unit](Throws.raise[String, Unit]("no")).flatMap(_ => move)
    // the type says B is reached; the abort says otherwise
    val closed: Prog[Throws[String], Unit, A, A] = p.flatMap(_ => Prog.transition[B, A, Throws[String], Unit](pure(())))
    assertEquals(!.run(Throws.runEither[Unit, String, Pure](closed.free)), Left("no"))
    assert(!moved, "the transition ran after an abort")
  }

  test("zero cost: the facade is the tree — diag/free are identity, flatMap is the same Bind") {
    val p: Int ! Writer[String] = Writer.tell("x").map(_ => 1)
    assert(Prog.diag[A, Writer[String], Int](p).free eq p, "diag then free is not the same object")
    val f: Int => Prog[Writer[String], Int, A, A] = n => Prog.pure(n + 1)
    Prog.diag[A, Writer[String], Int](p).flatMap(f).free match {
      case Free.Bind(a, _) => assert(a eq p, "flatMap did not build a Bind over the same head")
      case other => fail(s"flatMap built $other")
    }
    assertEquals(!.run(Writer.run[String, Int, Pure](Prog.diag[A, Writer[String], Int](p).flatMap(f).free)), (Seq("x"), 2))
  }

  test("a move left open has no `free`, and continuations start where the last step ended") {
    assert(compileErrors("""
      okay2.Prog.transition[okay2.TestProg.A, okay2.TestProg.B, okay2.Writer[String], Unit](okay2.Writer.tell("x")).free""").nonEmpty,
      "an open move unlifted")
    assert(compileErrors("""
      okay2.Prog.transition[okay2.TestProg.A, okay2.TestProg.B, okay2.Writer[String], Unit](okay2.Writer.tell("x"))
        .flatMap(_ => okay2.Prog.transition[okay2.TestProg.A, okay2.TestProg.B, okay2.Writer[String], Unit](okay2.Writer.tell("y")))""").nonEmpty,
      "a step starting at A was joined to one ending at B")
  }
}

object TestProg {
  sealed trait A
  sealed trait B

  sealed trait Idle
  sealed trait Open

  /** a protocol typed by its own module: the transitions are claimed
   * HERE, in the smart constructors, and nowhere else */
  object Tx {
    def begin: Prog[Writer[String], Unit, Idle, Open] = Prog.transition[Idle, Open, Writer[String], Unit](Writer.tell("begin"))
    def write(s: String): Prog[Writer[String], Unit, Open, Open] = Prog.diag[Open, Writer[String], Unit](Writer.tell(s))
    def commit: Prog[Writer[String], Unit, Open, Idle] = Prog.transition[Open, Idle, Writer[String], Unit](Writer.tell("commit"))
  }
}
