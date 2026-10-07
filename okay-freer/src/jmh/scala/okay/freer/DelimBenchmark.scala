package okay.freer

import okay.{guard}
import okay.given

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit
import okay.freer.Shift.{push, reset, shift}
import okay.freer.Row.at

/**
 * The price of universality. `Shift` lets a user define effects in
 * their own code — a generator is a prompt and a shift — and the
 * question this lane answers is what that costs against the effect
 * the library ships for the same job.
 *
 * Three ways to produce N values: the native `Writer` (an opaque
 * identity signature, zero allocation per tell), a generator built
 * from delimited control, and a plain List for the floor.
 */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class DelimBenchmark {

  val N = 1000

  // ---- the native effect

  @Benchmark
  def writerTell(): Int =
    def go(i: Int): Unit ! Writer % Int =
      if i >= N then pure(())
      else Writer.tell(i).flatMap(_ => go(i + 1))
    !.run(Writer.run[Int, Unit, Pure](go(0)))._1.length

  // ---- the same thing defined in user code, over Shift

  type Row = Shift % ? + Pure

  def emit(p: Prompt[List[Int]])(a: Int): Unit ! Row =
    shift[List[Int], Unit, Pure](p)(k => k(()).map(a :: _))

  @Benchmark
  def delimGenerator(): Int =
    !.run(reset[List[Int], Pure] { p =>
      def go(i: Int): Unit ! Row =
        if i >= N then pure(())
        else emit(p)(i).flatMap(_ => go(i + 1))
      go(0).map(_ => Nil)
    }).length

  // ---- what a delimiter costs when nothing is captured

  @Benchmark
  def delimPushOnly(): Int =
    !.run(Shift.run[Int, Pure] {
      def go(i: Int): Int ! Row =
        if i >= N then pure(i)
        else push[Int, Pure](Shift.prompt[Int])(pure(i)).flatMap(_ => go(i + 1))
      go(0)
    })

  // ---- `$` (delim-dollar, specs/shift0-dollar.md stage 1): the
  // primitive against APLAS 2012's macro-expression over push/shift0,
  // which stage 0 showed means the same. "Only" is N delimiters with a
  // return function and nothing captured (read against delimPushOnly);
  // "Resume" adds one shift0 per delimiter that resumes once, so ret
  // runs inside the continuation. The macro pays a capture per `$` even
  // when the body captures nothing, which is the price being asked.

  def dollarMacro[R](p: Prompt[R])(v: R => R ! Row)(e: R ! Row): R ! Row =
    push[R, Pure](p)(e.flatMap(x => Shift.shift0[R, R, Pure](p)(_ => v(x))))

  @Benchmark
  def delimDollarOnly(): Int =
    !.run(Shift.run[Int, Pure] {
      def go(i: Int): Int ! Row =
        if i >= N then pure(i)
        else Shift.dollar[Int, Int, Pure](Shift.prompt[Int])(x => pure(x + 1))(pure(i)).flatMap(_ => go(i + 1))
      go(0)
    })

  @Benchmark
  def delimMacroOnly(): Int =
    !.run(Shift.run[Int, Pure] {
      def go(i: Int): Int ! Row =
        if i >= N then pure(i)
        else dollarMacro[Int](Shift.prompt[Int])(x => pure(x + 1))(pure(i)).flatMap(_ => go(i + 1))
      go(0)
    })

  @Benchmark
  def delimDollarResume(): Int =
    !.run(Shift.run[Int, Pure] {
      def go(i: Int): Int ! Row =
        if i >= N then pure(i)
        else
          val p = Shift.prompt[Int]
          Shift.dollar[Int, Int, Pure](p)(x => pure(x + 1))(
            Shift.shift0[Int, Int, Pure](p)(k => k(i))).flatMap(_ => go(i + 1))
      go(0)
    })

  /** `delimDollarResume`'s step, each in a run of its own: N runs of one capture and one resume. Against
   * `delimDollarResume` (one run, N steps) the difference over N is what a run costs to start
   * (machine-start-cost: `Run`, `Steps`, `Nested`, the prompt) */
  @Benchmark
  def delimRunEach(): Int =
    var acc = 0
    var i = 0
    while i < N do
      val v = i
      acc += !.run(Shift.run[Int, Pure] {
        val p = Shift.prompt[Int]
        Shift.dollar[Int, Int, Pure](p)(x => pure(x + 1))(Shift.shift0[Int, Int, Pure](p)(k => k(v)))
      })
      i += 1
    acc

  @Benchmark
  def delimMacroResume(): Int =
    !.run(Shift.run[Int, Pure] {
      def go(i: Int): Int ! Row =
        if i >= N then pure(i)
        else
          val p = Shift.prompt[Int]
          dollarMacro[Int](p)(x => pure(x + 1))(
            Shift.shift0[Int, Int, Pure](p)(k => k(i))).flatMap(_ => go(i + 1))
      go(0)
    })

  // ---- layered reflection's reify (specs/layered-reflection.md stage 1):
  // one List layer over N elements, reflect = shift0 with a traversing
  // bind, so every element resumes the continuation and re-installs the
  // layer. The two lanes differ ONLY in the delimiter: λ$'s `η $ e`
  // (what Layered.reify is now) against stage 0's `push(e.map(η))`.

  val layerXs: List[Int] = List.range(0, N)

  def listBind(m: List[Int])(k: Int => List[Int] ! Row): List[Int] ! Row =
    m.foldRight(pure(List.empty[Int]): List[Int] ! Row)((a, rest) => k(a).flatMap(bs => rest.map(bs ++ _)))

  @Benchmark
  def layeredViaDollar(): Int =
    val p = Shift.prompt[List[Int]]
    !.run(Shift.run[List[Int], Pure](
      Shift.dollar[Int, List[Int], Pure](p)(r => pure(List(r)))(
        Shift.shift0[List[Int], Int, Pure](p)(k => listBind(layerXs)(k)).map(_ + 1)))).length

  @Benchmark
  def layeredViaPush(): Int =
    val p = Shift.prompt[List[Int]]
    !.run(Shift.run[List[Int], Pure](
      push[List[Int], Pure](p)(
        Shift.shift0[List[Int], Int, Pure](p)(k => listBind(layerXs)(k)).map(_ + 1).map(r => List(r))))).length

  // ---- handlers AS delimited control (specs/shift0-dollar.md stage 3,
  // FSCD 2019): the State handler two ways over the same N get/set
  // pairs. `stateHandle` is the library's loop; `stateDeep` is ret $ body
  // with every operation a shift0 (`stateShallow`, control0, left with
  // control0: cont-core-design). The construction is TestHandlersAsDollar's,
  // copied, with the residual row Pure.

  type SRow = okay.freer.State % Int
  def stateProg(n: Int): Int ! SRow =
    if n == 0 then okay.freer.State.get[Int]
    else okay.freer.State.get[Int].flatMap(s => okay.freer.State.set(s + 1)).flatMap(_ => stateProg(n - 1))

  /** every State operation through `op`; the program has no other row */
  def rewriteState[A](op: [X] => okay.freer.State[Int, X] => X ! Row)(prog: A ! SRow): A ! Row =
    (prog.resume: @unchecked) match
      case Free.Return(a) => pure(a)
      case Free.Inject(e) => op(e)
      case Free.Bind(Free.Inject(e), k) => op(e).flatMap(x => rewriteState(op)(k(x)))

  @Benchmark
  def stateHandle(): Int =
    okay.freer.State.run(0)(stateProg(N))._2

  @Benchmark
  def stateDeep(): Int =
    type Ans = Int => (Int, Int) ! Row
    val p = Shift.prompt[Ans]
    val op = [X] => (e: okay.freer.State[Int, X]) => (e match
      case okay.freer.State.Get() => Shift.shift0[Ans, Int, Pure](p)(k => pure((s: Int) => k(s).flatMap(f => f(s))))
      case okay.freer.State.Update(f1) => Shift.shift0[Ans, X, Pure](p)(k => pure((s: Int) => { val (b, s1) = f1(s); k(b).flatMap(f => f(s1)) }))
    ): X ! Row
    val ret: Int => Ans ! Row = a => pure((s: Int) => pure((s, a)))
    !.run(Shift.run[(Int, Int), Pure](Shift.dollar[Int, Ans, Pure](p)(ret)(rewriteState(op)(stateProg(N))).flatMap(f => f(0))))._2

  // ---- an operation of ANOTHER effect on the machine (handling-ever-per-machine): N State
  // operations performed inside `Shift.run`, under one delimiter, answered by `State.run`
  // outside — each leaves the machine as its head form. Before forwarding one, a machine looks
  // at its boundaries for a handler frame (handle-frames) only once a frame was installed in
  // the run (cont-atm; until then process-wide, `Cont0.Handling.ever`).

  type FW = okay.freer.State % Int
  type FRow = Shift % ? + FW
  def foreignProg(n: Int): Int ! FRow =
    if n == 0 then okay.freer.State.get[Int].at[FRow]
    else okay.freer.State.get[Int].at[FRow].flatMap(s => okay.freer.State.set(s + 1).at[FRow]).flatMap(_ => foreignProg(n - 1))

  def foreignRun(): Int =
    okay.freer.State.run(0)(Shift.run[Int, FW](Shift.push[Int, FW](Shift.prompt[Int])(foreignProg(N))))._2

  @Benchmark
  def stateForeign(): Int = foreignRun()

  // ---- handler INSTANCES (specs/lexical-instances.md): the same N get/set
  // pairs as stateHandle, through one `Lexical` instance, by strategy.
  // `tail` answers in place (evidence passing) with one guard delimiter
  // per installation; `deep` captures per operation.

  def lexSpin[G[+_]](s: Lexical.Inst[okay.freer.State % Int, G], n: Int): Int ! G =
    import Lexical.State.{get, set}
    if n == 0 then s.get else s.get.flatMap(v => s.set(v + 1)).flatMap(_ => lexSpin(s, n - 1))

  /** tail on a row with Shift: the guard, and the machine */
  @Benchmark
  def stateLexTail(): Int =
    !.run(Shift.run[(Int, Int), Pure](Lexical.State.tail[Int, Int, Row](0)(s => lexSpin(s, N))))._2

  /** tail on a row WITHOUT Shift: no guard, no machine (lexical-tail-allocs) */
  @Benchmark
  def stateLexTailPure(): Int =
    !.run(Lexical.State.tail[Int, Int, Pure](0)(s => lexSpin(s, N)))._2

  /** DIAGNOSTIC (lexical-tail-allocs): the unguarded program, but run
   * through the Shift machine, to split runner from guard */
  @Benchmark
  def stateLexTailPureInMachine(): Int =
    import okay.freer.Row.up
    !.run(Shift.run[(Int, Int), Pure](Lexical.State.tail[Int, Int, Pure](0)(s => lexSpin(s, N)).up[Row]))._2

  /** the optional `walk` strategy (lexical-tagged-walk): inert tagged
   * operations, the installation walks its body like a row handler */
  @Benchmark
  def stateLexWalk(): Int =
    !.run(Instances.exhausted[okay.freer.State % Int, (Int, Int), Pure](Lexical.State.walk[Int, Int, Pure](0)(s => lexSpin(s, N))))._2

  @Benchmark
  def stateLexDeep(): Int =
    !.run(Shift.run[(Int, Int), Pure](Lexical.State.deep[Int, Int, Row](0)(s => lexSpin(s, N))))._2

  // ---- ONE push, then ordinary work INSIDE the machine
  //
  // This is the shape a GUARD has (okay-llm's `Cut`, okay-ui's
  // `Scope`): a boundary installed once, and a body that does its
  // ordinary work under it and never captures. `delimPushOnly` above
  // measures N pushes and `delimGenerator` measures N captures;
  // neither measures this, which is the shape those consumers
  // actually run — and `Cut`'s header claims the guard "costs the
  // prompt push, not the capture price". The pair to read it against
  // is `writerTell`: the SAME N tells, outside any machine.

  @Benchmark
  def writerTellUnderDelim(): Int =
    type R = Writer % Int + Shift % ?
    def go(i: Int): Unit ! R =
      if i >= N then pure(())
      else effect[R, Unit](Writer(i)).flatMap(_ => go(i + 1))
    !.run(Writer.run[Int, Unit, Pure](
      Shift.run[Unit, Writer % Int](
        push[Unit, Writer % Int](Shift.prompt[Unit])(
          !.widen[Unit, Writer % Int + Shift % ?, Pure](go(0))))))._1.length

  // ---- the floor

  @Benchmark
  def plainList(): Int =
    var xs = List.empty[Int]
    var i = N - 1
    while i >= 0 do { xs = i :: xs; i -= 1 }
    xs.length
}
