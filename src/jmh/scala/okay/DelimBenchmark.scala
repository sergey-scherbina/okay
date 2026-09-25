package okay

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit
import okay.Delim.{push, reset, shift}

/**
 * The price of universality. `Delim` lets a user define effects in
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

  // ---- the same thing defined in user code, over Delim

  type Row = Delim + Pure

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
    !.run(Delim.run[Int, Pure] {
      def go(i: Int): Int ! Row =
        if i >= N then pure(i)
        else push[Int, Pure](Delim.prompt[Int])(pure(i)).flatMap(_ => go(i + 1))
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
    push[R, Pure](p)(e.flatMap(x => Delim.shift0[R, R, Pure](p)(_ => v(x))))

  @Benchmark
  def delimDollarOnly(): Int =
    !.run(Delim.run[Int, Pure] {
      def go(i: Int): Int ! Row =
        if i >= N then pure(i)
        else Delim.dollar[Int, Int, Pure](Delim.prompt[Int])(x => pure(x + 1))(pure(i)).flatMap(_ => go(i + 1))
      go(0)
    })

  @Benchmark
  def delimMacroOnly(): Int =
    !.run(Delim.run[Int, Pure] {
      def go(i: Int): Int ! Row =
        if i >= N then pure(i)
        else dollarMacro[Int](Delim.prompt[Int])(x => pure(x + 1))(pure(i)).flatMap(_ => go(i + 1))
      go(0)
    })

  @Benchmark
  def delimDollarResume(): Int =
    !.run(Delim.run[Int, Pure] {
      def go(i: Int): Int ! Row =
        if i >= N then pure(i)
        else
          val p = Delim.prompt[Int]
          Delim.dollar[Int, Int, Pure](p)(x => pure(x + 1))(
            Delim.shift0[Int, Int, Pure](p)(k => k(i))).flatMap(_ => go(i + 1))
      go(0)
    })

  @Benchmark
  def delimMacroResume(): Int =
    !.run(Delim.run[Int, Pure] {
      def go(i: Int): Int ! Row =
        if i >= N then pure(i)
        else
          val p = Delim.prompt[Int]
          dollarMacro[Int](p)(x => pure(x + 1))(
            Delim.shift0[Int, Int, Pure](p)(k => k(i))).flatMap(_ => go(i + 1))
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
    val p = Delim.prompt[List[Int]]
    !.run(Delim.run[List[Int], Pure](
      Delim.dollar[Int, List[Int], Pure](p)(r => pure(List(r)))(
        Delim.shift0[List[Int], Int, Pure](p)(k => listBind(layerXs)(k)).map(_ + 1)))).length

  @Benchmark
  def layeredViaPush(): Int =
    val p = Delim.prompt[List[Int]]
    !.run(Delim.run[List[Int], Pure](
      push[List[Int], Pure](p)(
        Delim.shift0[List[Int], Int, Pure](p)(k => listBind(layerXs)(k)).map(_ + 1).map(r => List(r))))).length

  // ---- handlers AS delimited control (specs/shift0-dollar.md stage 3,
  // FSCD 2019): the State handler three ways over the same N get/set
  // pairs. `stateHandle` is the library's loop; `stateDeep` is ret $ body
  // with every operation a shift0; `stateShallow` is control0 with the
  // handler re-installed around k. The construction is TestHandlersAsDollar's,
  // copied, with the residual row Pure.

  type SRow = okay.State % Int
  def stateProg(n: Int): Int ! SRow =
    if n == 0 then okay.State.get[Int]
    else okay.State.get[Int].flatMap(s => okay.State.set(s + 1)).flatMap(_ => stateProg(n - 1))

  /** every State operation through `op`; the program has no other row */
  def rewriteState[A](op: [X] => okay.State[Int, X] => X ! Row)(prog: A ! SRow): A ! Row =
    (prog.resume: @unchecked) match
      case Free.Return(a) => pure(a)
      case Free.Inject(e) => op(e)
      case Free.Bind(Free.Inject(e), k) => op(e).flatMap(x => rewriteState(op)(k(x)))

  @Benchmark
  def stateHandle(): Int =
    okay.State.run(0)(stateProg(N))._2

  @Benchmark
  def stateDeep(): Int =
    type Ans = Int => (Int, Int) ! Row
    val p = Delim.prompt[Ans]
    val op = [X] => (e: okay.State[Int, X]) => (e match
      case okay.State.Get() => Delim.shift0[Ans, Int, Pure](p)(k => pure((s: Int) => k(s).flatMap(f => f(s))))
      case okay.State.Set(s1) => Delim.shift0[Ans, Int, Pure](p)(k => pure((_: Int) => k(s1).flatMap(f => f(s1))))
    ): X ! Row
    val ret: Int => Ans ! Row = a => pure((s: Int) => pure((s, a)))
    !.run(Delim.run[(Int, Int), Pure](Delim.dollar[Int, Ans, Pure](p)(ret)(rewriteState(op)(stateProg(N))).flatMap(f => f(0))))._2

  @Benchmark
  def stateShallow(): Int =
    type Ans = Int => (Int, Int) ! Row
    val p = Delim.prompt[Ans]
    val op = [X] => (e: okay.State[Int, X]) => (e match
      case okay.State.Get() => Delim.control0[Ans, Int, Pure](p)(k => pure((s: Int) => push[Ans, Pure](p)(k(s)).flatMap(f => f(s))))
      case okay.State.Set(s1) => Delim.control0[Ans, Int, Pure](p)(k => pure((_: Int) => push[Ans, Pure](p)(k(s1)).flatMap(f => f(s1))))
    ): X ! Row
    val body = rewriteState(op)(stateProg(N)).map(a => (s: Int) => pure[Row, (Int, Int)]((s, a)))
    !.run(Delim.run[(Int, Int), Pure](push[Ans, Pure](p)(body).flatMap(f => f(0))))._2

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
    type R = Writer % Int + Delim
    def go(i: Int): Unit ! R =
      if i >= N then pure(())
      else effect[R, Unit](Writer(i)).flatMap(_ => go(i + 1))
    !.run(Writer.run[Int, Unit, Pure](
      Delim.run[Unit, Writer % Int](
        push[Unit, Writer % Int](Delim.prompt[Unit])(
          !.widen[Unit, Writer % Int + Delim, Pure](go(0))))))._1.length

  // ---- the floor

  @Benchmark
  def plainList(): Int =
    var xs = List.empty[Int]
    var i = N - 1
    while i >= 0 do { xs = i :: xs; i -= 1 }
    xs.length
}
