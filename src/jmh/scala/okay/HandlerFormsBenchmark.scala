package okay

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit

/**
 * specs/handler-forms.md: each author's form against the built-in handler it re-expresses, on N operations.
 * `reader`: N asks (Reader(r) vs Handler.answer). `state`: N get-and-set pairs (State(s) vs Handler.state).
 * `maybe`: N operations that resume (Maybe.option vs Handler.control).
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class HandlerFormsBenchmark {

  final val N = 1000

  def asks(n: Int): Int ! Reader % Int =
    if n == 0 then pure(0) else Reader.ask[Int].flatMap(r => !.tailcall(asks(n - 1)).map(_ + r))

  def steps(n: Int): Int ! State % Int =
    if n == 0 then State.get[Int] else State.get[Int].flatMap(s => State.set(s + 1).flatMap(_ => !.tailcall(steps(n - 1))))

  def somes(n: Int): Int ! Maybe =
    if n == 0 then pure(0) else effect[Maybe, Int](Maybe(Some(1))).flatMap(x => !.tailcall(somes(n - 1)).map(_ + x))

  val readerForm = Handler.answer[Reader % Int] { [X] => (e: Reader[Int, X]) => e match
    case Reader.Ask() => 1
    case Reader.Asks(g) => g(1)
  }

  val stateForm = Handler.state[State % Int, Int](0) { [X] => (s: Int, e: State[Int, X]) => e match
    case State.Get() => (s, s)
    case State.Set(n) => (n, n)
    case State.Modify(g) => { val n = g(s); (n, n) }
    case State.Update(g) => { val (b, n) = g(s); (n, b) }
  }

  val maybeForm = Handler.control[Maybe, Option]([A] => (a: A) => Some(a)):
    [X, A, G[+_]] => (e: Maybe[X], resume: X => Option[A] ! G) => e.value match
      case Some(x) => resume(x)
      case None => pure[G, Option[A]](None)

  @Benchmark def reader_builtin(): Int = asks(N).handle(Reader(1)).run
  @Benchmark def reader_answer(): Int = asks(N).handle(readerForm).run
  @Benchmark def state_builtin(): (Int, Int) = steps(N).handle(State(0)).run
  @Benchmark def state_form(): (Int, Int) = steps(N).handle(stateForm).run
  @Benchmark def maybe_builtin(): Option[Int] = somes(N).handle(Maybe.option).run
  @Benchmark def maybe_control(): Option[Int] = somes(N).handle(maybeForm).run
}
