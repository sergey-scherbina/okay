package okay.std.bench

import okay.{!, %, Effect, Handler, Pure, effect, pure, tailcall}
import okay.std.*

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit

/**
 * specs/handler-forms.md: each author's form against the built-in handler it re-expresses, on N operations.
 * `reader`: N asks (Reader(r) vs Handler.answering). `state`: N get-and-set pairs (State(s) vs Handler.state).
 * `maybe`: N operations that resume (Maybe.option vs Handler.deep).
 *
 * On the new road the root has ONE form per kind, a polymorphic function (`Handler.answering`, `Handler.state`,
 * `Handler.deep`): the classic's `.poly` lanes map to it directly. The classic's `{ case … }` lanes (`tick_cases`,
 * `count_cases`) are NEAREST EQUIVALENTS: the case form as the classic's macro expands it — the cases a function
 * of the operation at one type, its answer cast to the operation's — over the same root form.
 */
/** an effect whose every operation has a fixed answer, for the `{ case … }` form against `.poly` */
enum FormTick[+A] derives Effect:
  case Next() extends FormTick[Int]
  case Peek() extends FormTick[Int]

@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class HandlerFormsBenchmark {

  final val N = 1000

  def asks(n: Int): Int ! Reader % Int =
    if n == 0 then pure(0) else Reader.ask[Int].flatMap(r => tailcall(asks(n - 1)).map(_ + r))

  def steps(n: Int): Int ! State % Int =
    if n == 0 then State.get[Int] else State.get[Int].flatMap(s => State.set(s + 1).flatMap(_ => tailcall(steps(n - 1))))

  def somes(n: Int): Int ! Maybe =
    if n == 0 then pure(0) else effect[Maybe, Int](Maybe(Some(1))).flatMap(x => tailcall(somes(n - 1)).map(_ + x))

  val readerForm = Handler.answering[Reader % Int]([X] => (e: Reader[Int, X]) => e match
    case Reader.Ask() => 1
    case Reader.Asks(g) => g(1)
  )

  val stateForm = Handler.state[State % Int, Int](0)([X] => (s: Int, e: State[Int, X]) => e match
    case State.Get() => (s, s)
    case State.Update(g) => { val (b, n) = g(s); (n, b) }
  )

  val maybeForm = Handler.deep[Maybe, Option]([A] => (a: A) => Some(a))(
    [A, X] => (e: Maybe[X], resume: X => Option[A] ! Pure) => e.value match
      case Some(x) => resume(x)
      case None => pure[Option[A], Pure](None))

  def ticks(n: Int): Int ! FormTick =
    if n == 0 then pure(0) else effect[FormTick, Int](FormTick.Next()).flatMap(x => tailcall(ticks(n - 1)).map(_ + x))

  // the case form as the classic's macro expands it: the cases at one type, the answer cast to the operation's
  private val tickCaseFn: FormTick[Any] => Any = {
    case FormTick.Next() => 1
    case FormTick.Peek() => 0
  }
  private val countCaseFn: (Int, FormTick[Any]) => Any = {
    case (n, FormTick.Next()) => (n + 1, n)
    case (n, FormTick.Peek()) => (n, n)
  }

  val tickCases = Handler.answering[FormTick]([X] => (e: FormTick[X]) => tickCaseFn(e).asInstanceOf[X])
  val tickPoly = Handler.answering[FormTick]([X] => (e: FormTick[X]) => e match
    case FormTick.Next() => 1
    case FormTick.Peek() => 0
  )
  val countCases = Handler.state[FormTick, Int](0)([X] => (n: Int, e: FormTick[X]) => countCaseFn(n, e).asInstanceOf[(Int, X)])
  val countPoly = Handler.state[FormTick, Int](0)([X] => (n: Int, e: FormTick[X]) => e match
    case FormTick.Next() => (n + 1, n)
    case FormTick.Peek() => (n, n)
  )

  @Benchmark def tick_cases(): Int = ticks(N).handle(tickCases).run
  @Benchmark def tick_poly(): Int = ticks(N).handle(tickPoly).run
  @Benchmark def count_cases(): (Int, Int) = ticks(N).handle(countCases).run
  @Benchmark def count_poly(): (Int, Int) = ticks(N).handle(countPoly).run
  @Benchmark def reader_builtin(): Int = asks(N).handle(Reader(1)).run
  @Benchmark def reader_answer(): Int = asks(N).handle(readerForm).run
  @Benchmark def state_builtin(): (Int, Int) = steps(N).handle(State(0)).run
  @Benchmark def state_form(): (Int, Int) = steps(N).handle(stateForm).run
  @Benchmark def maybe_builtin(): Option[Int] = somes(N).handle(Maybe.option).run
  @Benchmark def maybe_control(): Option[Int] = somes(N).handle(maybeForm).run
}
