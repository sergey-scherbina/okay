package okay

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

/**
 * What COLLECTING costs on the happy path (specs/validated.md).
 *
 * The prediction, written before measuring: an all-valid `traverse`
 * over 1 000 leaves allocates within 10% of the same traverse at
 * `Either`, because the happy path builds the same number of nodes —
 * one wrapper per leaf either way, and the accumulation never runs.
 * If it does not, the encoding is wrong.
 *
 * The pair is matched: the same `traverse`, the same leaves, the same
 * answers, differing only in the carrier's instance.
 */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class ValidatedBenchmark {

  private val N = 1000
  private val xs: Seq[Int] = (1 to N).toVector

  private given Semigroup[Vector[String]] with
    def combine(x: Vector[String], y: Vector[String]): Vector[String] = x ++ y

  private given Monad[[A] =>> Either[Vector[String], A]] with
    def pure[A](a: A): Either[Vector[String], A] = Right(a)
    extension [A](e: Either[Vector[String], A])
      def flatMap[B](f: A => Either[Vector[String], B]): Either[Vector[String], B] = e.flatMap(f)

  @Benchmark
  def validatedAllValid(): Int =
    traverse(xs)(i => Validated.Valid(i): Validated[Vector[String], Int]) match
      case Validated.Valid(v) => v.length
      case Validated.Invalid(_) => -1

  @Benchmark
  def eitherAllRight(): Int =
    traverse(xs)(i => Right(i): Either[Vector[String], Int]) match
      case Right(v) => v.length
      case Left(_) => -1

  /** and the case the type exists for, priced once so nobody guesses:
   * every leaf invalid, so every `app` combines */
  @Benchmark
  def validatedAllInvalid(): Int =
    traverse(xs)(i => Validated.Invalid(Vector(s"$i")): Validated[Vector[String], Int]) match
      case Validated.Invalid(e) => e.length
      case Validated.Valid(_) => -1
}
