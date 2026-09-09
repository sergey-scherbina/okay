package okay

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit
import okay.given

/**
 * optics-core: THE GATE (specs/optics.md stage 0). An optic's `set`
 * pays for a tuple per `first` and a closure per composition; the bar
 * the operator accepted is 1.5x over the hand-written form. Every lane
 * has its control so the price reads as a RATIO:
 *
 *   lensSet        against copySet       one field, `Lens[S](_.f)` vs `copy`
 *   handLensSet    against copySet       `Lens(get, set)` with a hand copy — the profunctor's own price
 *   composedSet    against nestedCopy    lens ∘ prism ∘ lens vs copy inside copy inside Some
 *   fieldSet       against copySet       `Lens.field[S]("f")` — the Mirror route, with its array
 *   traversalOver  against vectorMap     `Traversal.each.modify` vs `Vector.map`
 *   traversalFold  against vectorSum     `foldMap` vs `foldLeft`
 *
 * Read per-lane MINIMA across forks (bench-one-round-lies), not the mean.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(3)
class OpticsBenchmark {

  final case class Address(city: String, zip: Int)
  final case class Person(name: String, age: Int, address: Option[Address])

  private val age: Lens[Person, Person, Int, Int] = Lens[Person](_.age)
  private val ageByName = Lens.field[Person]("age")
  private val ageByHand: Lens[Person, Person, Int, Int] = Lens(_.age, (p, a) => p.copy(age = a))
  private val address: Lens[Person, Person, Option[Address], Option[Address]] = Lens[Person](_.address)
  private val zip: Lens[Address, Address, Int, Int] = Lens[Address](_.zip)
  private val personZip: Affine[Person, Person, Int, Int] = address.andThen(Prism.some).andThen(zip)
  private val each = Traversal.each[Int, Int]

  private val p = Person("ada", 36, Some(Address("Warszawa", 1)))
  private val vec: Vector[Int] = Vector.tabulate(1000)(identity)
  private var n = 0

  given Monoid[Int] with
    def empty = 0
    def combine(x: Int, y: Int) = x + y

  @Benchmark def copySet: Person = { n += 1; p.copy(age = n) }
  @Benchmark def lensSet: Person = { n += 1; age.set(n)(p) }
  @Benchmark def fieldSet: Person = { n += 1; ageByName.set(n)(p) }
  @Benchmark def handLensSet: Person = { n += 1; ageByHand.set(n)(p) }

  @Benchmark def nestedCopy: Person = { n += 1; p.copy(address = p.address.map(_.copy(zip = n))) }
  @Benchmark def composedSet: Person = { n += 1; personZip.set(n)(p) }

  @Benchmark def vectorMap: Vector[Int] = vec.map(_ + 1)
  @Benchmark def traversalOver: Vector[Int] = each.modify(_ + 1)(vec)

  @Benchmark def vectorSum: Int = vec.foldLeft(0)(_ + _)
  @Benchmark def traversalFold: Int = each.foldMap(identity)(vec)
}
