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
 *   compiledComposedSet against composedSet and nestedCopy — the lane
 *     optics-fast exists for: the chain paid once, then called
 *   compiledLensSet     against lensSet — where there is no chain to
 *     pay, so the expected answer is "no difference"
 *   fusedComposedSet    against nestedCopy — optics-fuse: the update
 *     emitted by the compiler, which should BE the hand-written one
 *   fusedLensSet        against copySet — the same, one field deep
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
  import OpticsBenchmark.{Address, Person}

  private val age: Lens[Person, Person, Int, Int] = Lens[Person](_.age)
  private val ageByName = Lens.field[Person]("age")
  private val ageByHand: Lens[Person, Person, Int, Int] = Lens(_.age, (p, a) => p.copy(age = a))
  private val address: Lens[Person, Person, Option[Address], Option[Address]] = Lens[Person](_.address)
  private val zip: Lens[Address, Address, Int, Int] = Lens[Address](_.zip)
  private val personZip: Affine[Person, Person, Int, Int] = address.andThen(Prism.some).andThen(zip)
  private val each = Traversal.each[Int, Int]
  // optics-fast: the optic run ONCE at its concrete representation.
  // Built here, in a field — building it per call would measure the
  // build, which is the thing being avoided.
  // optics-fuse: the optic as an INLINE DEF, so the macro sees the
  // expression rather than a reference and can emit the update itself
  // the halves written out: `Lens[S](_.f)` is itself a macro, and a
  // macro cannot expand another macro's captured call (optics-fuse),
  // so a fusable optic names its own get and put
  private inline def iAge = Lens[Person, Person, Int, Int](_.age, (s, v) => s.copy(age = v))
  private inline def iName = Lens[Person, Person, String, String](_.name, (s, v) => s.copy(name = v))
  // optics-zero-tax: the SELECTOR-built lens, which is what code
  // actually writes, and which the fusion could not read until now
  private inline def sAge = Lens[Person](_.age)
  private inline def sAddress = Lens[Person](_.address)
  private inline def sZip = Lens[Address](_.zip)
  private inline def sPersonZip = sAddress.andThen(Prism.some[Address, Address]).andThen(sZip)
  private inline def iAddress =
    Lens[Person, Person, Option[Address], Option[Address]](_.address, (s, v) => s.copy(address = v))
  private inline def iZip = Lens[Address, Address, Int, Int](_.zip, (s, v) => s.copy(zip = v))
  private inline def iPersonZip = iAddress.andThen(Prism.some[Address, Address]).andThen(iZip)

  private val cAge = age.compiled
  private val cAgeLens = age.compiledLens
  private val cPersonZip = personZip.compiled

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
  @Benchmark def compiledComposedSet: Person = { n += 1; cPersonZip.set(n)(p) }
  @Benchmark def fusedComposedSet: Person = { n += 1; Fuse.set(iPersonZip)(n)(p) }
  @Benchmark def fusedLensSet: Person = { n += 1; Fuse.set(iAge)(n)(p) }
  @Benchmark def compiledLensSet: Person = { n += 1; cAge.set(n)(p) }
  @Benchmark def compiledShopSet: Person = { n += 1; cAgeLens.set(n)(p) }

  // optics-zero-tax: is the idiomatic lens free now?
  @Benchmark def fusedSelectorSet: Person = { n += 1; Fuse.set(sAge)(n)(p) }
  @Benchmark def fusedSelectorComposed: Person = { n += 1; Fuse.set(sPersonZip)(n)(p) }

  // optics-fuse-reads: the read side. `ageByName` is `Lens.field`,
  // which the planner still cannot read (a block with a statement in
  // it), so it prices the interpretation in the same run.
  @Benchmark def directAgeGet: Int = p.age
  @Benchmark def lensGet: Int = age.get(p)
  @Benchmark def fieldGet: Int = ageByName.get(p)
  /** the SAME selector lens read through the interpretation, which is
   * what `get` was before this lane — the matched pair for `lensGet`.
   * `fieldGet` is not one: it differs in the getter too (a Mirror's
   * productElement against `_.age`), which is two changes in one row. */
  @Benchmark def lensGetInterpreted: Int =
    age.apply[[X, Y] =>> Optic.Forget[Int, X, Y]](Optic.Forget(identity)).run(p)

  @Benchmark def vectorMap: Vector[Int] = vec.map(_ + 1)
  @Benchmark def traversalOver: Vector[Int] = each.modify(_ + 1)(vec)

  @Benchmark def vectorSum: Int = vec.foldLeft(0)(_ + _)
  @Benchmark def traversalFold: Int = each.foldMap(identity)(vec)

  // ---------------------------------------------------------------- can a compiled optic BEAT hand-written?
  //
  // The operator's question. Fusion already reaches hand-written to
  // the byte for one update (fusedLensSet above); the interesting
  // question is whether the LAWS license work a hand writes and a
  // compiler could delete. Two candidates, and each is measured
  // against the two things a person actually writes.
  //
  // (a) two updates to one product. The naive chain allocates an
  //     intermediate; the careful `copy` with both names does not. If
  //     the JIT's escape analysis already erases the intermediate,
  //     there is nothing here to win on the JVM and the answer is a
  //     number, not an opinion.
  @Benchmark def twoCopiesChained: Person = { n += 1; p.copy(age = n).copy(name = "x") }
  @Benchmark def oneCopyBoth: Person = { n += 1; p.copy(age = n, name = "x") }
  @Benchmark def fusedTwoSets: Person = { n += 1; Fuse.set(iName)("x")(Fuse.set(iAge)(n)(p)) }

  // (b) two passes over a container. The functor law says
  //     map(f) . map(g) == map(f . g), so a rewriter is LICENSED to
  //     delete the intermediate — which no `copy` fusion can claim,
  //     because the law is what makes it sound.
  @Benchmark def mapMapChained: Vector[Int] = vec.map(_ + 1).map(_ * 2)
  @Benchmark def mapFused: Vector[Int] = vec.map(x => (x + 1) * 2)
  @Benchmark def traversalTwice: Vector[Int] = each.modify(_ * 2)(each.modify(_ + 1)(vec))
  @Benchmark def traversalFusedByLaw: Vector[Int] = each.modify(x => (x + 1) * 2)(vec)
}

/** the model lives here, not in the class: JMH generates a subclass, and
 * a case class declared inside the benchmark is path-dependent — an
 * `inline def` expanded in the generated code then cannot name it
 * (found by optics-fuse) */
object OpticsBenchmark:
  final case class Address(city: String, zip: Int)
  final case class Person(name: String, age: Int, address: Option[Address])
