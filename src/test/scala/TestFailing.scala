package okay

/**
 * The row-typeclass recipe's BOUNDARY, enforced (row-typeclass-recipe):
 * which shapes of a row resolve to the guarding instance, and which
 * fall to the low-priority identity — silently, which is the hazard
 * the recipe has to name. `guarded` answers the question behaviourally
 * rather than by types: it hands the instance a `Run` that throws and
 * reports whether the hook ran.
 */
class TestFailing extends munit.FunSuite {

  /** did the instance for this row actually guard an Async.Run? */
  def guarded[F[+_]](e: F[Int])(using f: Failing[F]): Boolean =
    var hooked = false
    val g = f.guard(e, () => hooked = true)
    try g match
      case r: Async.Run[?] => r.run(): Unit
      case _ => ()
    catch case _: RuntimeException => ()
    hooked

  val boom: Async[Int] = Async.Run(() => throw RuntimeException("boom"))

  type S = Throws % String
  type P = Produce

  test("Async alone, and Async on either side of one +, are guarded") {
    assert(guarded[Async](boom))
    assert(guarded[Async + S](boom))
    assert(guarded[S + Async](boom))
  }

  test("deeper rows, which NO anchored instance matches, are guarded by the total fallback") {
    // `A + B + C` nests to the LEFT, so `(Async + S) + P` is not
    // `Async + ?G` to the implicit search and no typed instance has
    // its shape. Before the fallback existed these three silently
    // took an identity instance and the finalizers were abandoned —
    // the very defect the hook exists to prevent, measured here.
    assert(guarded[Async + S + P](boom), "(Async + S) + P")
    assert(guarded[S + P + Async](boom), "(S + P) + Async")
    assert(guarded[S + Async + P](boom), "(S + Async) + P")
  }

  test("a right-nested row is guarded too: the fallback reads the operation, not the shape") {
    type Nested = [A] =>> S[A] | (Async + P)[A]
    assert(guarded[Nested](boom))
  }

  test("an Async-free row is returned untouched — the hook has nothing to attach to") {
    assert(!guarded[S](Throws("no")))
    assert(!guarded[S + P](Throws("no")))
  }
}
