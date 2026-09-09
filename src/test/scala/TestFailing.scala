package okay

/**
 * Every shape of a row is guarded (row-typeclass-recipe,
 * failing-simplify) — `Async` alone through the typed instance, every
 * nesting through the total default. `guarded` answers the question
 * BEHAVIOURALLY rather than by types: it hands the instance a `Run`
 * that throws and reports whether the hook ran. A `summon` that
 * succeeds proves only that an instance was found, which is how the
 * identity default went unnoticed once already.
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

  test("Async alone (the typed instance), and Async on either side of one + (the default), are guarded") {
    assert(guarded[Async](boom))
    assert(guarded[Async + S](boom))
    assert(guarded[S + Async](boom))
  }

  test("deeper rows, which no instance could be written for, are guarded by the total default") {
    // `A + B + C` nests to the LEFT, so no instance can be written for
    // the shape without enumerating nestings. While the default was an
    // identity these three silently took it and the finalizers were
    // abandoned — the very defect the hook exists to prevent, measured
    // here so it cannot come back unseen.
    assert(guarded[Async + S + P](boom), "(Async + S) + P")
    assert(guarded[S + P + Async](boom), "(S + P) + Async")
    assert(guarded[S + Async + P](boom), "(S + Async) + P")
  }

  test("a right-nested row is guarded too: the default reads the operation, not the shape") {
    type Nested = [A] =>> S[A] | (Async + P)[A]
    assert(guarded[Nested](boom))
  }

  test("an Async-free row is returned untouched — the hook has nothing to attach to") {
    assert(!guarded[S](Throws("no")))
    assert(!guarded[S + P](Throws("no")))
  }
}
