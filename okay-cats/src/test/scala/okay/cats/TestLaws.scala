package okay.cats

import _root_.cats.Eq
import _root_.cats.laws.discipline.{MonadErrorTests, MonadTests}
import okay.{!, %, +, Pure, Throws, effect, pure, runEither}
import okay.given
import org.scalacheck.{Arbitrary, Gen}

/**
 * The ecosystem's own law suites over our instances (specs/interop.md):
 * programs are compared by RUNNING them — the only observation a
 * program-as-value offers — and the generators produce left-nested
 * binds on purpose, so the laws exercise the Bind rotation too.
 * (The RuleSets unfold into plain munit-scalacheck properties —
 * discipline-munit 2.0.0 is inline-incompatible with munit 1.1.)
 */
class TestLaws extends munit.ScalaCheckSuite {

  private def checkAll(name: String, rules: org.typelevel.discipline.Laws#RuleSet): Unit =
    for (id, prop) <- rules.all.properties do property(s"$name: $id")(prop)

  // ---- Monad over the pure row

  type P[A] = A ! Pure

  given [A](using Eq[A]): Eq[P[A]] = Eq.by(p => !.run(p))

  given [A](using a: Arbitrary[A]): Arbitrary[P[A]] = Arbitrary(Gen.oneOf(
    a.arbitrary.map(x => (pure(x): P[A])),
    for x <- a.arbitrary; y <- a.arbitrary
    yield (pure(y): P[A]).flatMap(_ => pure(x)).flatMap(v => pure(v)),
  ))

  checkAll("Monad[A ! Pure]", MonadTests[P].monad[Int, Int, String])

  // ---- MonadError over a row with Throws

  type PE[A] = A ! (Throws % String + Pure)

  given [A](using Eq[A]): Eq[PE[A]] =
    Eq.by(p => !.run(runEither[A, Pure, String](p)))

  given [A](using a: Arbitrary[A]): Arbitrary[PE[A]] = Arbitrary(Gen.oneOf(
    a.arbitrary.map(x => (pure(x): PE[A])),
    Gen.alphaStr.map(e => (effect(Throws(e)): PE[A])),
    for x <- a.arbitrary; y <- a.arbitrary
    yield (pure(y): PE[A]).flatMap(_ => pure(x)),
  ))

  checkAll("MonadError[A ! (Throws % String + Pure)]",
    MonadErrorTests[PE, String].monadError[Int, Int, String])
}
