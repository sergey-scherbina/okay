package okay

import okay.RowLift.{at, plus}

/** Two states in one row, told apart by their keys. */
class TestKeyed extends munit.FunSuite {

  type Count = Keyed.At["count", Int]
  type Name  = Keyed.At["name", String]
  type Both  = Count + Name

  test("one keyed state behaves as State does") {
    val p: Int ! Count =
      for
        n <- Keyed.get["count", Int]
        _ <- Keyed.put["count", Int](n + 1)
        m <- Keyed.get["count", Int]
      yield m
    assertEquals(Keyed.run["count", Int, Int](7)(p), (8, 8))
  }

  test("two keyed states share one row, and each reaches its own cell") {
    val p: (Int, String) ! Both =
      for
        n  <- Keyed.get["count", Int].plus[Name]
        _  <- Keyed.put["count", Int](n + 1).plus[Name]
        s  <- Keyed.get["name", String].at[Both]
        _  <- Keyed.put["name", String](s + "!").at[Both]
        n2 <- Keyed.get["count", Int].plus[Name]
        s2 <- Keyed.get["name", String].at[Both]
      yield (n2, s2)

    val inner: (String, (Int, String)) ! Count =
      Keyed.handle["name", String, (Int, String), Count](
        "ada")(p.at[Name + Count])
    val (count, (name, answer)) =
      !.run(Keyed.handle["count", Int, (String, (Int, String)), okay.Pure](7)(inner))
    assertEquals(answer, (8, "ada!"))
    assertEquals(count, 8)
    assertEquals(name, "ada!")
  }

  test("the same VALUE type under two keys stays separate") {
    type A = Keyed.At["a", Int]
    type B = Keyed.At["b", Int]
    val p: (Int, Int) ! (A + B) =
      for
        _ <- Keyed.put["a", Int](1).plus[B]
        _ <- Keyed.put["b", Int](2).at[A + B]
        x <- Keyed.get["a", Int].plus[B]
        y <- Keyed.get["b", Int].at[A + B]
      yield (x, y)
    val inner = Keyed.handle["b", Int, (Int, Int), A](0)(p.at[B + A])
    val (a, (b, answer)) =
      !.run(Keyed.handle["a", Int, (Int, (Int, Int)), okay.Pure](0)(inner))
    assertEquals(answer, (1, 2))
    assertEquals(a, 1)
    assertEquals(b, 2)
  }
}
