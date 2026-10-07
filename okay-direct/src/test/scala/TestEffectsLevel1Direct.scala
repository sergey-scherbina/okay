package okay


import okay.freer.*


import okay.std.*
import okay.freer.given
import okay.Direct.*

/** level 1 through the typeclass in direct style: one program over `Classic[M]`, run in Free and in Eager */
class TestEffectsLevel1Direct extends munit.FunSuite:

  type Row = Shift % Int + State % Int

  def program[M[_[+_], _]](using E: Classic[M]): M[State % Int, Int] =
    given Monad[[A] =>> M[Row, A]] = Classic.monad[M, Row]
    E.reset[Int, State % Int](direct[[A] =>> M[Row, A]] {
      val x = E.shift[Int, Int, State % Int](k => direct[[A] =>> M[Row, A]] { k(1).? + k(10).? }).?
      x * 2 + E.perform[Row, Int](State.Get[Int, Int]()).?
    })

  test("Free and Eager: the same direct program, the same answer") {
    val F = summon[Classic[Free]]
    val G = summon[Classic[Eager]]
    assertEquals(F.run(F.handle(program[Free], State(5))), (5, 32))
    assertEquals(G.run(G.handle(program[Eager], State(5))), (5, 32))
  }
