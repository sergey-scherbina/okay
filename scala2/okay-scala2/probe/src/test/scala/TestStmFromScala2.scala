package scala2probe

import okay.TRef
import okay.scala2._

/** okay-stm from Scala 2.13 (specs/scala2-facade.md, stage 15.3) */
class TestStmFromScala2 extends munit.FunSuite {

  def transfer(from: TRef[Int], to: TRef[Int], amount: Int): Unit ! Tx = for {
    balance <- Tx.read(from)
    _ <- Tx.check(balance >= amount)
    _ <- Tx.write(from, balance - amount)
    _ <- Tx.update(to)(_ + amount)
  } yield ()

  test("a transfer commits both writes together") {
    val a = Stm.ref(100)
    val b = Stm.ref(0)
    Stm.atomically(transfer(a, b, 30)).runWith
    assertEquals((a.get, b.get), (70, 30))
  }

  test("a thousand increments from eight fibers lose nothing") {
    val counter = Stm.ref(0)
    val one = Stm.atomically(Tx.update(counter)(_ + 1))
    def many(n: Int): Unit ! Async = if (n == 0) pure(()) else one.flatMap(_ => many(n - 1))
    def all[A](es: List[A ! Async]): List[A] ! Async =
      es.foldRight(pure(List.empty[A]): List[A] ! Async)((e, rest) => e.flatMap(a => rest.map(a :: _)))
    val prog = for {
      fibers <- all(List.fill(8)(Async.fork(many(125))))
      _ <- all(fibers.map(_.join))
    } yield counter.get
    assertEquals(prog.runWith, 1000)
  }

  test("retry waits until another fiber makes the condition true") {
    val account = Stm.ref(0)
    val out = Stm.ref(0)
    val prog = for {
      waiting <- Async.fork(Stm.atomically(transfer(account, out, 50)))
      _ <- Async.sleep(20)
      _ <- Stm.atomically(Tx.write(account, 80))
      _ <- waiting.join
    } yield (account.get, out.get)
    assertEquals(prog.runWith, (30, 50))
  }

  test("orElse takes the second branch when the first retries, and drops the first's writes") {
    val empty = Stm.ref(0)
    val full = Stm.ref(10)
    val touched = Stm.ref("untouched")
    val take = (r: TRef[Int]) => for {
      n <- Tx.read(r)
      _ <- Tx.write(touched, "first")
      _ <- Tx.check(n > 0)
      _ <- Tx.write(r, n - 1)
    } yield n
    val which = Stm.atomically(Tx.orElse(take(empty), Tx.read(full).map(_ * 100))).runWith
    assertEquals((which, touched.get, full.get), (1000, "untouched", 10))
  }

  test("I/O inside a transaction does not compile") {
    assert(compileErrors("Stm.atomically(Async(println(1)))").contains("type mismatch"))
  }
}
