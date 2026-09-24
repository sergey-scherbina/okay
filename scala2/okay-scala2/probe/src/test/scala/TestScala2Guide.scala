package scala2probe

import okay.scala2._

import scala.collection.mutable.ListBuffer

/** Every snippet of docs/scala2.md, verbatim, compiled by scalac 2.13
 * under -Xlint -Werror. A snippet in the guide that is not here is not
 * checked by anything. */
class TestScala2Guide extends munit.FunSuite {

  // ---- §2 A first program

  test("§2 a first program") {
    val greeting: Prog[String] = for {
      name <- Prog.delay(sys.props.getOrElse("user.name", "world"))
      n <- Prog.pure(name.length)
    } yield "hello " + name + " (" + n + ")"

    assert(greeting.run().startsWith("hello "))
  }

  test("§2 failures") {
    def parse(s: String): Prog[Int] = Prog.delay(s.trim.toInt)

    assertEquals(parse(" 42 ").run(), 42)
    assert(parse("x").runEither().isLeft)
    assertEquals(parse("x").recover(_ => Prog.pure(0)).run(), 0)
  }

  // ---- §3 Several effects in one program

  test("§3 a row of effects") {
    def count(word: String): Unit ! State[Map[String, Int]] =
      State.modify[Map[String, Int]](m => m.updated(word, m.getOrElse(word, 0) + 1))

    def countAll(text: String): Int ! (State[Map[String, Int]] + Writer[String]) = {
      val words = text.split("\\s+").toList.filter(_.nonEmpty)
      words.foldLeft(Eff.pure(0): Int ! (State[Map[String, Int]] + Writer[String])) { (acc, w) =>
        for {
          n <- acc
          _ <- count(w)
          _ <- Writer.tell("saw " + w)
        } yield n + 1
      }
    }

    val (log, (counts, total)) = Eff.run(Writer.run(State.run(Map.empty[String, Int])(countAll("a b a"))))
    assertEquals(total, 3)
    assertEquals(counts, Map("a" -> 2, "b" -> 1))
    assertEquals(log, Vector("saw a", "saw b", "saw a"))
  }

  test("§3 reader and throws") {
    final case class Config(limit: Int)

    def withdraw(amount: Int): Int ! (Reader[Config] + State[Int] + Throws[String]) = for {
      cfg <- Reader.ask[Config]
      balance <- State.get[Int]
      _ <- if (amount > cfg.limit) Throws.raise[String, Unit]("over the limit")
           else if (amount > balance) Throws.raise[String, Unit]("insufficient funds")
           else State.put(balance - amount)
      left <- State.get[Int]
    } yield left

    def attempt(amount: Int): (Int, Either[String, Int]) =
      Eff.run(State.run(100)(Throws.run(Reader.run(Config(limit = 50))(withdraw(amount)))))

    assertEquals(attempt(30), (70, Right(70)))
    assertEquals(attempt(80), (100, Left("over the limit")))
  }

  test("§3 the handler order decides what a failure keeps") {
    val p: Int ! (State[Int] + Throws[String]) =
      State.put(7).flatMap(_ => Throws.raise[String, Int]("no"))
    assertEquals(Eff.run(State.run(0)(Throws.run(p))), (7, Left("no")))
    assertEquals(Eff.run(Throws.run(State.run(0)(p))), Left("no"))
  }

  // ---- §4 Failure: the claims the prose makes, checked

  test("§4 Async.delay lets an exception escape; Async.attempt and Prog.delay make it a value") {
    val boom = new IllegalStateException("boom")
    assert(intercept[IllegalStateException](Eff.runAsync(Async.delay[Int](throw boom))) eq boom)
    assertEquals(Eff.runAsync(Throws.run(Async.attempt[Int](throw boom))), Left(boom))
    assertEquals(Prog.delay[Int](throw boom).runEither(), Left(boom))
    assert(intercept[IllegalStateException](Prog.pure(1).map[Int](_ => throw boom).run()) eq boom)
  }

  // ---- §10 the unhandled-effect message, as the guide quotes it

  val stillWriting: Int ! (State[Int] + Writer[String]) = State.get[Int]

  test("§10 the message for an unhandled effect") {
    val errors = compileErrors("Eff.run(State.run(1)(stillWriting))")
    assert(errors.contains("required: okay.scala2.Eff[okay.scala2.State[Int] with Any,?]"), errors)
  }

  // ---- §5 Your own effect

  test("§5 a key-value store as an effect") {
    sealed trait KV[A] extends Op[A]
    final case class Get(key: String) extends KV[Option[String]]
    final case class Put(key: String, value: String) extends KV[Unit]
    object KV extends Effect[KV]

    def inMemory[R, B](store: scala.collection.mutable.Map[String, String]): Handler[KV, R, B] =
      new Handler[KV, R, B] {
        def apply[X](op: KV[X], k: X => B ! R): B ! R = op match {
          case Get(key) => k(store.get(key))
          case Put(key, value) => store(key) = value; k(())
        }
      }

    val program: Option[String] ! Effect[KV] = for {
      _ <- KV.send(Put("lang", "scala"))
      v <- KV.send(Get("lang"))
    } yield v.map(_.toUpperCase)

    val store = scala.collection.mutable.Map.empty[String, String]
    assertEquals(KV.run(program)(a => Eff.pure(a))(inMemory(store)), Some("SCALA"))
    assertEquals(store.toMap, Map("lang" -> "scala"))
  }

  test("§5 the same program, a different handler: a log instead of a store") {
    sealed trait KV[A] extends Op[A]
    final case class Get(key: String) extends KV[Option[String]]
    final case class Put(key: String, value: String) extends KV[Unit]
    object KV extends Effect[KV]

    val program: Option[String] ! Effect[KV] = for {
      _ <- KV.send(Put("lang", "scala"))
      v <- KV.send(Get("lang"))
    } yield v

    val log = ListBuffer.empty[String]
    def dryRun[R, B]: Handler[KV, R, B] = new Handler[KV, R, B] {
      def apply[X](op: KV[X], k: X => B ! R): B ! R = op match {
        case Get(key) => log += ("get " + key); k(None)
        case Put(key, value) => log += ("put " + key + "=" + value); k(())
      }
    }
    assertEquals(KV.run(program)(a => Eff.pure(a))(dryRun), None)
    assertEquals(log.toList, List("put lang=scala", "get lang"))
  }

  // ---- §6 Continuations

  test("§6 shift and reset") {
    // the continuation k is "the rest of the block": here, _ * 2 then + 1
    val twice: Int = Cont.reset(
      Cont.shift[Int, Int, Int](k => k(k(3))).map(_ * 2).map(_ + 1)
    )
    assertEquals(twice, 15)
  }

  // ---- §7 Streams

  test("§7 a stream") {
    val words: Source[String] = Source("the", "quick", "brown", "fox", "jumps")
    val lengths: Vector[Int] ! Async = words.filter(_.length > 3).map(_.length).runCollect
    assertEquals(Eff.runAsync(lengths), Vector(5, 5, 5))

    val total: Int ! Async = words.zipWithIndex.take(3).runFold(0) { case (acc, (w, _)) => acc + w.length }
    assertEquals(Eff.runAsync(total), 13)
  }

  // ---- §8 Fibers and channels

  test("§8 a worker pool over a channel") {
    val jobs = Channel[Int](8)
    val results = Channel[Int](8)

    def worker: Unit ! Async = jobs.receive.flatMap {
      case Some(n) => results.send(n * n).flatMap(_ => worker)
      case None => Eff.pure(())
    }

    def feed(ns: List[Int]): Unit ! Async = ns match {
      case n :: rest => jobs.send(n).flatMap(_ => feed(rest))
      case Nil => Async.delay(jobs.close())
    }

    val program: Int ! Async = for {
      w1 <- Async.fork(worker)
      w2 <- Async.fork(worker)
      _ <- Async.fork(feed((1 to 10).toList))
      _ <- Async.fork(w1.join.flatMap(_ => w2.join).flatMap(_ => Async.delay(results.close())))
      sum <- results.source.runFold(0)(_ + _)
    } yield sum

    assertEquals(Eff.runAsync(program), 385)
  }
}
