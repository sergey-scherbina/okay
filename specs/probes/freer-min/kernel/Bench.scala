package okay.freer

import Freer.*

/** a HYPOTHESIS, not a result (the performance skill): machine against resume on three plain shapes, best of
 * rounds after warm-up, one JVM, no JMH. Enough to see an order of magnitude, nothing finer */
object Bench:
  enum Ask[+A]:
    case Number extends Ask[Int]
  type Row = Diag[Ask]

  def valueR[A](p: Freer[Pure, Unit, Unit, A]): A = p.resume match
    case Return(a) => a
    case other => throw IllegalStateException(s"$other")
  def valueM[A](p: Freer[Pure, Unit, Unit, A]): A =
    val r: Freer[Pure, Unit, Unit, A] = Machine.run(p)
    r match
      case Return(a) => a
      case other => throw IllegalStateException(s"$other")

  // handled outside: the resume loop and the machine loop over Ask
  @annotation.tailrec def askR[A](p: Freer[Row, Unit, Unit, A], in: Int): A = p.resume match
    case Return(a) => a
    case Bind(Inject(op), k) => op match
      case Ask.Number => askR(k(in), in)
    case Inject(Ask.Number) => in
    case other => throw IllegalStateException(s"$other")
  @annotation.tailrec def askM[A](p: Freer[Row, Unit, Unit, A], in: Int): A =
    val r: Freer[Row, Unit, Unit, A] = Machine.run(p)
    r match
      case Return(a) => a
      case Bind(Inject(op), k) => op match
        case Ask.Number => askM(k(in), in)
      case other => throw IllegalStateException(s"$other")

  def rightNested(n: Int): Freer[Pure, Unit, Unit, Int] =
    def loop(i: Int, acc: Int): Freer[Pure, Unit, Unit, Int] = if i == 0 then pure(acc) else delay(loop(i - 1, acc + 1)).flatMap(x => pure(x))
    loop(n, 0)
  def leftNested(n: Int): Freer[Pure, Unit, Unit, Int] =
    (1 to n).foldLeft(pure[Int, Unit](0): Freer[Pure, Unit, Unit, Int])((p, _) => p.map(_ + 1))
  def askLoop(n: Int): Freer[Row, Unit, Unit, Int] =
    def loop(i: Int, acc: Int): Freer[Row, Unit, Unit, Int] = if i == 0 then pure(acc) else inject(Ask.Number).flatMap(x => loop(i - 1, acc + x))
    loop(n, 0)

  def time(name: String, rounds: Int)(body: => Int): Unit =
    var best = Long.MaxValue
    var out = 0
    for _ <- 1 to rounds do
      val t0 = System.nanoTime(); out = body; val t = System.nanoTime() - t0
      if t < best then best = t
    println(f"$name%-34s ${best / 1e6}%8.1f ms  ($out)")

  def main(args: Array[String]): Unit =
    val n = 1000000
    for _ <- 1 to 3 do { valueR(rightNested(n)); valueM(rightNested(n)); askR(askLoop(n), 1); askM(askLoop(n), 1); valueR(leftNested(n/10)); valueM(leftNested(n/10)) }
    time("right-nested 1M, resume", 7)(valueR(rightNested(n)))
    time("right-nested 1M, machine", 7)(valueM(rightNested(n)))
    time("left-nested 100k, resume", 7)(valueR(leftNested(n / 10)))
    time("left-nested 100k, machine", 7)(valueM(leftNested(n / 10)))
    time("ask loop 1M handled outside, resume", 7)(askR(askLoop(n), 1))
    time("ask loop 1M handled outside, machine", 7)(askM(askLoop(n), 1))
