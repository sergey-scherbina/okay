package okay.k5


/** multi-prompt capture DERIVED from captures to the nearest delimiter only: shift0 twice, the inner one's
 * delimiter put back with `dollar` around the inner continuation, the outer continuation as its `ret` */
object Nearest:
  def main(args: Array[String]): Unit =
    val p = Prompt[Pure, Unit, Int]("p")
    val q = Prompt[Pure, Unit, Int]("q")
    // the reference: a capture to p THROUGH q's delimiter (the machine's Under walk): 64
    val crossing = reset(p)(reset(q)(shift0[Int](p)(k => k(1).flatMap(k)).map(_ + 10)).map(_ * 2))
    // the same with captures to the NEAREST delimiter only (Has.Head each time): k1 up to q, then k2 up to p,
    // and k(x) = dollar(p)(k2)(k1(x)) — the inner segment under a fresh p whose ret is the outer continuation
    val nearest = reset(p)(reset(q)(
      shift0[Int](q)(k1 =>
        shift0[Int](p)(k2 =>
          val k: Int => Freer[Pure, EmptyTuple, Unit, Unit, Int] = x => dollar(p)(k2)(k1(x))
          k(1).flatMap(k))).map(_ + 10)).map(_ * 2))
    println(Test.value(crossing))
    println(Test.value(nearest))
