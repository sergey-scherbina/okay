package okay.py

import okay.{Choose, effect, runChoice, given}
import PyValue.*

/**
 * The program-as-data walker without an interpreter (remote-foreign):
 * a scripted far side that, like the shim, KEEPS every continuation by id
 * — so a continuation continued twice is visible here (default gate).
 */
class TestPyProgramShape extends munit.FunSuite {

  /** `choose [1,2]` then `choose [10,20]`, then their sum: kept by id */
  private final class Far extends okay.Handler[PyEval]:
    var continued = Vector.empty[Long]
    private val konts = scala.collection.mutable.Map.empty[Long, PyValue => PyNode]
    private var next = 0L
    private def node(n: PyNode): Either[Condition, PyNode] = Right(n)
    private def step(name: String, args: Vector[PyValue], k: PyValue => PyNode): PyNode =
      next += 1
      konts(next) = k
      PyNode.Perform(name, args, next)
    def handle[A](op: PyEval[A]): A = op match
      case PyEval.Program(_, "m:pairs", _, _, _) =>
        node(step("choose", Vector(Arr(Vector(I64(1), I64(2)))), {
          case I64(x) => step("choose", Vector(Arr(Vector(I64(10), I64(20)))), {
            case I64(y) => PyNode.Done(I64(x + y))
            case v => PyNode.Done(Str(s"bad $v"))
          })
          case v => PyNode.Done(Str(s"bad $v"))
        }))
      case PyEval.Continue(_, k, a) =>
        continued :+= k
        a.map(konts(k))
      case PyEval.Forget(_) => ()
      case other => throw IllegalArgumentException(s"not scripted: $other")

  test("a Choice handler continues the SAME continuation twice, and every branch answers") {
    val far = Far()
    val choose = Py.callback[Vector[Long], Long]("choose")(xs => effect[Choose, Long](Choose(xs)))
    val run = Py.program[Long]("m:pairs").calling(Py.callbacks(choose))()
    assertEquals(runChoice(run.program).runWith(using far).toList, List(Right(11L), Right(21L), Right(12L), Right(22L)))
    // k=1 (the first choice) twice; each branch then makes its own second choice
    assertEquals(far.continued.count(_ == 1L), 2)
  }
}
