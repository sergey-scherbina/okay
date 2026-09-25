package okay.r

import okay.codec.Schema
import okay.py.WireLink

/**
 * A frame an R worker ANSWERS is read by R's value rules, whatever is in
 * scope where it is read (foreign-one-value: a frame carries its rules).
 * Default gate: the far side is a scripted link speaking R's hello, so no R
 * is needed. Python's rules would refuse the typed NA below; R's read it as
 * an absent `Option`.
 */
class TestRFrameRules extends munit.FunSuite:

  final case class Obs(n: Option[Int], x: Double) derives Schema

  private def scripted(answer: String): WireLink = new WireLink:
    def hello(): Option[String] = Some("""{"shim":9,"r":"4.4.1","speaks":{"frames":["columnar"]}}""")
    def roundTrip(line: String): Option[String] = Some(answer)
    def exchange(message: Array[Byte]): Option[Array[Byte]] = None
    def close(): Unit = ()

  test("an R-answered frame's rows read a typed NA as None, by R's rules") {
    val frame = """{"t":"frame","v":2,"cols":[""" +
      """{"name":"n","type":"i","values":[7,0],"na":[1]},""" +
      """{"name":"x","type":"d","values":[1.5,2.5],"na":[],"nan":[]}]}"""
    val r = RSubprocess.over(scripted(s"""{"id":1,"ok":$frame}"""))
    val answered = r.handler.handle(REval.Frame("m::f", RFrame(Vector.empty), Vector.empty))
    // read where no R rules are in scope: the frame's own rules decide
    assertEquals(answered.flatMap(_.rows[Obs]), Right(Vector(Obs(Some(7), 1.5), Obs(None, 2.5))))
  }

  test("the same cells built by Python's rules are refused at the NA — the rules are what differs") {
    val cols = Vector("n" -> Vector(RValue.I32(7), RValue.NA(RType.Integer)), "x" -> Vector(RValue.F64(1.5), RValue.F64(2.5)))
    assert(okay.py.PyFrame(cols).rows[Obs].isLeft)
    assertEquals(RFrame(cols).rows[Obs], Right(Vector(Obs(Some(7), 1.5), Obs(None, 2.5))))
  }
