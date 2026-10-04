package okay2.codec

final case class SgAddress(city: String, zip: String, line: Option[String])
final case class SgOrder(id: Long, user: String, amount: Double, active: Boolean,
                         tags: List[String], addr: SgAddress, note: Option[String],
                         priority: Int = 3, scores: Vector[Double] = Vector(1.5))
sealed trait SgShape
object SgShape {
  final case class Circle(r: Double) extends SgShape
  final case class Square(side: Double, name: String) extends SgShape
  case object Dot extends SgShape
}
final case class SgDrawing(shapes: List[SgShape], main: SgShape)
/** a newtype: travels as its underlying String in BOTH modes */
final case class SgEmail(value: String)
object SgEmail {
  implicit val schema: Schema[SgEmail] = Schema.wrap[SgEmail, String](SgEmail(_), _.value)
}
final case class SgContact(email: SgEmail, name: String)
/** recursion: the type meets itself, the staged code delegates the inner fold */
final case class SgTree(label: String, kids: List[SgTree])
object SgTree {
  implicit lazy val schema: Schema[SgTree] = Schema.derived
}

/** the fixtures both staged suites share (okay-codec's TestStaged and
 * TestStagedCbor declare the same ones each) */
object StagedModels {
  val orders: Seq[SgOrder] = Seq(
    SgOrder(42L, "ada", 12.5, true, List("new", "vip"), SgAddress("Kyiv", "01001", None), Some("leave at door")),
    SgOrder(0L, "", 0.0, false, Nil, SgAddress("", "", Some("")), None),
    SgOrder(-7L, "q\"uo\\te\n", -1e300, true, List("a\tb"), SgAddress("x", "y", Some("z")), Some("")),
    SgOrder(1L, "u", 3.0, true, List("t"), SgAddress("c", "z", None), None, priority = 9, scores = Vector()))

  val drawings: Seq[SgDrawing] = Seq(
    SgDrawing(List(SgShape.Circle(1.0), SgShape.Dot, SgShape.Square(2.0, "s")), SgShape.Dot),
    SgDrawing(Nil, SgShape.Circle(0.5)),
    SgDrawing(List(SgShape.Square(1, "a\"b")), SgShape.Square(2, "")))
}
