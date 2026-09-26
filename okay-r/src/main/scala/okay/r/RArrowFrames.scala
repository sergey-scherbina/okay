package okay.r

import okay.arrow.{Column, Table}
import RValue.*

/**
 * An `RFrame` as Arrow columns and back (r-arrow), okay-py's `ArrowFrames`
 * twin. A column crosses as Arrow when every cell is one of R's four
 * atomic types (logical, integer, double, character) or `NA`; anything
 * else (a raw column, a nested list, a held object, a record) is not one
 * of those, and the frame takes the JSON/CBOR road instead: `table` says
 * why. Unlike Python's `I64`-only model, R's own `I32` (32-bit integer)
 * stays 32-bit on the way out too — there is no widening to do.
 */
object RArrowFrames extends okay.foreign.FrameTables:

  def table(f: RFrame): Either[String, Table] =
    val n = f.cols.headOption.fold(0)(_._2.length)
    f.cols.find(_._2.length != n) match
      case Some((name, c)) => Left(s"column '$name' has ${c.length} cells, the first has $n")
      case None =>
        val cols = f.cols.map((name, cells) => column(cells).left.map(why => s"column '$name' $why").map(name -> _))
        cols.collectFirst { case Left(why) => why }.toLeft(Table(cols.collect { case Right(c) => c }, Vector.empty))

  private def notNA(v: RValue): Boolean = v match
    case NA(_) => false
    case _ => true

  private def column(cells: Vector[RValue]): Either[String, Column] =
    val valid = cells.map(notNA).toArray
    cells.find(notNA) match
      case None =>
        // every cell is NA (or the column is empty): the first NA names the
        // R type to hold, else this is a column of nulls alone
        Right(cells.collectFirst { case NA(t) => t } match
          case Some(RType.Logical) => Column.Bool(new Array[Boolean](cells.length), valid)
          case Some(RType.Integer) => Column.Ints(32, true, new Array[Long](cells.length), valid)
          case Some(RType.Double) => Column.Float64(new Array[Double](cells.length), valid)
          case Some(RType.Character) => Column.Utf8(Array.fill(cells.length)(""), valid)
          case None => Column.Nulls(cells.length))
      case Some(Bool(_)) => each(cells) { case Bool(v) => v }.map(v => Column.Bool(v.toArray, valid))
      case Some(I32(_)) => each(cells) { case I32(v) => v.toLong }.map(v => Column.Ints(32, true, v.toArray, valid))
      case Some(F64(_)) => each(cells) { case F64(v) => v }.map(v => Column.Float64(v.toArray, valid))
      case Some(Str(_)) => each(cells) { case Str(v) => v }.map(v => Column.Utf8(v.toArray, valid))
      case Some(other) => Left(s"holds ${kind(other)}, which is not an Arrow column here")

  /** every non-NA cell of the column's kind; an NA becomes `zero`'s slot */
  private def each[A](cells: Vector[RValue])(pick: PartialFunction[RValue, A])(using z: Zero[A]): Either[String, Vector[A]] =
    val out = Vector.newBuilder[A]
    var bad: Option[RValue] = None
    val it = cells.iterator
    while bad.isEmpty && it.hasNext do
      val c = it.next()
      c match
        case NA(_) => out += z.zero
        case _ => pick.lift(c) match
          case Some(a) => out += a
          case None => bad = Some(c)
    bad match
      case Some(c) => Left(s"mixes kinds (${kind(c)} among ${kind(cells.find(notNA).get)})")
      case None => Right(out.result())

  private trait Zero[A]:
    def zero: A
  private given Zero[Boolean] with
    def zero = false
  private given Zero[Long] with
    def zero = 0L
  private given Zero[Double] with
    def zero = 0.0
  private given Zero[String] with
    def zero = ""

  private def kind(v: RValue): String = v match
    case RNull => "NULL"
    case NA(_) => "NA"
    case Bool(_) => "logicals"
    case I32(_) => "integers"
    case F64(_) => "doubles"
    case Str(_) => "characters"
    case Bytes(_) => "raw"
    case Vec(_) => "a nested vector"
    case Named(_) => "a named list"
    case Ref(_) => "a held object"
    case _ => "an integer past 32 bits"

  /** a table as a frame. A column past R's own four types is normalised on
   * the way in (narrow ints stay `I32`, a wider or unsigned int and
   * float32 widen to `F64` rather than overflow); a kind an `RFrame`
   * cannot say at all (a list, a struct, bytes) is refused by name. */
  def frame(t: Table): RFrame =
    RFrame(t.cols.map((name, c) => name -> cells(c, name)))

  private def cells(c0: Column, name: String): Vector[RValue] =
    // a dictionary reads as its values (okay-arrow stage 8)
    val c = c0.decoded
    def each[A](values: Array[A], ok: Array[Boolean])(cell: A => RValue, na: RType): Vector[RValue] =
      Vector.tabulate(values.length)(i => if ok(i) then cell(values(i)) else NA(na))
    c match
      case Column.Bool(v, ok) => each(v, ok)(Bool(_), RType.Logical)
      case Column.Float64(v, ok) => each(v, ok)(F64(_), RType.Double)
      case Column.Utf8(v, ok) => each(v, ok)(Str(_), RType.Character)
      case Column.Nulls(n) => Vector.fill(n)(NA(RType.Logical))
      // a signed int of 32 bits or fewer fits R's own integer exactly;
      // anything wider, or unsigned at 32 bits, may not, so it widens
      // to double rather than risk a silent overflow
      case Column.Ints(bits, signed, v, ok) if signed && bits <= 32 =>
        each(v, ok)(x => I32(x.toInt), RType.Integer)
      case Column.Ints(_, _, v, ok) => each(v, ok)(x => F64(x.toDouble), RType.Double)
      case Column.Float32(v, ok) => each(v, ok)(x => F64(x.toDouble), RType.Double)
      case other =>
        throw IllegalStateException(s"column '$name' is Arrow ${other.getClass.getSimpleName}, which an RFrame cannot say")
