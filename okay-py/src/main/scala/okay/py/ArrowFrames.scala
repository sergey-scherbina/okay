package okay.py

import okay.codec.ArrowIpc
import okay.codec.ArrowIpc.Column
import PyValue.*

/**
 * A `PyFrame` as Arrow columns and back (py-arrow). A column crosses as
 * Arrow when every cell is one kind — ints, floats, strings or bools —
 * or None; a column of None alone is Arrow's null type. Anything else (a
 * column mixing ints and floats, a big int, bytes, a list, a dict, a
 * handle) is not one of the five columns, and the frame takes the JSON
 * road instead: `table` says why.
 */
object ArrowFrames:

  def table(f: PyFrame): Either[String, ArrowIpc.Table] =
    val n = f.cols.headOption.fold(0)(_._2.length)
    f.cols.find(_._2.length != n) match
      case Some((name, c)) => Left(s"column '$name' has ${c.length} cells, the first has $n")
      case None =>
        val cols = f.cols.map((name, cells) => column(cells).left.map(why => s"column '$name' $why").map(name -> _))
        cols.collectFirst { case Left(why) => why }.toLeft(ArrowIpc.Table(cols.collect { case Right(c) => c }, Vector.empty))

  private def column(cells: Vector[PyValue]): Either[String, Column] =
    val valid = cells.map(_ != PyNone).toArray
    cells.find(_ != PyNone) match
      case None => Right(Column.Nulls(cells.length))
      case Some(I64(_)) => each(cells) { case I64(v) => v }.map(v => Column.Int64(v.toArray, valid))
      case Some(F64(_)) => each(cells) { case F64(v) => v }.map(v => Column.Float64(v.toArray, valid))
      case Some(Str(_)) => each(cells) { case Str(v) => v }.map(v => Column.Utf8(v.toArray, valid))
      case Some(Bool(_)) => each(cells) { case Bool(v) => v }.map(v => Column.Bool(v.toArray, valid))
      case Some(other) => Left(s"holds ${kind(other)}, which is not an Arrow column here")

  /** every non-None cell of the column's kind; a None becomes `zero`'s slot */
  private def each[A](cells: Vector[PyValue])(pick: PartialFunction[PyValue, A])(using z: Zero[A]): Either[String, Vector[A]] =
    val out = Vector.newBuilder[A]
    var bad: Option[PyValue] = None
    val it = cells.iterator
    while bad.isEmpty && it.hasNext do
      val c = it.next()
      if c == PyNone then out += z.zero
      else pick.lift(c) match
        case Some(a) => out += a
        case None => bad = Some(c)
    bad match
      case Some(c) => Left(s"mixes kinds (${kind(c)} among ${kind(cells.find(_ != PyNone).get)})")
      case None => Right(out.result())

  private trait Zero[A]:
    def zero: A
  private given Zero[Long] with
    def zero = 0L
  private given Zero[Double] with
    def zero = 0.0
  private given Zero[String] with
    def zero = ""
  private given Zero[Boolean] with
    def zero = false

  private def kind(v: PyValue): String = v match
    case PyNone => "None"
    case Bool(_) => "bools"
    case I64(_) => "ints"
    case BigI(_) => "an int past 64 bits"
    case F64(_) => "floats"
    case Str(_) => "strings"
    case Bytes(_) => "bytes"
    case Arr(_) => "lists"
    case Dict(_) => "dicts"
    case Ref(_) => "handles"

  def frame(t: ArrowIpc.Table): PyFrame =
    PyFrame(t.cols.map { (name, c) =>
      def cells[A](values: Array[A], ok: Array[Boolean])(cell: A => PyValue): Vector[PyValue] =
        Vector.tabulate(values.length)(i => if ok(i) then cell(values(i)) else PyNone)
      name -> (c match
        case Column.Int64(v, ok) => cells(v, ok)(I64(_))
        case Column.Float64(v, ok) => cells(v, ok)(F64(_))
        case Column.Utf8(v, ok) => cells(v, ok)(Str(_))
        case Column.Bool(v, ok) => cells(v, ok)(Bool(_))
        case Column.Nulls(n) => Vector.fill(n)(PyNone))
    })
