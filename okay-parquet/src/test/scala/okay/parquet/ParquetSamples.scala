package okay.parquet

import okay.arrow.{Column, Table, TimeUnit}

/** tables and comparisons every Parquet test uses, on every platform */
object ParquetSamples extends munit.Assertions:
  /** a table of every type we write, with nulls in every column */
  def sample(n: Int, seed: Int = 1): Table =
    val rnd = scala.util.Random(seed)
    def ok = Array.tabulate(n)(i => i % 7 != 3)
    Table(Vector(
      "id" -> Column.Int64(Array.tabulate(n)(_.toLong * 1000003L), Array.fill(n)(true)),
      "i8" -> Column.Ints(8, true, Array.tabulate(n)(i => (i % 256 - 128).toLong), ok),
      "u16" -> Column.Ints(16, false, Array.tabulate(n)(i => (i * 7 % 65536).toLong), ok),
      "i32" -> Column.Ints(32, true, Array.tabulate(n)(_ => rnd.nextInt().toLong), ok),
      "f32" -> Column.Float32(Array.tabulate(n)(i => i * 0.5f), ok),
      "f64" -> Column.Float64(Array.tabulate(n)(_ => rnd.nextGaussian()), ok),
      "flag" -> Column.Bool(Array.tabulate(n)(i => i % 3 == 0), ok),
      "name" -> Column.Utf8(Array.tabulate(n)(i => s"row $i — чай"), ok),
      "blob" -> Column.Binary(Array.tabulate(n)(i => Array.fill(i % 5)(i.toByte)), ok),
      "hash" -> Column.FixedBinary(4, Array.tabulate(n)(i => Array[Byte](i.toByte, 1, 2, 3)), ok),
      "day" -> Column.Date32(Array.tabulate(n)(i => 19000 + i % 400), ok),
      "at" -> Column.Timestamp(TimeUnit.Micro, Some("UTC"), Array.tabulate(n)(i => 1700000000000000L + i), ok),
      "naive" -> Column.Timestamp(TimeUnit.Milli, None, Array.tabulate(n)(i => 1700000000000L + i), ok)),
      Vector("written.by" -> "TestOkayParquet"))

  def same(a: Table, b: Table): Unit =
    assertEquals(b.cols.map(_._1), a.cols.map(_._1))
    assertEquals(b.rows, a.rows)
    for ((name, x), (_, y)) <- a.cols.zip(b.cols) do
      assertEquals(Column.describe(y), Column.describe(x), name)
      assertEquals(y.validity.toVector, x.validity.toVector, s"$name: validity")
      for i <- 0 until x.length if x.validity(i) do
        assertEquals(cell(y, i), cell(x, i), s"$name, row $i")

  def cell(c: Column, i: Int): Any = c match
    case Column.Int64(v, _) => v(i)
    case Column.Ints(_, _, v, _) => v(i)
    case Column.Float32(v, _) => v(i)
    case Column.Float64(v, _) => v(i)
    case Column.Bool(v, _) => v(i)
    case Column.Utf8(v, _) => v(i)
    case Column.Binary(v, _) => v(i).toVector
    case Column.FixedBinary(_, v, _) => v(i).toVector
    case Column.Date32(v, _) => v(i)
    case Column.Timestamp(_, _, v, _) => v(i)
    case Column.Decimal(_, _, v, _) => v(i)
    case other => other

  /** a cell as a comparable value, nulls and nesting included */
  def value(c: Column, i: Int): Any =
    if !c.validity(i) then null
    else c match
      case Column.ListOf(o, child, _) => (o(i) until o(i + 1)).toVector.map(value(child, _))
      case Column.Struct(fs, _) => fs.map((k, f) => k -> value(f, i))
      case other => cell(other, i)

  /** every column's rows as values */
  def values(t: Table): Vector[(String, Vector[Any])] =
    t.cols.map((n, c) => n -> (0 until c.length).toVector.map(value(c, _)))

  /** lists (null, empty, with null elements), structs (null, with null
   * fields), a list of structs and a list of lists */
  def nested(n: Int): Table =
    val all = Array.fill(n)(true)
    // tags: row i has i % 4 strings; every 5th row a null list, every 7th an empty one
    val tagOk = Array.tabulate(n)(i => i % 5 != 0)
    val tagLen = Array.tabulate(n)(i => if !tagOk(i) || i % 7 == 0 then 0 else i % 4 + 1)
    val tagOff = tagLen.scanLeft(0)(_ + _)
    val tagVals = Array.tabulate(tagOff(n))(k => s"tag $k")
    val tagValid = Array.tabulate(tagOff(n))(k => k % 9 != 4)
    // point: null every 6th row, y null every 4th
    val px = Array.tabulate(n)(_ * 0.5)
    val py = Array.tabulate(n)(_ * 1.5)
    // trips: list of (id, km), i % 3 of them
    val tripLen = Array.tabulate(n)(_ % 3)
    val tripOff = tripLen.scanLeft(0)(_ + _)
    val m = tripOff(n)
    // matrix: list of lists of ints: i % 3 rows of (i % 2 + 1) ints
    val rowsLen = Array.tabulate(n)(_ % 3)
    val rowsOff = rowsLen.scanLeft(0)(_ + _)
    val inner = rowsOff(n)
    val innerLen = Array.tabulate(inner)(k => k % 2 + (if k % 5 == 0 then 0 else 1))
    val innerOff = innerLen.scanLeft(0)(_ + _)
    Table(Vector(
      "id" -> Column.Int64(Array.tabulate(n)(_.toLong), all),
      "tags" -> Column.ListOf(tagOff, Column.Utf8(tagVals, tagValid), tagOk),
      "point" -> Column.Struct(Vector(
        "x" -> Column.Float64(px, Array.fill(n)(true)),
        "y" -> Column.Float64(py, Array.tabulate(n)(_ % 4 != 0))), Array.tabulate(n)(_ % 6 != 0)),
      "trips" -> Column.ListOf(tripOff, Column.Struct(Vector(
        "id" -> Column.Int64(Array.tabulate(m)(_.toLong * 10), Array.fill(m)(true)),
        "km" -> Column.Float64(Array.tabulate(m)(_ * 0.25), Array.tabulate(m)(_ % 3 != 1))), Array.fill(m)(true)), all),
      "matrix" -> Column.ListOf(rowsOff, Column.ListOf(innerOff,
        Column.Int64(Array.tabulate(innerOff(inner))(_.toLong), Array.fill(innerOff(inner))(true)),
        Array.fill(inner)(true)), all)),
      Vector.empty)

