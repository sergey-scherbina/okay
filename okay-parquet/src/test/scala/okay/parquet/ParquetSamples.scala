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

