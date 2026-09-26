package okay.parquet

import okay.arrow.Column
import java.nio.file.Files
import java.sql.DriverManager

/**
 * DUCKDB AS A THIRD ORACLE (specs/data.md, duckdb-lake-reads): embedded,
 * no network. It reads what ours writes; ours reads what it writes with
 * its own defaults (its dictionary pages, its compression).
 */
class TestParquetDuckDb extends munit.FunSuite:
  import ParquetSamples.*

  def query[A](sql: String)(f: java.sql.ResultSet => A): A =
    val c = DriverManager.getConnection("jdbc:duckdb:")
    try
      val rs = c.createStatement().executeQuery(sql)
      rs.next(): Unit
      f(rs)
    finally c.close()

  test("DuckDB reads ours: the rows, the nulls, the strings") {
    val t = sample(4000)
    val file = Files.createTempFile("okay-parquet", ".parquet")
    Files.write(file, OkayParquet.write(t, groupRows = 1500)): Unit
    val (n, ids, names, nullNames) = query(
      s"select count(*), sum(id), count(distinct name), count(*) - count(name) from read_parquet('$file')") { rs =>
      (rs.getLong(1), rs.getBigDecimal(2).longValue, rs.getLong(3), rs.getLong(4)) }
    assertEquals(n, 4000L)
    assertEquals(ids, (0 until 4000).map(_.toLong * 1000003L).sum)
    val absent = (0 until 4000).count(_ % 7 == 3)
    assertEquals(nullNames, absent.toLong)
    assertEquals(names, (4000 - absent).toLong)
    val at = query(s"select \"at\" from read_parquet('$file') where id = 1000003")(_.getObject(1).toString)
    assert(at.startsWith("2023-11-14"), s"a UTC timestamp read back as $at")
  }

  test("ours reads DuckDB's: dictionary pages, its compression, its types") {
    for codec <- Vector("snappy", "zstd", "uncompressed") do
      val file = Files.createTempFile("okay-duckdb", ".parquet")
      Files.delete(file)
      val c = DriverManager.getConnection("jdbc:duckdb:")
      try c.createStatement().execute(
        s"""copy (select range as id, 'city ' || (range % 13) as city,
                  case when range % 5 = 0 then null else (range * 0.25)::double end as x,
                  range * 0.25 as price,
                  (range % 2 = 0) as flag
                  from range(50000))
            to '$file' (format parquet, compression $codec, row_group_size 20000)""")
      finally c.close()
      val t = OkayParquet.read(ReadAt.of(Files.readAllBytes(file)))
      assertEquals(t.rows, 50000, codec)
      assertEquals(OkayParquet.footer(ReadAt.of(Files.readAllBytes(file))).groups.length, 3, codec)
      t.cols.toMap.apply("city") match
        case Column.Utf8(v, _) => assertEquals(v(27), "city 1", codec)
        case other => fail(s"$codec: city as ${Column.describe(other)}")
      t.cols.toMap.apply("x") match
        case Column.Float64(v, ok) =>
          assert(!ok(0) && ok(1), codec); assertEquals(v(3), 0.75, codec)
        case other => fail(s"$codec: x as ${Column.describe(other)}")
      // DuckDB's `range * 0.25` is a DECIMAL: read as one, unscaled
      t.cols.toMap.apply("price") match
        case Column.Decimal(_, 2, v, _) => assertEquals(v(3), BigInt(75), codec)
        case other => fail(s"$codec: price as ${Column.describe(other)}")
  }

  test("DuckDB's nested columns — lists, structs, a list of structs — read equal") {
    val file = Files.createTempFile("okay-duckdb-nested", ".parquet")
    Files.delete(file)
    val c = DriverManager.getConnection("jdbc:duckdb:")
    try c.createStatement().execute(
      s"""copy (select range as id,
                case when range % 5 = 0 then null else list_transform(range(range % 3), k -> 'v' || k) end as xs,
                case when range % 4 = 0 then null else {'a': range, 'b': 'b' || range} end as st,
                list_transform(range(range % 2), k -> {'k': k, 'w': k * 0.5}) as ps
                from range(3000))
          to '$file' (format parquet, row_group_size 1000)""")
    finally c.close()
    val t = OkayParquet.read(ReadAt.of(Files.readAllBytes(file)))
    val n = 3000
    assertEquals(values(t), Vector(
      "id" -> Vector.tabulate(n)(_.toLong),
      "xs" -> Vector.tabulate(n)(i => if i % 5 == 0 then null else (0 until i % 3).toVector.map(k => s"v$k")),
      "st" -> Vector.tabulate(n)(i => if i % 4 == 0 then null else Vector("a" -> i.toLong, "b" -> s"b$i")),
      "ps" -> Vector.tabulate(n)(i => (0 until i % 2).toVector.map(k => Vector("k" -> k.toLong, "w" -> k * 0.5)))))
  }

  test("DuckDB reads ours NESTED") {
    val t = nested(2000)
    val file = Files.createTempFile("okay-parquet-nested", ".parquet")
    Files.write(file, OkayParquet.write(t, groupRows = 700)): Unit
    val v = values(t).toMap
    def lists(col: String) = v(col).collect { case xs: Vector[?] => xs.length.toLong }.sum
    val (tags, nullTags, trips, points) = query(
      s"""select sum(len(tags)), count(*) - count(tags), sum(len(trips)), count(point)
          from read_parquet('$file')""") { rs => (rs.getLong(1), rs.getLong(2), rs.getLong(3), rs.getLong(4)) }
    assertEquals(tags, lists("tags"))
    assertEquals(nullTags, v("tags").count(_ == null).toLong)
    assertEquals(trips, lists("trips"))
    assertEquals(points, v("point").count(_ != null).toLong)
    val km = query(s"select sum(t.km) from (select unnest(trips) as t from read_parquet('$file'))")(_.getDouble(1))
    val expected = v("trips").collect { case xs: Vector[?] => xs }.flatten.collect {
      case fs: Vector[?] => fs.collectFirst { case ("km", k: Double) => k }.getOrElse(0.0) }.sum
    assertEqualsDouble(km, expected, 1e-9)
  }

