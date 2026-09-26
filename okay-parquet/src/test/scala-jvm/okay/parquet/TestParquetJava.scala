package okay.parquet

import okay.arrow.{Column, Table}

/**
 * THE STANDARD BESIDE OURS (specs/own-or-standard.md, point 5): the same
 * files either way — parquet-java reads what ours writes, ours reads what
 * parquet-java writes with its own defaults (dictionary pages, Snappy).
 */
class TestParquetJava extends munit.FunSuite:
  import ParquetSamples.*

  test("with no import the codec is ours; the import picks parquet-java; both name themselves") {
    assertEquals(summon[ParquetCodec].name, "okay")
    locally {
      import ParquetJava.given
      assertEquals(summon[ParquetCodec].name, "parquet-java")
    }
  }

  test("ours writes, parquet-java reads the same rows — every compression") {
    val t = sample(3000)
    for compress <- Compress.values do
      val bytes = OkayParquet.write(t, groupRows = 1000, compress = compress)
      val f = ParquetJava.footer(ReadAt.of(bytes))
      assertEquals(f.groups, Vector(1000L, 1000L, 1000L), s"$compress")
      assertEquals(f.columns, OkayParquet.footer(ReadAt.of(bytes)).columns, s"$compress: the two read one schema")
      same(t, ParquetJava.read(ReadAt.of(bytes)))
  }

  test("parquet-java writes (dictionary pages, its defaults), ours reads the same rows") {
    val t = sample(20000)
    for compress <- Compress.values do
      val bytes = ParquetJava.write(t, compress = compress)
      same(t, OkayParquet.read(ReadAt.of(bytes)))
  }

  test("parquet-java's DICTIONARY pages are what ours reads, not only PLAIN") {
    // a low-cardinality column: parquet-java's default writer dictionary-encodes it
    val n = 10000
    val t = Table(Vector("city" -> Column.Utf8(Array.tabulate(n)(i => s"city ${i % 12}"), Array.fill(n)(true))), Vector.empty)
    val bytes = ParquetJava.write(t)
    assert(bytes.length < n * 4, s"${bytes.length} bytes for $n rows: not dictionary-encoded")
    same(t, OkayParquet.read(ReadAt.of(bytes)))
  }

  test("by name, and the refusal names the jar to add") {
    assertEquals(Parquets.byName("okay").map(_.name), Right("okay"))
    assertEquals(Parquets.byName("parquet-java").map(_.name), Right("parquet-java"))
    assertEquals(Parquets.byName("duckdb"), Left("unknown Parquet implementation 'duckdb' (okay, parquet-java)"))
    assertEquals(ParquetJava.missing(), None)
    val why = ParquetJava.missing("org.apache.parquet.NoSuchClass").getOrElse(fail("a missing class went unnoticed"))
    assert(why.contains("parquet-hadoop:1.16.0") && why.contains("OkayParquet"), why)
  }
