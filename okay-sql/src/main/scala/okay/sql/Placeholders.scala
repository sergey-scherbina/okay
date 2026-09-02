package okay.sql

/**
 * The one mechanical dialect difference the seam itself introduced
 * (specs/sql.md, demo-pg-backend): JDBC binds `?`, the pg wire binds
 * `$n`. A program written against `?` runs on the pg driver by
 * renumbering — a pure string rewrite, NOT a dialect layer: every
 * other character of the statement stays the string the DBA reads.
 */
object Placeholders:

  /** `?` outside single-quoted literals and double-quoted identifiers
   * becomes `$1..$n` in order of appearance. A doubled `''` inside a
   * literal closes and reopens the quote, which is exactly right. A
   * statement that uses pg's own `?` operators (jsonb) is not a `?`
   * program and does not ask for this. */
  def numbered(sql: String): String =
    val sb = new StringBuilder(sql.length + 8)
    var i = 0
    var n = 0
    var quote: Char = 0
    while i < sql.length do
      val c = sql.charAt(i)
      if quote != 0 then
        sb.append(c): Unit
        if c == quote then quote = 0
      else c match
        case '\'' | '"' => quote = c; sb.append(c): Unit
        case '?' => n += 1; sb.append('$').append(n): Unit
        case _ => sb.append(c): Unit
      i += 1
    sb.result()
