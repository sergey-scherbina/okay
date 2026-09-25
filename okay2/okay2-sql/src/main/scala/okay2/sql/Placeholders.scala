package okay2.sql

/** `?` placeholders renumbered `$1..$n` for engines that want them;
 * quoted literals and identifiers are left alone (okay-sql's
 * Placeholders.scala) */
object Placeholders {

  def numbered(sql: String): String = {
    val sb = new StringBuilder(sql.length + 8)
    var i = 0
    var n = 0
    var quote: Char = 0
    while (i < sql.length) {
      val c = sql.charAt(i)
      if (quote != 0) {
        sb.append(c)
        if (c == quote) quote = 0
      } else c match {
        case '\'' | '"' => quote = c; sb.append(c)
        case '?' => n += 1; sb.append('$').append(n)
        case _ => sb.append(c)
      }
      i += 1
    }
    sb.result()
  }
}
