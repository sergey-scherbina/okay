package okay.codec

/** the number json-escape-alloc claims, re-derivable; best of N,
 * because a single timed call at this scale prices the JIT */
object EscBench:
  private def best(label: String, n: Int = 12)(body: => Any): Unit =
    var lo = Double.MaxValue
    for _ <- 1 to n do
      val t0 = System.nanoTime(); body: Unit
      val d = (System.nanoTime() - t0) / 1e6
      if d < lo then lo = d
    println(f"  $label%-40s $lo%8.1f ms")

  /** escape as it was: a String allocated per character */
  def oldEscape(s: String): String =
    s.flatMap {
      case '"' => "\\\""
      case '\\' => "\\\\"
      case '\n' => "\\n"
      case '\t' => "\\t"
      case '\r' => "\\r"
      case c => c.toString
    }

  def main(args: Array[String]): Unit =
    val rows = args.headOption.flatMap(_.toIntOption).getOrElse(200000)
    val plain = Vector.tabulate(rows)(i => s"row $i with plain text")
    val quoted = Vector.tabulate(rows)(i => "a \"quoted\" row " + i + "\nwith a newline")
    val doc = Json.JObj(Vector("rows" -> Json.JArr(plain.map(Json.JStr(_)))))
    println(s"$rows strings")
    best("escape, nothing to escape (old)")(plain.map(oldEscape).length)
    best("escape, nothing to escape (new)")(plain.map(Json.escape).length)
    best("escape, every string escapes (old)")(quoted.map(oldEscape).length)
    best("escape, every string escapes (new)")(quoted.map(Json.escape).length)
    best("Json.print, whole document")(Json.print(doc).length)
    val agree = (plain ++ quoted).forall(s => oldEscape(s) == Json.escape(s))
    println(s"  the two agree on every string? $agree")
