package okay.codec

import okay.*
import okay.lex.Scan
import okay.lex.Json as JsonLex
import okay.parse.{Cst, JsonParse, Parse}

/** where the lossless road's time goes; best of N, because a single
 * timed call at this scale measures the JIT as much as the code */
object ProfCst:
  private def best(label: String, n: Int = 12)(body: => Any): Unit =
    var lo = Double.MaxValue
    for _ <- 1 to n do
      val t0 = System.nanoTime()
      body: Unit
      val d = (System.nanoTime() - t0) / 1e6
      if d < lo then lo = d
    println(f"  $label%-40s $lo%8.1f ms")

  /** what Json.cst was before json-cst-batch-road: one Writer.tell per
   * CHARACTER, two transducer stages, a LazyList. Kept here, in one
   * place, so TestJsonCst can prove the new road builds the same tree
   * and this profiler can price the difference. */
  private def chars(t: String, i: Int = 0): Unit ! Writer % Char =
    if i >= t.length then pure(())
    else Writer.tell(t.charAt(i)).flatMap(_ => chars(t, i + 1))

  def streamingCst(t: String): Cst[okay.lex.Json.K] = Parse.toCst(
    through(through(chars(t))(Scan.stage(JsonLex.scan)))(JsonParse.driver).toLazyList)

  // the projection as it was before json-projection-alloc: a Vector
  // from every node and every leaf, and grouped(2) for the fields.
  // Kept here so the two can be A/B'd in ONE run — across runs the
  // GC state of a million-object tree swamps the difference.
  private def oldValues(c: Cst[okay.lex.Json.K]): Vector[Json] = c match
    case Cst.Node("object", kids) => Vector(Json.JObj(oldPairs(kids)))
    case Cst.Node("array", kids) => Vector(Json.JArr(kids.flatMap(oldValues)))
    case Cst.Node(_, kids) => kids.flatMap(oldValues)
    case Cst.Leaf(t) => t.kind match
      case okay.lex.Json.K.Str => Vector(Json.JStr(t.lexeme))
      case okay.lex.Json.K.Num => Vector(Json.JNum(t.lexeme.toDoubleOption.getOrElse(0d)))
      case okay.lex.Json.K.Bool => Vector(Json.JBool(t.lexeme == "true"))
      case okay.lex.Json.K.Null => Vector(Json.JNull)
      case _ => Vector.empty
    case Cst.Err(_, m) => Vector(Json.JErr(m))

  private def oldPairs(kids: Vector[Cst[okay.lex.Json.K]]): Vector[(String, Json)] =
    val vs = kids.flatMap(oldValues)
    vs.grouped(2).collect {
      case Vector(Json.JStr(k), v) => (k, v)
      case Vector(Json.JErr(m), v) => (s"<$m>", v)
    }.toVector

  def main(args: Array[String]): Unit =
    val rows = args.headOption.flatMap(_.toIntOption).getOrElse(100000)
    val nums = Json.JArr(Vector.tabulate(rows)(i => Json.JNum(i.toDouble)))
    val strs = Json.JArr(Vector.tabulate(rows)(i => Json.JStr("row" + i)))
    val doc = Json.JObj(Vector("cols" -> Json.JArr(Vector(
      Json.JArr(Vector(Json.JStr("a"), nums)),
      Json.JArr(Vector(Json.JStr("s"), strs))))))
    val text = Json.print(doc)
    println(s"${text.length} bytes")

    best("Json.parse (fast value road)")(Json.parse(text).hashCode)
    best("Json.lossless (CST + projection)")(Json.lossless(text).hashCode)
    best("Json.cst (the tree alone)")(Json.cst(text).hashCode)
    best("  the OLD streaming road, same tree")(streamingCst(text).hashCode)
    best("  with reparse snapshots (every 64)")(
      Parse.full(JsonLex.scan, JsonParse.instrs)(text, 64).tree.hashCode)
    best("  Scan.all alone")(Scan.all(JsonLex.scan)(text, Int.MaxValue).tokens.length)
    val toks = Scan.all(JsonLex.scan)(text, Int.MaxValue).tokens
    best("  instrs alone")(toks.iterator.flatMap(JsonParse.instrs).length)
    val tree = Json.cst(text)
    best("  projection alone (new)")(Json.value(tree).hashCode)
    best("  projection alone (OLD shape)")(oldValues(tree).hashCode)
    println(s"  render byte-identical? ${Json.render(tree) == text}")
