package okay.refine

import okay.codec.{Cbor, Json, Xml, Yaml}
import okay.parse.Cst

/**
 * The first level: what FORMAT the bytes are in (specs/refine.md §2).
 *
 * Every text format is decided over the dialect's OWN lossless tree,
 * not by a sniff of the first byte: `{a: 1}` begins like JSON and is
 * not, and the parser already knows — its tree has an error node or it
 * does not, has structure or does not. A detector with a grammar of
 * its own would drift from the codec's; this one cannot, it IS the
 * codec's.
 *
 * `write` is the dialect's `render`, the lossless law made a function,
 * so a detected document writes back to the bytes it came from.
 */
enum Doc:
  case Json(tree: Cst[okay.lex.Json.K])
  case Xml(tree: Cst[okay.codec.Xml.K])
  case Yaml(tree: Cst[okay.codec.Yaml.K])
  case Cbor(bytes: Array[Byte])

object Format:

  /** bytes that are UTF-8 — the gate every text format stands behind */
  val text: Refine[Array[Byte], String] =
    Refine.step[Array[Byte], String]("text")(bs =>
      Utf8.invalidAt(bs) match
        case -1 => Right(new String(bs, java.nio.charset.StandardCharsets.UTF_8))
        case at => Left(s"not UTF-8 at byte $at"))(
      _.getBytes(java.nio.charset.StandardCharsets.UTF_8))

  /** a JSON object or array with no damage */
  val json: Refine[String, Doc.Json] =
    Refine.step[String, Doc.Json]("json")(s =>
      structured(Json.cst(s), Set("object", "array"), "a JSON object or array").map(Doc.Json(_)))(
      d => Json.render(d.tree))

  /** an XML document with at least one element and no damage */
  val xml: Refine[String, Doc.Xml] =
    Refine.step[String, Doc.Xml]("xml")(s =>
      val tree = Xml.cst(s)
      firstError(tree).toLeft(()).flatMap(_ =>
        if hasElement(tree) then Right(Doc.Xml(tree))
        else Left("no element")))(
      d => Xml.render(d.tree))

  /**
   * A YAML mapping or sequence with no damage, and NOTHING ELSE at the
   * root: the block dialect reads `{"a": 1}` as a scalar `{` followed by
   * a mapping (flow style is outside its scope, specs/codecs.md), with
   * no error node — so the structural test alone let YAML claim every
   * JSON object (found by TestFormat's first run). A root-level scalar
   * beside the structure is the tell.
   */
  val yaml: Refine[String, Doc.Yaml] =
    Refine.step[String, Doc.Yaml]("yaml")(s =>
      val tree = Yaml.cst(s)
      firstError(tree).toLeft(()).flatMap(_ =>
        if rootKinds(tree).exists(Set("map", "seq")) && !rootScalar(tree) then Right(Doc.Yaml(tree))
        else Left("not a YAML mapping or sequence")))(
      d => Yaml.render(d.tree))

  /** exactly one well-formed CBOR item */
  val cbor: Refine[Array[Byte], Doc.Cbor] =
    Refine.step[Array[Byte], Doc.Cbor]("cbor")(bs =>
      if bs.isEmpty then Left("empty")
      else
        val in = Cbor.In(bs)
        in.skipItem().flatMap(_ =>
          if in.peek == -1 then Right(Doc.Cbor(bs))
          else Left("bytes after the first item")))(
      _.bytes)

  /**
   * The level. EVERY alternative runs (specs/refine.md, Decisions):
   * `{"a": 1}` is a JSON object and a YAML mapping, and the answer to
   * that is `Unclear` naming both, never whichever came first.
   */
  val detect: Refine[Array[Byte], Doc] =
    cbor.widen[Doc] <|> (text andThen (json.widen[Doc] <|> xml.widen[Doc] <|> yaml.widen[Doc]))

  /**
   * The bridge from a detected document to a VALUE, so a Schema pattern
   * can follow: JSON and YAML project into the same `Json` (the one
   * decode algebra, specs/codecs.md); XML and CBOR have no value
   * projection here and decline saying so. The write renders the value
   * as JSON text and re-reads its tree — a document written back
   * through this bridge is JSON, whatever it was read from, which is
   * what makes a path through it a CONVERSION.
   */
  val value: Refine[Doc, Json] =
    Refine.step[Doc, Json]("value") {
      case Doc.Json(tree) => Right(Json.value(tree))
      case Doc.Yaml(tree) => Right(Yaml.parse(Yaml.render(tree)))
      case Doc.Xml(_) => Left("no value projection for xml")
      case Doc.Cbor(_) => Left("no value projection for cbor without a schema")
    }(j => Doc.Json(Json.cst(Json.print(j))))

  /** the tree's first error, as the reason — the parser's own words */
  private def firstError[K](tree: Cst[K]): Option[String] =
    Cst.errors(tree).headOption.map((t, m) => t.fold(m)(x => s"$m at ${x.span}"))

  /** no damage, and a root node of one of the kinds — a bare scalar is
   * declined, it is no document (specs/refine.md, Decisions) */
  private def structured[K](tree: Cst[K], kinds: Set[String], want: String): Either[String, Cst[K]] =
    firstError(tree).toLeft(()).flatMap(_ =>
      if rootKinds(tree).exists(kinds) then Right(tree) else Left(s"not $want"))

  /** the kinds of the nodes at the root — the root itself, or the
   * document node's children, since a dialect may wrap the value */
  private def rootKinds[K](tree: Cst[K]): Set[String] = tree match
    case Cst.Node(kind, kids) =>
      kids.collect { case Cst.Node(k, _) => k }.toSet + kind
    case _ => Set.empty

  /** a scalar at the root, beside or instead of the structure */
  private def rootScalar(tree: Cst[Yaml.K]): Boolean = tree match
    case Cst.Node(_, kids) => kids.exists {
      case Cst.Leaf(t) => t.kind == Yaml.K.Scalar || t.kind == Yaml.K.Quoted
      case _ => false
    }
    case _ => false

  private def hasElement(tree: Cst[Xml.K]): Boolean =
    var found = false
    var stack: List[Cst[Xml.K]] = tree :: Nil
    while !found && stack.nonEmpty do
      val here = stack.head
      stack = stack.tail
      here match
        case Cst.Node(_, kids) =>
          if kids.exists { case Cst.Leaf(t) => t.kind == Xml.K.Open || t.kind == Xml.K.SelfClose; case _ => false }
          then found = true
          else stack = kids.foldRight(stack)(_ :: _)
        case _ => ()
    found

  /** UTF-8 validity by hand, so the same check runs on every
   * platform: the index of the first offending byte, or -1 */
  private[refine] object Utf8:
    def invalidAt(bs: Array[Byte]): Int =
      var i = 0
      val n = bs.length
      var bad = -1
      while bad < 0 && i < n do
        val b = bs(i) & 0xFF
        val need =
          if b < 0x80 then 0
          else if (b & 0xE0) == 0xC0 && b >= 0xC2 then 1
          else if (b & 0xF0) == 0xE0 then 2
          else if (b & 0xF8) == 0xF0 && b <= 0xF4 then 3
          else -1
        if need < 0 then bad = i
        else
          var k = 1
          while bad < 0 && k <= need do
            if i + k >= n || (bs(i + k) & 0xC0) != 0x80 then bad = i
            k += 1
          if bad < 0 then i += need + 1
      bad
