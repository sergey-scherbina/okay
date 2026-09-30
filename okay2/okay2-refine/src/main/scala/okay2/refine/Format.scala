package okay2.refine

import okay2.codec.{Json, Xml}
import okay2.parse.Cst

/**
 * The first level: what FORMAT the bytes are in — okay-refine's
 * `Format`, for the dialects the Scala 2 codec has today: JSON and
 * XML. YAML and CBOR join the level the day okay2-codec reads them
 * (a new format is one more alternative in `detect`, nothing here
 * edited).
 *
 * Every text format is decided over the dialect's OWN lossless tree,
 * not by a sniff of the first byte: the parser already knows — its
 * tree has an error node or it does not, has structure or does not.
 * `write` is the dialect's `render`, so a detected document writes
 * back to the bytes it came from.
 */
sealed trait Doc
object Doc {
  final case class Json(tree: Cst[okay2.lex.Json.K]) extends Doc
  final case class Xml(tree: Cst[okay2.codec.Xml.K]) extends Doc
}

object Format {

  /** bytes that are UTF-8 — the gate every text format stands behind */
  val text: Refine[Array[Byte], String] =
    Refine.step[Array[Byte], String]("text")(bs =>
      Utf8.invalidAt(bs) match {
        case -1 => Right(new String(bs, java.nio.charset.StandardCharsets.UTF_8))
        case at => Left(s"not UTF-8 at byte $at")
      })(
      _.getBytes(java.nio.charset.StandardCharsets.UTF_8))

  /** the first character that is not blank (a byte-order mark skipped),
   * or -1 — what each dialect asks BEFORE its total parser runs: a
   * necessary condition, so the verdicts do not move, only the reason
   * is sooner (okay-refine's format-cheap-decline) */
  private def lead(s: String): Int = {
    var i = if (s.startsWith("﻿")) 1 else 0
    while (i < s.length && Character.isWhitespace(s.charAt(i))) i += 1
    if (i < s.length) s.charAt(i).toInt else -1
  }

  /** a JSON object or array with no damage */
  val json: Refine[String, Doc.Json] =
    Refine.step[String, Doc.Json]("json")(s =>
      lead(s) match {
        case '{' | '[' => structured(Json.cst(s), Set("object", "array"), "a JSON object or array").map(Doc.Json(_))
        case -1 => Left("empty")
        case c => Left(s"begins with '${c.toChar}', not { or [")
      })(
      d => Json.render(d.tree))

  /** an XML document with at least one element and no damage — STRICT
   * XML, no HTML void elements: this detects data documents, and an
   * HTML page's `<br>` declining as "never closed" is the right answer */
  val xml: Refine[String, Doc.Xml] =
    Refine.step[String, Doc.Xml]("xml")(s =>
      // a well-formed document's prolog and root all begin with `<`: text before the root is not XML
      if (lead(s) != '<') Left(if (lead(s) == -1) "empty" else s"begins with '${lead(s).toChar}', not <")
      else {
        val tree = Xml.cst(s, Xml.strict)
        firstError(tree).toLeft(()).flatMap(_ =>
          if (hasElement(tree)) Right(Doc.Xml(tree))
          else Left("no element"))
      })(
      d => Xml.render(d.tree))

  /** The level. EVERY alternative runs: two takers are `Unclear`, never
   * whichever came first. */
  val detect: Refine[Array[Byte], Doc] =
    text andThen (json.widen[Doc] <|> xml.widen[Doc])

  /** The bridge from a detected document to a VALUE, so a Schema
   * pattern can follow: JSON into `Json`, XML through `Xml.value`
   * (elements as objects, attributes as `@name`, repeats as arrays).
   * The write renders the value as JSON text and re-reads its tree — a
   * document written back through this bridge is JSON, whatever it was
   * read from, which is what makes a path through it a CONVERSION. */
  val value: Refine[Doc, Json] =
    Refine.step[Doc, Json]("value") {
      case Doc.Json(tree) => Right(Json.value(tree))
      case Doc.Xml(tree) => Right(Xml.value(tree))
    }(j => Doc.Json(Json.cst(Json.print(j))))

  /** the tree's first error, as the reason — the parser's own words */
  private def firstError[K](tree: Cst[K]): Option[String] =
    Cst.errors(tree).headOption.map { case (t, m) => t.fold(m)(x => s"$m at ${x.span}") }

  /** no damage, and a root node of one of the kinds — a bare scalar is
   * declined, it is no document */
  private def structured[K](tree: Cst[K], kinds: Set[String], want: String): Either[String, Cst[K]] =
    firstError(tree).toLeft(()).flatMap(_ =>
      if (rootKinds(tree).exists(kinds)) Right(tree) else Left(s"not $want"))

  /** the kinds of the nodes at the root — the root itself, or the
   * document node's children, since a dialect may wrap the value */
  private def rootKinds[K](tree: Cst[K]): Set[String] = tree match {
    case Cst.Node(kind, kids) => kids.collect { case Cst.Node(k, _) => k }.toSet + kind
    case _ => Set.empty
  }

  private def hasElement(tree: Cst[Xml.K]): Boolean = {
    var found = false
    var stack: List[Cst[Xml.K]] = tree :: Nil
    while (!found && stack.nonEmpty) {
      val here = stack.head
      stack = stack.tail
      here match {
        case Cst.Node(_, kids) =>
          if (kids.exists { case Cst.Leaf(t) => t.kind == Xml.K.Open || t.kind == Xml.K.SelfClose; case _ => false })
            found = true
          else stack = kids.foldRight(stack)(_ :: _)
        case _ => ()
      }
    }
    found
  }

  /** UTF-8 validity by hand, so the same check runs on every
   * platform: the index of the first offending byte, or -1 */
  private[refine] object Utf8 {
    def invalidAt(bs: Array[Byte]): Int = {
      var i = 0
      val n = bs.length
      var bad = -1
      while (bad < 0 && i < n) {
        val b = bs(i) & 0xFF
        val need =
          if (b < 0x80) 0
          else if ((b & 0xE0) == 0xC0 && b >= 0xC2) 1
          else if ((b & 0xF0) == 0xE0) 2
          else if ((b & 0xF8) == 0xF0 && b <= 0xF4) 3
          else -1
        if (need < 0) bad = i
        else {
          var k = 1
          while (bad < 0 && k <= need) {
            if (i + k >= n || (bs(i + k) & 0xC0) != 0x80) bad = i
            k += 1
          }
          if (bad < 0) i += need + 1
        }
      }
      bad
    }
  }
}
