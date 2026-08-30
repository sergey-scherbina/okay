package okay.rag

import okay.Fold
import okay.lex.{Channel, Token}
import okay.parse.Cst

/**
 * Structural retrieval — the half of code search that needs no
 * embeddings at all (specs/rag.md, P10f). Definitions and references
 * are collected by a FOLD over the parse, which means the index is
 * mergeable: the index of a project is the merge of the indexes of
 * its files, so building it distributes and updating it incrementally
 * is the same operation.
 *
 * "The definition of X" and "what mentions X" are exact answers, and
 * for code they are usually better than similarity. Semantic search
 * covers the other half — "where is the thing that does…" — and the
 * two fuse (P10c).
 */

/** one definition: what it is called, what kind it is, where it lives */
final case class Symbol(name: String, kind: String, source: String,
                        span: okay.lex.Span, path: Seq[String])

/**
 * The index: definitions by name, and every mention by name. Both
 * sides are plain maps, so `merge` is map-union — the Monoid the
 * whole design keeps leaning on.
 */
final case class Index(defs: Map[String, Vector[Symbol]] = Map.empty,
                       refs: Map[String, Vector[(String, okay.lex.Span)]] = Map.empty):
  def merge(that: Index): Index =
    def join[A](a: Map[String, Vector[A]], b: Map[String, Vector[A]]) =
      b.foldLeft(a)((m, kv) => m.updated(kv._1, m.getOrElse(kv._1, Vector.empty) ++ kv._2))
    Index(join(defs, that.defs), join(refs, that.refs))

  /** exactly where a name is defined */
  def definition(name: String): Vector[Symbol] = defs.getOrElse(name, Vector.empty)

  /** everywhere it is mentioned, definitions included */
  def mentions(name: String): Vector[(String, okay.lex.Span)] =
    refs.getOrElse(name, Vector.empty)

  /** the names defined here, for a browsing agent */
  def names: Set[String] = defs.keySet

object Symbols {

  /** the index is a Monoid: files combine, so projects distribute */
  given okay.Monoid[Index] with
    def empty: Index = Index()
    def combine(x: Index, y: Index): Index = x.merge(y)

  private def isName[K](t: Token[K]): Boolean =
    t.channel == Channel.Syntax && t.lexeme.nonEmpty &&
      (t.lexeme.head.isLetter || t.lexeme.head == '_')

  /**
   * Walk one parsed file into an Index. A definition node's name is
   * the first identifier after its keyword; the kind is the keyword
   * itself. Every other identifier is a mention.
   */
  def of(source: String, tree: Cst[Code.K]): Index =
    var index = Index()

    def walk(node: Cst[Code.K], path: Seq[String]): Unit = node match
      case Cst.Node(kind, kids) =>
        val here = if kind == "def" then path :+ defName(kids).getOrElse("?") else path
        if kind == "def" then
          for
            name <- defName(kids)
            kw <- kids.collectFirst { case Cst.Leaf(t) if t.kind == Code.K.Keyword => t }
            sp <- span(node)
          do
            val sym = Symbol(name, kw.lexeme, source, sp, path)
            index = index.copy(defs =
              index.defs.updated(name, index.defs.getOrElse(name, Vector.empty) :+ sym))
        kids.foreach(walk(_, here))
      case Cst.Leaf(t) =>
        if isName(t) && t.kind == Code.K.Ident then
          index = index.copy(refs =
            index.refs.updated(t.lexeme,
              index.refs.getOrElse(t.lexeme, Vector.empty) :+ (source, t.span)))
      case Cst.Err(_, _) => ()

    walk(tree, Seq.empty)
    index

  /** the identifier a definition node introduces: the first one after
   * its keyword, before any brace */
  private def defName(kids: Vector[Cst[Code.K]]): Option[String] =
    val leaves = kids.collect { case Cst.Leaf(t) => t }
    leaves.dropWhile(_.kind != Code.K.Keyword).drop(1)
      .takeWhile(t => t.kind != Code.K.Open)
      .find(t => t.kind == Code.K.Ident && isName(t))
      .map(_.lexeme)

  /** the byte range a subtree covers */
  private def span(c: Cst[Code.K]): Option[okay.lex.Span] =
    def toks(x: Cst[Code.K]): Vector[Token[Code.K]] = x match
      case Cst.Node(_, kids) => kids.flatMap(toks)
      case Cst.Leaf(t) => Vector(t)
      case Cst.Err(t, _) => t.toVector

    val ts = toks(c)
    if ts.isEmpty then None
    else
      val first = ts.minBy(_.span.offset).span
      val last = ts.maxBy(t => t.span.offset + t.span.length).span
      Some(okay.lex.Span(first.offset, first.line, first.column,
        (last.offset + last.length) - first.offset))

  /** index a whole project, file by file — a fold, therefore
   * mergeable, therefore distributable */
  def project(files: Seq[Source]): Index =
    files.foldLeft(Index()) { (idx, f) =>
      idx.merge(of(f.id, Code.parse(f.text).tree))
    }

  /** the index as a Fold, for streaming ingestion */
  def fold: Fold[Source, Index] =
    Fold(Index())((idx, f) => idx.merge(of(f.id, Code.parse(f.text).tree)))

  /** the segment a symbol names — retrieval with no vectors in play */
  def segment(sym: Symbol, src: Source): Segment =
    Segment(sym.source, sym.span,
      src.text.substring(sym.span.offset, sym.span.offset + sym.span.length),
      sym.path :+ s"${sym.kind} ${sym.name}")
}
