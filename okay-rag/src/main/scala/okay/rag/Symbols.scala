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
   * Walk one parsed file into an Index. `defHead` decides what a
   * definition node is called and what kind it is; every other
   * identifier is a mention.
   *
   * `identifiers` says whether this file's words ARE identifiers.
   * They are not, in prose: under a grammar with no definers every
   * word of a README becomes a leaf, and indexing those as references
   * buys two thousand mentions of "the" and nothing else. Prose is
   * what BM25 is for (`Keyword`), and it still lands there.
   */
  def of(source: String, tree: Cst[Code.K], identifiers: Boolean = true): Index =
    var index = Index()

    def walk(node: Cst[Code.K], path: Seq[String]): Unit = node match
      case Cst.Node(kind, kids) =>
        val head = if kind == "def" then defHead(kids) else None
        val here = if kind == "def" then path :+ head.map(_._2).getOrElse("?") else path
        if kind == "def" then
          for
            (kw, name) <- head
            sp <- span(node)
          do
            val sym = Symbol(name, kw, source, sp, path)
            index = index.copy(defs =
              index.defs.updated(name, index.defs.getOrElse(name, Vector.empty) :+ sym))
        kids.foreach(walk(_, here))
      case Cst.Leaf(t) =>
        if identifiers && isName(t) && t.kind == Code.K.Ident then
          index = index.copy(refs =
            index.refs.updated(t.lexeme,
              index.refs.getOrElse(t.lexeme, Vector.empty) :+ (source, t.span)))
      case Cst.Err(_, _) => ()

    walk(tree, Seq.empty)
    index

  /**
   * The identifier a definition node introduces.
   *
   * The scan covers the node's HEAD — the keywords, modifiers and
   * signature before the body — and stops at the first token that
   * proves the head is over: a `{`, or any punctuation at depth zero
   * (`=`, `:`, `;`, `,`). That stop is what keeps `val v = compute(1)`
   * from being named after `compute`.
   *
   * Within the head, two candidates:
   *
   *  1. the identifier that OPENS a parameter list — `hello` in
   *     `public String hello()`, and `Hello` in Go's
   *     `func (g Greeter) Hello()`, where the first parenthesized
   *     group is a receiver and not a signature;
   *  2. otherwise the identifier following the LAST keyword —
   *     `Thing` in `public static final class Thing`, `hello` in
   *     `def hello: String`, and `Greeter` in Go's
   *     `type Greeter struct`, where a trailing word that is not a
   *     definer would otherwise win.
   *
   * Parentheses and brackets nest, so a parameter's own type never
   * bids for the name.
   */
  private def defHead(kids: Vector[Cst[Code.K]]): Option[(String, String)] =
    val head = kids.collect { case Cst.Leaf(t) if t.channel == Channel.Syntax => t }

    var depth = 0
    var named: Option[(String, String)] = None    // (kind, name)
    var called: Option[(String, String)] = None
    var keyword = ""                              // the last keyword seen
    var expect = false                            // …and it wants a name
    var prev: Option[Token[Code.K]] = None
    var done = false

    for t <- head if !done do
      t.kind match
        case Code.K.Open if t.lexeme == "{" && depth == 0 => done = true
        case Code.K.Open =>
          if depth == 0 && t.lexeme == "(" && called.isEmpty then
            prev.filter(p => p.kind == Code.K.Ident && isName(p))
              .foreach(p => called = Some((keyword, p.lexeme)))
          depth += 1
        case Code.K.Close =>
          depth -= 1
          if depth < 0 then done = true
        case Code.K.Keyword if depth == 0 =>
          keyword = t.lexeme; expect = true
        case Code.K.Ident if depth == 0 && expect && isName(t) =>
          named = Some((keyword, t.lexeme)); expect = false
        case Code.K.Keyword | Code.K.Ident => ()
        case _ => if depth == 0 then done = true
      if !done then prev = Some(t)

    called.orElse(named)

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

  /** one source, parsed as its own language */
  def source(f: Source): Index =
    of(f.id, Code.source(f).tree,
      identifiers = Language.of(f.id).exists(_.definers.nonEmpty))

  /** index a whole project, file by file — a fold, therefore
   * mergeable, therefore distributable */
  def project(files: Seq[Source]): Index =
    files.foldLeft(Index())((idx, f) => idx.merge(source(f)))

  /** the index as a Fold, for streaming ingestion */
  def fold: Fold[Source, Index] =
    Fold(Index())((idx, f) => idx.merge(source(f)))

  /** the segment a symbol names — retrieval with no vectors in play */
  def segment(sym: Symbol, src: Source): Segment =
    Segment(sym.source, sym.span,
      src.text.substring(sym.span.offset, sym.span.offset + sym.span.length),
      sym.path :+ s"${sym.kind} ${sym.name}")
}
