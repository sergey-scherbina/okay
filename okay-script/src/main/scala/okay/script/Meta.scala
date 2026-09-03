package okay.script

/** Front-matter + heading-scoped ```yaml metadata, as a typed AST and
 * as a current-position Context -- see specs/okay-script.md
 * "Metadata as context".
 */
object Meta:

  enum Value:
    case Str(s: String)
    case Arr(items: Vector[Value])
    case Obj(fields: Vector[(String, Value)])

    def field(key: String): Option[Value] = this match
      case Value.Obj(fs) => fs.collectFirst { case (k, v) if k == key => v }
      case _ => None

    def asString: Option[String] = this match
      case Value.Str(s) => Some(s)
      case _ => None

  final case class Section(level: Int, title: String, yaml: Vector[Value], children: Vector[Section])

  /** `root` is synthetic (level 0, title ""), carrying any ```yaml
   * that appeared before the first heading; `frontMatter` is the
   * `---`-delimited block, if present.
   */
  final case class Doc(frontMatter: Map[String, String], root: Section)

  /** `doc` is the WHOLE file's tree; `path` is the ancestor chain
   * (root..nearest heading) for one position in the document -- see
   * the spec for why a path Section's `children` may be incomplete
   * while `doc.root`'s never is.
   */
  final case class Context(doc: Doc, path: Vector[Section]):
    /** nearest-enclosing-heading-wins, then front-matter */
    def get(key: String): Option[String] =
      path.reverseIterator
        .flatMap(s => s.yaml.reverseIterator.flatMap(_.field(key)).flatMap(_.asString))
        .nextOption()
        .orElse(doc.frontMatter.get(key))

    def apply(key: String): String =
      get(key).getOrElse(throw new NoSuchElementException(s"Meta.Context: no key \"$key\""))

    /** the nearest REAL enclosing heading -- excludes the synthetic
     * root (level 0), so this is `None` outside any heading, even
     * though `path` itself always includes the root. */
    def section: Option[Section] = path.lastOption.filter(_.level > 0)

  private val empty: Context = Context(Doc(Map.empty, Section(0, "", Vector.empty, Vector.empty)), Vector.empty)

  // Confirmed empirically (2026-09-03): a plain `given ctx: T = expr`
  // is a VAL, evaluated ONCE, not re-evaluated per summon -- so a
  // `given` cannot be the auto-refreshing "current position" this
  // needs. `current`/`setCurrent` -- a plain, always-fresh method call
  // -- is the real mechanism; code that wants `given`/context-function
  // ergonomics writes `given Meta.Context = Meta.current` itself,
  // locally, immediately before where it is used (correct there,
  // since it is a fresh read at that exact point, not carried forward
  // across a heading transition the way an auto-injected one would
  // have to be).
  @volatile private var _current: Context = empty

  /** the metadata for wherever synthesized code has most recently told
   * `setCurrent` it is -- see specs/okay-script.md "Metadata as
   * context".
   */
  def current: Context = _current

  def setCurrent(c: Context): Unit = _current = c

  private val headingRe = """(#{1,6})\s+(.*?)\s*""".r
  private val yamlFenceOpen = """```yaml(\s+\S+)?\s*""".r
  private val fenceClose = """```\s*""".r

  def parse(markdown: String): Doc =
    val lines = markdown.linesWithSeparators.toVector.map(_.stripLineEnd)

    var i = 0
    var frontMatter: Map[String, String] = Map.empty
    if lines.headOption.contains("---") then
      val end = lines.indexWhere(_ == "---", 1)
      if end > 0 then
        frontMatter = lines.slice(1, end).flatMap(parseKv).collect { case (k, Value.Str(v)) => k -> v }.toMap
        i = end + 1

    final case class Building(level: Int, title: String, var yaml: Vector[Value], var children: Vector[Section])
    val stack = scala.collection.mutable.ArrayBuffer(Building(0, "", Vector.empty, Vector.empty))
    def closeTo(level: Int): Unit =
      while stack.length > 1 && stack.last.level >= level do
        val b = stack.remove(stack.length - 1)
        val sec = Section(b.level, b.title, b.yaml, b.children)
        stack.last.children = stack.last.children :+ sec

    while i < lines.length do
      val line = lines(i)
      if yamlFenceOpen.matches(line) then
        var j = i + 1
        val body = Vector.newBuilder[String]
        while j < lines.length && !fenceClose.matches(lines(j)) do
          body += lines(j); j += 1
        stack.last.yaml = stack.last.yaml :+ parseYaml(body.result())
        i = j + 1
      else
        line match
          case headingRe(hashes, title) =>
            val level = hashes.length
            closeTo(level)
            stack += Building(level, title, Vector.empty, Vector.empty)
            i += 1
          case _ =>
            i += 1

    closeTo(1)
    Doc(frontMatter, Section(0, "", stack.head.yaml, stack.head.children))

  private def parseKv(raw: String): Option[(String, Value)] =
    val s = raw.trim
    val idx = s.indexOf(':')
    if idx < 0 then None
    else
      val k = s.take(idx).trim
      val v = s.drop(idx + 1).trim
      if k.isEmpty then None else Some(k -> Value.Str(v))

  /** A minimal YAML subset -- flat mapping, or a list of flat mappings
   * (`- key: v` starting a new object, indented `key: v` lines
   * continuing it) -- the shape ../it-consulting's own site content
   * uses. Not a general YAML parser. `private[script]`: ScalaScript's
   * own tokenizer parses ```yaml fences as it walks a document, and
   * reuses this rather than duplicating it.
   */
  private[script] def parseYaml(lines: Vector[String]): Value =
    if lines.exists(_.trim.startsWith("- ")) then
      val items = Vector.newBuilder[Value]
      var current = Vector.newBuilder[(String, Value)]
      var any = false
      def flush(): Unit =
        if any then items += Value.Obj(current.result())
        current = Vector.newBuilder[(String, Value)]
        any = false
      for raw <- lines do
        val trimmed = raw.trim
        if trimmed.startsWith("- ") then
          flush()
          parseKv(trimmed.drop(2)).foreach { case (k, v) => current += (k -> v); any = true }
        else if trimmed.nonEmpty then
          parseKv(trimmed).foreach { case (k, v) => current += (k -> v); any = true }
      flush()
      Value.Arr(items.result())
    else
      Value.Obj(lines.flatMap(l => parseKv(l)))
