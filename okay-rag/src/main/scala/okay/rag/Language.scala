package okay.rag

/**
 * What a definition-boundary grammar needs to know about a language
 * (specs/rag.md, P10f). Not a language definition — a description of
 * the four things that decide where a definition starts and ends:
 * how comments are written, how strings are written, which words
 * introduce a definition, and whether structure is delimited by
 * braces or by indentation.
 *
 * That is deliberately little, and it is only enough because the
 * parser is TOTAL: what a description does not cover becomes ordinary
 * leaves rather than a failure, so a language can be added in five
 * lines and sharpened later without a rewrite.
 */
enum Layout:
  /** structure is delimited by braces: C, Java, Scala, JS, Rust, Go */
  case Braces
  /** structure is delimited by indentation: Python */
  case Indent

final case class Language(name: String,
                          extensions: Set[String],
                          lineComment: String,
                          blockComment: Option[(String, String)],
                          docPrefix: Option[String],
                          quotes: Set[Char],
                          triple: Boolean,
                          definers: Set[String],
                          layout: Layout)

object Language {

  val scala: Language = Language("scala", Set("scala", "sc"),
    "//", Some(("/*", "*/")), Some("/**"), Set('"', '\''), triple = true,
    Set("def", "val", "var", "class", "object", "trait", "enum", "type",
      "given", "case"),
    Layout.Braces)

  val java: Language = Language("java", Set("java"),
    "//", Some(("/*", "*/")), Some("/**"), Set('"', '\''), triple = true,
    Set("class", "interface", "enum", "record", "void", "public", "private",
      "protected", "static"),
    Layout.Braces)

  val javascript: Language = Language("javascript", Set("js", "mjs", "cjs", "jsx"),
    "//", Some(("/*", "*/")), Some("/**"), Set('"', '\'', '`'), triple = false,
    Set("function", "class", "const", "let", "var", "async", "export"),
    Layout.Braces)

  val typescript: Language = javascript.copy(name = "typescript",
    extensions = Set("ts", "tsx"),
    definers = javascript.definers ++ Set("interface", "type", "enum", "declare"))

  val rust: Language = Language("rust", Set("rs"),
    "//", Some(("/*", "*/")), Some("///"), Set('"'), triple = false,
    Set("fn", "struct", "enum", "trait", "impl", "mod", "const", "static",
      "type", "macro_rules"),
    Layout.Braces)

  val go: Language = Language("go", Set("go"),
    "//", Some(("/*", "*/")), None, Set('"', '`', '\''), triple = false,
    // not `package`: it introduces no definition, and as a definer it
    // would open a node that swallows the file
    Set("func", "type", "var", "const"),
    Layout.Braces)

  val c: Language = Language("c", Set("c", "h", "cc", "cpp", "hpp", "cxx"),
    "//", Some(("/*", "*/")), Some("/**"), Set('"', '\''), triple = false,
    Set("struct", "union", "enum", "typedef", "class", "namespace", "template",
      "static", "void", "int"),
    Layout.Braces)

  val python: Language = Language("python", Set("py", "pyi"),
    "#", None, None, Set('"', '\''), triple = true,
    Set("def", "class", "async"),
    Layout.Indent)

  /**
   * The fallback for a file no language claims: no comments, no
   * strings, no definers, so the file becomes a flat run of leaves
   * and splits by size alone. Prose deserves that and not a
   * programming language's grammar — under Scala's rules a README
   * saying "the type of a given value" would open two definitions.
   */
  val text: Language = Language("text", Set.empty,
    "", None, None, Set.empty, triple = false, Set.empty, Layout.Braces)

  val all: Seq[Language] =
    Seq(scala, java, javascript, typescript, rust, go, c, python)

  private val byExtension: Map[String, Language] =
    all.flatMap(l => l.extensions.map(e => (e, l))).toMap

  /** the language of a path, by extension */
  def of(path: String): Option[Language] =
    val i = path.lastIndexOf('.')
    if i < 0 then None else byExtension.get(path.substring(i + 1).toLowerCase)

  /** every extension any known language claims */
  def extensions: Set[String] = byExtension.keySet
}
