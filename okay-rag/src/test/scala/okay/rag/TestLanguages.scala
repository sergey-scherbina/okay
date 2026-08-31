package okay.rag

import okay.parse.Cst

/**
 * One definition-boundary grammar, described per language. What is
 * asserted for each is the same short list — the tree is lossless,
 * the definitions are found and named, and a damaged file still
 * indexes — because that is all this layer promises, and promising it
 * for a new language should cost five lines in `Language`.
 */
class TestLanguages extends munit.FunSuite {

  def index(src: Source, lang: Language): Index =
    Symbols.of(src.id, Code.parse(src.text, 64, lang).tree)

  def check(lang: Language, id: String, text: String, expect: Seq[String]): Unit =
    val src = Source(id, text)
    val tree = Code.parse(text, 64, lang).tree
    assertEquals(Cst.lexemes(tree), text, s"${lang.name}: not lossless")
    val idx = Symbols.of(id, tree)
    for name <- expect do
      assert(idx.definition(name).nonEmpty,
        s"${lang.name}: '$name' not found among ${idx.names.toSeq.sorted.take(12)}")
    // and every definition quotes its file exactly
    for sym <- expect.flatMap(idx.definition) do
      assert(Symbols.segment(sym, src).quotes(src), s"${lang.name}: $sym misquotes")

  /** three quotes, written where a triple-quoted literal cannot hold them */
  val q3 = "\"\"\""

  test("scala") {
    check(Language.scala, "A.scala",
      s"""package p
         |/** doc */
         |class Greeter(name: String) {
         |  def hello: String = s"Hi, $$name"
         |  private val secret = ${q3}triple
         |  " quoted, and a trailing slash \\$q3
         |}
         |object Main { def run(): Unit = () }
         |""".stripMargin,
      Seq("Greeter", "hello", "secret", "Main", "run"))
  }

  test("a definition owns its BLOCK, not its parameter list") {
    // `class Greeter(name: String) { … }` used to end at the `)`,
    // which cost the body and, for a Go method, the name as well
    val text = "class Greeter(name: String) {\n  def hello = 1\n}\nobject Main\n"
    val idx = index(Source("A.scala", text), Language.scala)
    val greeter = idx.definition("Greeter").head
    val hello = idx.definition("hello").head
    val main = idx.definition("Main").head
    val ends = greeter.span.offset + greeter.span.length
    assert(ends > text.indexOf('}'), s"Greeter ended at ${ends}, before its body")
    assert(hello.span.offset < ends, "hello is not inside the class that contains it")
    assert(main.span.offset >= ends, "Main was swallowed by the class")
    assertEquals(hello.path, Seq("Greeter"), "the nesting path is not recorded")

    val go = index(Source("a.go", "func (g Greeter) Hello() string {\n\treturn \"\"\n}\n"),
      Language.go)
    assert(go.definition("Hello").nonEmpty,
      "a Go method is named after its receiver, not by it")
  }

  test("java") {
    check(Language.java, "A.java",
      """package p;
        |/** doc */
        |public class Greeter {
        |  private String name;
        |  public String hello() { return "Hi, " + name; }
        |}
        |interface Greets { void greet(); }
        |""".stripMargin,
      Seq("Greeter", "Greets"))
  }

  test("javascript and typescript") {
    check(Language.javascript, "a.js",
      """// a comment
        |function hello(name) { return `Hi, ${name}`; }
        |const greeter = { hi: 1 };
        |class Greeter { constructor() {} }
        |""".stripMargin,
      Seq("hello", "greeter", "Greeter"))

    check(Language.typescript, "a.ts",
      """interface Greets { greet(): void }
        |type Name = string
        |export function hello(n: Name): string { return "hi" }
        |""".stripMargin,
      Seq("Greets", "Name", "hello"))
  }

  test("rust") {
    check(Language.rust, "a.rs",
      """/// doc
        |pub struct Greeter { name: String }
        |impl Greeter {
        |    fn hello(&self) -> String { format!("Hi, {}", self.name) }
        |}
        |trait Greets { fn greet(&self); }
        |""".stripMargin,
      Seq("Greeter", "hello", "Greets"))
  }

  test("go") {
    check(Language.go, "a.go",
      """package main
        |// a comment
        |type Greeter struct { name string }
        |func (g Greeter) Hello() string { return "Hi, " + g.name }
        |""".stripMargin,
      Seq("Greeter", "Hello"))
  }

  test("python: structure by INDENTATION, not braces") {
    val text =
      """# a comment
        |class Greeter:
        |    def __init__(self, name):
        |        self.name = name
        |
        |    def hello(self):
        |        return f"Hi, {self.name}"
        |
        |def main():
        |    print(Greeter("x").hello())
        |""".stripMargin
    check(Language.python, "a.py", text, Seq("Greeter", "hello", "main"))

    // the nesting is real: hello lives inside Greeter, main does not
    val idx = index(Source("a.py", text), Language.python)
    val greeter = idx.definition("Greeter").head
    val hello = idx.definition("hello").head
    val main = idx.definition("main").head
    val inside = greeter.span.offset + greeter.span.length
    assert(hello.span.offset < inside,
      "hello was not nested inside the class its indentation puts it in")
    assert(main.span.offset >= inside, "main was swallowed by the class")
  }

  test("python: a triple-quoted docstring is one token, braces inside are text") {
    val text =
      "def f():\n    \"\"\"doc with } and # not a comment\"\"\"\n    return 1\n"
    val tree = Code.parse(text, 64, Language.python).tree
    assertEquals(Cst.lexemes(tree), text)
    assert(Symbols.of("a.py", tree).definition("f").nonEmpty)
  }

  test("comments and strings cannot open a definition") {
    // the words are there, but only as comment and string content
    val text =
      """// def notADefinition
        |val s = "class NotAClass"
        |/* object NotAnObject */
        |val real = 1
        |""".stripMargin
    val idx = index(Source("a.scala", text), Language.scala)
    assert(idx.definition("real").nonEmpty)
    assertEquals(idx.definition("notADefinition"), Vector.empty)
    assertEquals(idx.definition("NotAClass"), Vector.empty)
    assertEquals(idx.definition("NotAnObject"), Vector.empty)
  }

  test("the language is chosen by extension, and an unknown one still parses") {
    assertEquals(Language.of("src/Main.scala").map(_.name), Some("scala"))
    assertEquals(Language.of("a/b/c.py").map(_.name), Some("python"))
    assertEquals(Language.of("x.rs").map(_.name), Some("rust"))
    assertEquals(Language.of("README.md"), None)
    // an unknown extension falls back and still produces a tree
    val p = Code.parseFile("notes.txt", "some text\n")
    assertEquals(Cst.lexemes(p.tree), "some text\n")
  }

  test("a modifier chain is one definition, and it keeps its doc comment") {
    // `final case class` is three keywords, two of them definers —
    // as three nodes the doc comment landed on an unnamed one ahead
    // of the class, and the class had no doc at all
    def doc(lang: Language, id: String, text: String, name: String): Unit =
      val src = Source(id, text)
      val sym = Symbols.source(src).definition(name)
      assertEquals(sym.size, 1, s"${lang.name}: $name defined ${sym.size} times")
      val seg = Symbols.segment(sym.head, src)
      assert(seg.quotes(src))
      assert(seg.text.contains("what it is for"),
        s"${lang.name}: the doc comment is outside the definition:\n${seg.text}")

    doc(Language.scala, "A.scala",
      "/** what it is for */\nfinal case class Thing(a: Int) {\n  def f = 1\n}\n",
      "Thing")
    doc(Language.java, "A.java",
      "/** what it is for */\npublic static final class Thing {\n  int a;\n}\n",
      "Thing")
    doc(Language.rust, "a.rs",
      "/// what it is for\npub struct Thing { a: u8 }\n", "Thing")

    // Python's docstring is not a comment above the definition, it is
    // the first statement of the body — so it is inside by nesting,
    // and `async def` still has to be one definition and not two
    doc(Language.python, "a.py",
      "async def thing():\n    \"\"\"what it is for\"\"\"\n    return 1\n", "thing")
  }

  test("a mixed-language project indexes each file by its own grammar") {
    val files = Seq(
      Source("a.scala", "class Greeter { def hello = 1 }\n"),
      Source("b.py", "class Snake:\n    def slither(self):\n        pass\n"),
      Source("c.rs", "pub fn crab() -> u8 { 1 }\n"),
      Source("d.go", "func (g G) Gopher() {}\n"),
      Source("e.js", "function node(a) { return a; }\n"))
    val idx = Symbols.project(files)
    for name <- Seq("Greeter", "hello", "Snake", "slither", "crab", "Gopher", "node") do
      assert(idx.definition(name).nonEmpty,
        s"'$name' missing from ${idx.names.toSeq.sorted}")
    // and each symbol names the file it actually came from
    assertEquals(idx.definition("slither").head.source, "b.py")
    assertEquals(idx.definition("crab").head.source, "c.rs")
  }

  test("prose is not read as code") {
    // under Scala's rules every one of these words would open a
    // definition; a file no language claims has no definers at all
    val md = Source("README.md",
      "The type of a given value, the case for it, and a val or two.\n")
    val idx = Symbols.project(Seq(md))
    assertEquals(idx.names, Set.empty[String])
    assertEquals(Cst.lexemes(Code.source(md).tree), md.text)
  }

  test("a damaged file in any language still indexes") {
    for (lang, text) <- Seq(
      Language.scala -> "class A { def f = \"unterminated\n",
      Language.python -> "class A:\n    def f(:\n        \"\"\"unclosed\n",
      Language.rust -> "fn main() { let x = /* unclosed\n",
      Language.javascript -> "function f( { `unclosed template\n")
    do
      val tree = Code.parse(text, 64, lang).tree
      assertEquals(Cst.lexemes(tree), text, s"${lang.name}: damage lost the text")
  }
}
