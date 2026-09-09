package okay.ui

import okay.codec.{Cbor, Json, Schema}

/**
 * The frontend protocol as an ARTIFACT (specs/frontend.md, stage 1):
 * one definition — the derived `Schema`s of the tree, the events, the
 * patches and the message envelope — from which both encodings
 * (JSON lines, CBOR bytes), the contract document a client in another
 * language reads, and the conformance script come. Nothing here is
 * hand-mapped: a field renamed in `Ui` renames itself on the wire and
 * in the document in the same compile.
 *
 * The shapes are the codec's own: a sum is `{"Case": {...fields}}`, a
 * product its fields by name, an enumeration its lower-case name.
 *
 * The conversation:
 *
 *   client → server   Hello {vocab, version}   first, once
 *   server → client   Tree {ui}                the full tree, then
 *   server → client   Patch {patch}            narrow patches
 *   client → server   Event {event}            what the user did
 *   either            Close
 *
 * A server that receives an Event before any Hello serves the client
 * as level-L (an empty vocabulary): the handshake is what a client
 * gains by speaking it, not a gate.
 */
object Protocol {

  /** the protocol's version; a Hello names the one it speaks */
  val version: Int = 1

  // ---- the enumerations spell themselves as short lower-case names
  given Schema[Dir] = Schema.enumeration(Dir.values.toVector, d => if d == Dir.Horizontal then "h" else "v")
  given Schema[Role] = Schema.enumeration(Role.values.toVector, _.toString.toLowerCase)
  given Schema[InputKind] = Schema.enumeration(InputKind.values.toVector, _.toString.toLowerCase)
  given Schema[Tone] = Schema.enumeration(Tone.values.toVector, _.toString.toLowerCase)
  given Schema[Size] = Schema.enumeration(Size.values.toVector, _.toString.toLowerCase)
  given Schema[Style] = Schema.derived
  given Schema[Ui] = Schema.derived
  given Schema[Event] = Schema.derived
  given Schema[Patch] = Schema.derived

  /** the envelope: every line on the wire is one of these */
  enum Msg:
    case Hello(vocab: Vector[String], version: Int)
    case Tree(ui: Ui)
    case Patch(patch: okay.ui.Patch)
    case Event(event: okay.ui.Event)
    case Close

  given Schema[Msg] = Schema.derived

  def hello(vocab: Set[String]): Msg = Msg.Hello(vocab.toVector.sorted, version)

  // ---- JSON lines
  def line(m: Msg): String = Json.write(m)
  /** total: damage is None, never a throw — a wire that crashes on
   * one bad line loses every good one after it */
  def parse(s: String): Option[Msg] = Json.read[Msg](s).toOption

  // ---- CBOR bytes, the same definition
  def bytes(m: Msg): Array[Byte] = Cbor.write(m)
  def ofBytes(b: Array[Byte]): Option[Msg] = Cbor.read[Msg](b).toOption

  // ---- the shapes tests and transports ask for by name
  def eventLine(e: Event): String = line(Msg.Event(e))
  def treeOf(l: String): Option[Ui] = parse(l).collect { case Msg.Tree(u) => u }
  def patchOf(l: String): Option[Patch] = parse(l).collect { case Msg.Patch(p) => p }
  def eventOf(l: String): Option[Event] = parse(l).collect { case Msg.Event(e) => e }
  /** does this line END a connection — an Event.Closed or a Close */
  def closes(l: String): Boolean = parse(l).exists {
    case Msg.Event(Event.Closed) | Msg.Close => true
    case _ => false
  }

  // ---- the document: rendered FROM the schemas, so it cannot drift

  /** the contract a client in any language implements — prose that
   * is fixed, and the JSON Schema of every shape, which is derived */
  def document: String =
    s"""# The frontend protocol, version $version

This file is RENDERED by `okay.ui.Protocol.document` from the derived
schemas of the tree, the events, the patches and the message envelope
(okay-ui's `TestProtocol` fails when it drifts; regenerate with
`OKAY_RENDER=1 sbt okayUiJVM/testOnly okay.ui.TestProtocol`). It is
the contract a thin client implements — in Kotlin, Swift or anything
else — with no dependency on okay: read `docs/protocol/conformance.jsonl`
and reproduce it (specs/frontend.md).

## The conversation

Lines of JSON (one message per line) or CBOR items, the same shapes:

```
client → server   Hello {vocab, version}   first, once
server → client   Tree {ui}                the full tree, then
server → client   Patch {patch}            narrow patches
client → server   Event {event}            what the user did
either            Close
```

- A sum is an object with ONE key, the case name: `{"Text": {...}}`.
  A product is its fields by name, every field present. An
  enumeration is a string from its list (`"h"`/`"v"` for a direction).
  `[T]` is a JSON array of T, `T?` a field that may be `null` or
  absent, `int` a JSON number without a fraction.
- `Hello.vocab` lists the SEMANTIC nodes the client draws itself
  (`form`, `items`, `table`, `tabs`, `modal`). Every other node is
  LOWERED by the server to the layout level before it is sent, so a
  client that claims nothing receives only: `Text`, `Row`, `Column`,
  `Box`, `Image`, `Button`, `Input`, `Check`, `Select`, `Scroll`.
- A server that receives an `Event` before any `Hello` serves the
  client as if it had claimed nothing.
- Patch paths index children in order: `Row`/`Column`/`Box` children,
  `Scroll`'s child at 0, `Form` fields, `Items` items, `Modal`'s body
  at 1. `Reorder` gives the survivors' old indices in new order;
  removals come first (descending), then one reorder, then insertions
  (ascending), then content patches.
- Events name KEYS; the server drops an event whose key is not on the
  tree it last sent — the shown tree is the capability list.
- A `Box` with `weights` divides its main axis by weight; `gap` and
  `pad` are in character units; style is TOKENS a host maps to its
  own idiom.

## The shapes

Rendered from the derived schemas; a name on the right is defined
below, a sum lists its cases with `|`.

```
${describe(Vector(summon[Schema[Msg]], summon[Schema[Ui]], summon[Schema[Event]], summon[Schema[Patch]]))}
```
"""

  /**
   * The shape language: every named type (a product or a sum) on its
   * own line, once, in first-reference order — so a recursive type
   * (Ui holds Vectors of Ui) is a name, not an infinite expansion.
   * A sum's cases are inlined with their fields; a product referenced
   * as a field type is a name.
   */
  def describe(roots: Vector[Schema[?]]): String =
    val out = scala.collection.mutable.LinkedHashMap[String, String]()
    def nameOf(s: Schema[?]): String = s match
      case Schema.SInt | Schema.SLong => "int"
      case Schema.SDouble => "number"
      case Schema.SBool => "bool"
      case Schema.SString | Schema.SChar => "string"
      case Schema.SBytes => "bytes"
      case Schema.SOption(of) => nameOf(of()) + "?"
      case Schema.SList(of) => "[" + nameOf(of()) + "]"
      case Schema.SVector(of) => "[" + nameOf(of()) + "]"
      case iso @ Schema.SIso(u, _, _) => iso.vocabulary match
        case Some(vs) => vs.map(v => Json.write(v)(using u().asInstanceOf[Schema[Any]])).mkString(" | ")
        case None => nameOf(u())
      case p: Schema.SProduct[?] => define(p.name, p); p.name
      case su: Schema.SSum[?] => define(su.name, su); su.name
    def fields(p: Schema.SProduct[?]): String =
      p.fields.map((n, sc) => s"$n: ${nameOf(sc())}").mkString("{", ", ", "}")
    def define(name: String, s: Schema[?]): Unit =
      if !out.contains(name) then
        out(name) = ""   // reserve before recursing: the guard
        out(name) = s match
          case p: Schema.SProduct[?] => s"$name = ${fields(p)}"
          case su: Schema.SSum[?] =>
            s"$name = " + su.cases.map { (cn, sc) => sc() match
              case p: Schema.SProduct[?] => s"$cn ${fields(p)}"
              case other => s"$cn ${nameOf(other)}"
            }.mkString("\n  | ")
          case other => s"$name = ${nameOf(other)}"
    roots.foreach(r => { val _ = nameOf(r) })
    out.values.mkString("\n\n")

  /** the conformance script: a scripted session, one JSON record per
   * line — `{"out": line}` is what the client sends, `{"in": line,
   * "tree": ui}` what it receives and the tree it must hold after
   * applying it. A client that reproduces every `tree` and every
   * `out` speaks the protocol. */
  def conformance: Vector[String] =
    import okay.{!, Writer, through}
    import okay.given
    def view(n: Int): Ui = Ui.Box(Vector(
      Ui.Text(s"count: $n", Style(tone = Tone.Emphasis)),
      Ui.Form(Vector(Ui.Input("", "name", "Name"), Ui.Check(n % 2 == 0, "even", "Even")), "Save", "f"),
      Ui.Items((0 until n).toVector.map(i => Ui.Row(Vector(Ui.Text(s"item $i"), Ui.Button("x", s"del$i")), key = s"i$i")), "list"),
      Ui.Row(Vector(Ui.Button("-", "dec"), Ui.Button("+", "inc", Role.Primary)))),
      Dir.Vertical, gap = 1)
    def update(n: Int, e: Event): Int = e match
      case Event.Pressed("inc") => n + 1
      case Event.Pressed("dec") => n - 1
      case _ => n
    val sent = Vector(hello(Set.empty), Msg.Event(Event.Pressed("inc")), Msg.Event(Event.Pressed("inc")),
      Msg.Event(Event.Edited("name", "ada")), Msg.Event(Event.Pressed("dec")), Msg.Event(Event.Closed))
      .map(line)
    val (received, _) = !.run(Writer.run(through(Writer.of(sent.toList))(Wire.serve(0)(view)(update))))
    // interleave as the client sees them: hello, tree, then each event and its patches
    var tree: Ui = Ui.Text("")
    def record(l: String): String =
      Protocol.parse(l) match
        case Some(Msg.Tree(u)) => tree = u
        case Some(Msg.Patch(p)) => tree = Ui.patch(tree, p)
        case _ => ()
      s"""{"in":$l,"tree":${Json.write(tree)}}"""
    val out = sent.map(l => s"""{"out":$l}""")
    // the server answers the hello with the tree, then each event with its patches;
    // pair them by replaying: everything the server said, in order, after the hello
    val ins = received.toVector.map(record)
    // the first out (hello) precedes every in; the remaining outs each
    // precede the patches they caused — but the pairing is the
    // client's business, and a client applies ins in order regardless
    out.take(1) ++ ins.take(1) ++ out.drop(1) ++ ins.drop(1)
}
