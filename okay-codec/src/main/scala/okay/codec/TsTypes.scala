package okay.codec

/**
 * TypeScript declarations READ into Scala types (typescript-types T3,
 * specs/typescript-types.md): the direction where a type is written first
 * in TypeScript. `TsTypes.scala(source, pkg)` answers Scala source —
 * `final case class … derives Schema`, `enum … derives Schema`, `type` —
 * whose `Schema`s make okay's JSON codec write exactly the values those
 * TypeScript types describe.
 *
 * It reads the declaration subset that describes DATA:
 *  - `interface X { a: T; b?: T }` -> a case class (an optional field is
 *    an `Option` defaulting to `None`);
 *  - `type X = { A: A } | { B: B }` -> an enum whose cases are the named
 *    interfaces — the shape okay's JSON codec gives a sum (`Stubs.typescript`
 *    writes it); `type X = A | B` whose interfaces carry `type: "A"` is read
 *    the same way (the shape okay-py's wire gives a sum);
 *  - `T[]`, `Array<T>` -> `Vector[T]`; `T | null` -> `Option[T]`;
 *  - `string`, `number` (a `Double`), `boolean`, and the aliases
 *    `Stubs.typescript` names — `Int`, `Long`, `Char`, `BigIntDigits`,
 *    `Base64` — back to the exact Scala leaf, so Scala -> TypeScript ->
 *    Scala is exact;
 *  - `type X = T` otherwise -> a Scala type alias.
 *
 * Anything else — generics, intersections, functions, tuples, `Record`,
 * string-literal unions (whose JSON shape is not a Scala enum's) — is
 * REFUSED by name and line, never guessed. Comments, `import`s and
 * `export`/`declare` modifiers are skipped. A parser of this subset, not
 * of TypeScript: the installed TypeScript (7, the native port) ships no
 * compiler API to call, and a subset read exactly is better than a
 * language read approximately.
 */
object TsTypes:

  enum TsType:
    case Named(name: String, args: Vector[TsType])
    case Arr(of: TsType)
    case Union(parts: Vector[TsType])
    case Obj(fields: Vector[Field])
    case Lit(value: String)
    case Null

  final case class Field(name: String, tpe: TsType, optional: Boolean)

  enum Decl:
    case Interface(name: String, fields: Vector[Field])
    case Alias(name: String, tpe: TsType)

  // ---------------------------------------------------------------- tokens

  private enum Tok:
    case Id(s: String)
    case Str(s: String)
    case Num(s: String)
    case Sym(s: String)

  private final case class At(tok: Tok, line: Int)

  private def tokens(src: String): Either[String, Vector[At]] =
    val out = Vector.newBuilder[At]
    var i = 0
    var line = 1
    var bad: Option[String] = None
    def peek(k: Int) = if i + k < src.length then src.charAt(i + k) else '\u0000'
    while bad.isEmpty && i < src.length do
      val c = src.charAt(i)
      if c == '\n' then { line += 1; i += 1 }
      else if c.isWhitespace then i += 1
      else if c == '/' && peek(1) == '/' then
        while i < src.length && src.charAt(i) != '\n' do i += 1
      else if c == '/' && peek(1) == '*' then
        i += 2
        while i < src.length && !(src.charAt(i) == '*' && peek(1) == '/') do
          if src.charAt(i) == '\n' then line += 1
          i += 1
        i += 2
      else if c.isLetter || c == '_' || c == '$' then
        val start = i
        while i < src.length && (src.charAt(i).isLetterOrDigit || src.charAt(i) == '_' || src.charAt(i) == '$') do i += 1
        out += At(Tok.Id(src.substring(start, i)), line)
      else if c.isDigit then
        val start = i
        while i < src.length && (src.charAt(i).isLetterOrDigit || src.charAt(i) == '.') do i += 1
        out += At(Tok.Num(src.substring(start, i)), line)
      else if c == '"' || c == '\'' then
        val q = c
        val sb = StringBuilder()
        i += 1
        while i < src.length && src.charAt(i) != q do
          if src.charAt(i) == '\\' && i + 1 < src.length then { sb += src.charAt(i + 1); i += 2 }
          else { sb += src.charAt(i); i += 1 }
        i += 1
        out += At(Tok.Str(sb.toString), line)
      else if "{}[]()<>|&;:?,=.*".indexOf(c) >= 0 then
        out += At(Tok.Sym(c.toString), line)
        i += 1
      else bad = Some(s"line $line: unexpected '$c'")
    bad.toLeft(out.result())

  // ---------------------------------------------------------------- parser

  private final class Refused(val why: String) extends RuntimeException(why)

  private final class Parser(ts: Vector[At]):
    private var i = 0
    private def line = if i < ts.length then ts(i).line else ts.lastOption.fold(1)(_.line)
    private def refuse(why: String): Nothing = throw Refused(s"line $line: $why")
    private def peek: Option[Tok] = ts.lift(i).map(_.tok)
    private def next(): Tok = { val t = ts(i).tok; i += 1; t }
    private def sym(s: String): Boolean = peek.contains(Tok.Sym(s))
    private def id(s: String): Boolean = peek.contains(Tok.Id(s))
    private def expect(s: String): Unit =
      if sym(s) then i += 1 else refuse(s"expected '$s', found ${peek.fold("the end")(describe)}")
    private def describe(t: Tok): String = t match
      case Tok.Id(s) => s"'$s'"
      case Tok.Str(s) => s"\"$s\""
      case Tok.Num(s) => s
      case Tok.Sym(s) => s"'$s'"
    private def name(): String = next() match
      case Tok.Id(s) => s
      case other => refuse(s"expected a name, found ${describe(other)}")

    def decls(): Vector[Decl] =
      val out = Vector.newBuilder[Decl]
      while i < ts.length do
        if id("export") || id("declare") then i += 1
        else if id("import") then
          while i < ts.length && !sym(";") do i += 1
          if sym(";") then i += 1
        else if id("interface") then
          i += 1
          val n = name()
          if sym("<") then refuse(s"interface $n is generic; generics are not read")
          if id("extends") then refuse(s"interface $n extends another; inheritance is not read")
          out += Decl.Interface(n, obj())
        else if id("type") then
          i += 1
          val n = name()
          if sym("<") then refuse(s"type $n is generic; generics are not read")
          expect("=")
          out += Decl.Alias(n, union())
          if sym(";") then i += 1
        else if sym(";") then i += 1
        else refuse(s"only interfaces and type aliases describe data; found ${peek.fold("the end")(describe)}")
      out.result()

    private def obj(): Vector[Field] =
      expect("{")
      val fs = Vector.newBuilder[Field]
      while !sym("}") do
        if id("readonly") && ts.lift(i + 1).exists(a => a.tok != Tok.Sym(":") && a.tok != Tok.Sym("?")) then i += 1
        val n = next() match
          case Tok.Id(s) => s
          case Tok.Str(s) => s
          case other => refuse(s"expected a field name, found ${describe(other)}")
        if sym("(") then refuse(s"field $n is a method; functions are not data")
        val optional = sym("?")
        if optional then i += 1
        expect(":")
        fs += Field(n, union(), optional)
        if sym(";") || sym(",") then i += 1
      expect("}")
      fs.result()

    private def union(): TsType =
      if sym("|") then i += 1
      val first = postfix()
      if sym("&") then refuse("an intersection (&) is not read")
      if !sym("|") then first
      else
        val parts = Vector.newBuilder[TsType]
        parts += first
        while sym("|") do
          i += 1
          parts += postfix()
        TsType.Union(parts.result())

    private def postfix(): TsType =
      var t = primary()
      while sym("[") do
        i += 1
        expect("]")
        t = TsType.Arr(t)
      t

    private def primary(): TsType = peek match
      case Some(Tok.Sym("{")) => TsType.Obj(obj())
      case Some(Tok.Sym("(")) =>
        i += 1
        val t = union()
        expect(")")
        if sym("=") then refuse("a function type is not data")
        t
      case Some(Tok.Sym("[")) => refuse("a tuple is not read")
      case Some(Tok.Str(s)) => { i += 1; TsType.Lit(s) }
      case Some(Tok.Num(n)) => refuse(s"a number literal type ($n) is not read")
      case Some(Tok.Id("null")) => { i += 1; TsType.Null }
      case Some(Tok.Id(n)) =>
        i += 1
        val args =
          if !sym("<") then Vector.empty
          else
            i += 1
            val as = Vector.newBuilder[TsType]
            as += union()
            while sym(",") do { i += 1; as += union() }
            expect(">")
            as.result()
        TsType.Named(n, args)
      case other => refuse(s"expected a type, found ${other.fold("the end")(describe)}")

  /** the declarations in `source`, or where and why they were refused */
  def parse(source: String): Either[String, Vector[Decl]] =
    tokens(source).flatMap { ts =>
      try Right(Parser(ts).decls())
      catch case r: Refused => Left(r.why)
    }

  // ------------------------------------------------------------- Scala out

  /** the aliases `Stubs.typescript` writes for Scala leaves */
  private val leaves = Map("Int" -> "Int", "Long" -> "Long", "Char" -> "Char",
    "BigIntDigits" -> "BigInt", "Base64" -> "Array[Byte]")

  private val scalaKeywords = Set("abstract", "case", "catch", "class", "def", "do", "else", "enum", "export",
    "extends", "false", "final", "finally", "for", "given", "if", "implicit", "import", "lazy", "match", "new",
    "null", "object", "override", "package", "private", "protected", "return", "sealed", "super", "then",
    "throw", "trait", "true", "try", "type", "val", "var", "while", "with", "yield")

  private def ident(n: String): String =
    if n.matches("[A-Za-z_][A-Za-z0-9_]*") && !scalaKeywords(n) then n else s"`$n`"

  /**
   * Scala source for the data declarations in `source`, in package `pkg`,
   * or the first refusal by name.
   */
  def scala(source: String, pkg: String): Either[String, String] =
    parse(source).flatMap(ds => render(ds, pkg))

  def render(decls: Vector[Decl], pkg: String): Either[String, String] =
    val interfaces = decls.collect { case d: Decl.Interface => d.name -> d }.toMap
    val aliasNames = decls.collect { case Decl.Alias(n, _) => n }.toSet
    // an alias `type Int = number;` that Stubs wrote is a leaf, not a type
    def isLeafAlias(d: Decl): Boolean = d match
      case Decl.Alias(n, TsType.Named(_, _)) => leaves.contains(n)
      case _ => false

    def scalaType(t: TsType, where: String): String = t match
      case TsType.Named("string", _) => "String"
      case TsType.Named("number", _) => "Double"
      case TsType.Named("boolean", _) => "Boolean"
      case TsType.Named(n, Vector()) if leaves.contains(n) => leaves(n)
      case TsType.Named("Array", Vector(of)) => s"Vector[${scalaType(of, where)}]"
      case TsType.Named("Record" | "Map", _) => throw Refused(s"$where: a map is not read; a Schema has no map case")
      case TsType.Named(n, Vector()) if interfaces.contains(n) || aliasNames(n) => ident(n)
      case TsType.Named(n, Vector()) => throw Refused(s"$where: '$n' is not declared here")
      case TsType.Named(n, _) => throw Refused(s"$where: '$n<…>' is generic; generics are not read")
      case TsType.Arr(of) => s"Vector[${scalaType(of, where)}]"
      case TsType.Union(parts) if parts.contains(TsType.Null) =>
        parts.filterNot(_ == TsType.Null) match
          case Vector(one) => s"Option[${scalaType(one, where)}]"
          case _ => throw Refused(s"$where: a union of several types besides null is not a field type")
      case TsType.Union(_) => throw Refused(s"$where: a union is read only as a whole `type` (a sum)")
      case TsType.Obj(_) => throw Refused(s"$where: an inline object type needs a name (an interface)")
      case TsType.Lit(v) => throw Refused(s"$where: a literal type (\"$v\") is not read")
      case TsType.Null => throw Refused(s"$where: null alone is not a type")

    def params(fields: Vector[Field], owner: String, skip: Set[String]): String =
      fields.filterNot(f => skip(f.name)).map { f =>
        val t = scalaType(f.tpe, s"$owner.${f.name}")
        if f.optional then
          if t.startsWith("Option[") then s"${ident(f.name)}: $t = None"
          else s"${ident(f.name)}: Option[$t] = None"
        else s"${ident(f.name)}: $t"
      }.mkString(", ")

    /** the cases of a sum: `{ A: A } | …` (JSON's shape) or `A | B` whose
     * interfaces carry `type: "A"` (the wire's) — each the name of an interface */
    def sumCases(parts: Vector[TsType]): Option[Vector[(String, Decl.Interface, Set[String])]] =
      val json = parts.map {
        case TsType.Obj(Vector(Field(c, TsType.Named(i, Vector()), false))) if interfaces.get(i).exists(_ => c == i) =>
          Some((c, interfaces(i), Set.empty[String]))
        case _ => None
      }
      val wire = parts.map {
        case TsType.Named(i, Vector()) => interfaces.get(i).flatMap { d =>
          d.fields.headOption.collect { case Field("type", TsType.Lit(c), false) if c == i => (c, d, Set("type")) }
        }
        case _ => None
      }
      if json.forall(_.isDefined) then Some(json.flatten)
      else if wire.forall(_.isDefined) then Some(wire.flatten)
      else None

    try
      val sums = decls.collect { case Decl.Alias(n, TsType.Union(ps)) if !ps.contains(TsType.Null) => n -> ps }
      val cases = sums.map((n, ps) => n -> sumCases(ps).getOrElse(throw Refused(
        s"type $n: a union is read as a sum when each member is { Case: Case } or an interface whose first field is type: \"Case\"")))
      val inlined = cases.flatMap(_._2.map(_._2.name)).toSet
      val sumMap = cases.toMap
      val body = decls.flatMap {
        case d if isLeafAlias(d) => None
        case Decl.Interface(n, _) if inlined(n) => None
        case Decl.Interface(n, fs) => Some(s"final case class ${ident(n)}(${params(fs, n, Set.empty)}) derives Schema")
        case Decl.Alias(n, _) if sumMap.contains(n) =>
          val cs = sumMap(n).map { (c, d, skip) =>
            val ps = params(d.fields, s"$n.$c", skip)
            if ps.isEmpty then s"  case ${ident(c)}" else s"  case ${ident(c)}($ps)"
          }
          Some(s"enum ${ident(n)} derives Schema:\n${cs.mkString("\n")}")
        case Decl.Alias(n, t) => Some(s"type ${ident(n)} = ${scalaType(t, n)}")
      }
      Right(s"package $pkg\n\nimport okay.codec.Schema\n\n// Generated by okay.codec.TsTypes from TypeScript declarations: regenerate it, do not edit it.\n\n" +
        body.mkString("\n\n") + "\n")
    catch case r: Refused => Left(r.why)
