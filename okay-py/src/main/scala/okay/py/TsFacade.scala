package okay.py

import java.nio.file.{Files, Path}
import okay.codec.TsTypes
import okay.codec.TsTypes.TsType

/**
 * A TypeScript module behind a GENERATED typed Scala object
 * (typescript-types T7, specs/typescript-types.md): the TypeScript twin of
 * `PyFacade`, for Scala and TypeScript both on the backend.
 *
 * The TYPES come from the TypeScript compiler, not from a guess:
 * `declarations` runs `tsc --declaration --emitDeclarationOnly` on the
 * module, so a function whose source left its answer's type to inference
 * is declared with the type tsc inferred. `render` reads those
 * declarations (`TsTypes.parseModule`) and writes Scala: the module's own
 * data types as T3 writes them (`case class`/`enum … derives Schema`), and
 * an object with one method per exported function, each calling it on the
 * TypeScript worker through `Ts.fn`, in the JSON codec's shape.
 *
 *  - a `Promise<T>` answers `T`: the worker awaits it;
 *  - `unknown`, `any` and `void` are TYPE PARAMETERS (`Out: Schema` for the
 *    answer, `A: ToPy` for an argument), as in PyFacade: the facade
 *    guesses nothing;
 *  - an optional parameter is left out, and the method's comment says so;
 *  - a function it cannot type (a generic, a callback parameter, a
 *    `Prog<T>` program) is a COMMENT saying why, never a guessed method;
 *  - a type the module imports (`import type { Order } from "./model.ts"`,
 *    typically written from Scala by `Stubs.typescript`) is taken to be in
 *    scope; `imports` are the Scala import lines that put it there.
 *
 * {{{
 * sbt "okayPy/runMain okay.py.TsFacade <dir> shop Shop my.pkg my.model.*"
 * }}}
 */
object TsFacade:

  /** the declarations tsc writes for `dir/module.ts`, or its complaint */
  def declarations(dir: Path, module: String, tsc: String = "tsc"): Either[String, String] =
    val out = Files.createTempDirectory("okay-ts-facade")
    val p = ProcessBuilder(tsc, "--declaration", "--emitDeclarationOnly", "--strict", "--skipLibCheck",
      "--allowImportingTsExtensions", "--target", "es2022", "--module", "nodenext",
      "--outDir", out.toString, s"$module.ts").directory(dir.toFile).redirectErrorStream(true).start()
    val said = String(p.getInputStream.readAllBytes(), "UTF-8")
    val emitted = out.resolve(s"$module.d.ts")
    if p.waitFor() == 0 && Files.exists(emitted) then Right(Files.readString(emitted))
    else Left(s"tsc refused $module.ts:\n$said")

  private val keywords = Set("abstract", "case", "catch", "class", "def", "do", "else", "enum", "export",
    "extends", "false", "final", "finally", "for", "given", "if", "implicit", "import", "lazy", "match",
    "new", "null", "object", "override", "package", "private", "protected", "return", "sealed", "super",
    "then", "throw", "trait", "true", "try", "type", "val", "var", "while", "with", "yield")

  private def ident(n: String): String = if keywords(n) then s"`$n`" else n

  /** a type as TypeScript writes it, for the method's comment */
  private def show(t: TsType): String = t match
    case TsType.Named(n, Vector()) => n
    case TsType.Named(n, as) => s"$n<${as.map(show).mkString(", ")}>"
    case TsType.Arr(of @ TsType.Union(_)) => s"(${show(of)})[]"
    case TsType.Arr(of) => s"${show(of)}[]"
    case TsType.Union(ps) => ps.map(show).mkString(" | ")
    case TsType.Obj(fs) => fs.map(f => s"${f.name}${if f.optional then "?" else ""}: ${show(f.tpe)}").mkString("{ ", "; ", " }")
    case TsType.Lit(v) => s"\"$v\""
    case TsType.Null => "null"

  private def isOpen(t: TsType): Boolean = t match
    case TsType.Named("unknown" | "any" | "void" | "undefined", Vector()) => true
    case _ => false

  /** a method, or why there is none */
  private final case class Method(tparams: Vector[String], params: Vector[String], args: Vector[String],
                                  out: String, needsSchema: Boolean, needsToPy: Boolean)

  private def method(fn: TsTypes.Fn, known: Set[String]): Either[String, Method] =
    val required = fn.params.filterNot(_.optional)
    val answer = fn.returns match
      case TsType.Named("Promise", Vector(t)) => t
      case t => t
    answer match
      case TsType.Named("Prog", _) => return Left("a program (Prog<T>): call it with Ts.program, offering its callbacks")
      case _ => ()
    if required.size > 4 then return Left(s"${required.size} required parameters; call it with Ts.fn directly")
    val tparams = Vector.newBuilder[String]
    val outT =
      if isOpen(answer) then { tparams += "Out: Schema"; Right("Out") }
      else TsTypes.scalaType(answer, known, s"${fn.name}'s answer")
    val params = required.zipWithIndex.map { (p, i) =>
      if isOpen(p.tpe) then
        tparams += s"A${i + 1}: ToPy"
        Right(s"${ident(p.name)}: A${i + 1}")
      else TsTypes.scalaType(p.tpe, known, s"${fn.name}(${p.name})").map(t => s"${ident(p.name)}: $t")
    }
    for
      out <- outT
      ps <- params.foldLeft[Either[String, Vector[String]]](Right(Vector.empty))((acc, p) => acc.flatMap(v => p.map(v :+ _)))
    yield
      val tps = tparams.result()
      Method(tps, ps, required.map(p => ident(p.name)), out,
        needsSchema = tps.exists(_.endsWith(": Schema")), needsToPy = tps.exists(_.endsWith(": ToPy")))

  /** the Scala source of an object calling `module`'s exported functions */
  def render(obj: String, pkg: String, module: String, dts: String, imports: Seq[String] = Nil): Either[String, String] =
    for
      m <- TsTypes.parseModule(dts)
      data <- TsTypes.renderData(m.decls, m.imported.toSet)
    yield
      val known = m.imported.toSet ++ m.decls.map {
        case TsTypes.Decl.Interface(n, _) => n
        case TsTypes.Decl.Alias(n, _) => n
      }
      val methods = m.functions.map(_.map(fn => fn -> method(fn, known)))
      val made = methods.collect { case Right((_, Right(mm))) => mm }
      // what the methods and the data will use, so no import is unused (an
      // unused import is a warning in the user's build)
      val needsSchema = data.nonEmpty || made.exists(_.needsSchema)
      val needsToPy = made.exists(_.needsToPy)
      val b = StringBuilder()
      b ++= s"package $pkg\n\n"
      if made.nonEmpty then b ++= "import okay.!\n"
      if needsSchema then b ++= "import okay.codec.Schema\n"
      val py = (if made.nonEmpty then Vector("Condition", "ForeignEval", "Ts") else Vector.empty) ++ Option.when(needsToPy)("ToPy")
      if py.nonEmpty then b ++= s"import okay.py.{${py.sorted.mkString(", ")}}\n"
      imports.foreach(i => b ++= s"import $i\n")
      b ++= s"\n// Generated by okay.py.TsFacade from the TypeScript module `$module`: regenerate it, do not edit it.\n"
      data.foreach(d => b ++= s"\n$d\n")
      b ++= s"\nobject $obj:\n"
      for f <- methods do
        b ++= "\n"
        f match
          case Left(u) =>
            b ++= s"  // ${u.name}: not generated — ${u.why}\n"
          case Right((fn, Left(why))) =>
            b ++= s"  // ${fn.name}: not generated — $why\n"
          case Right((fn, Right(mm))) =>
            val shown = fn.params.map(p => s"${p.name}${if p.optional then "?" else ""}: ${show(p.tpe)}").mkString(", ")
            val left = fn.params.filter(_.optional).map(_.name)
            b ++= "  /**\n"
            b ++= s"   * TypeScript: `${fn.name}($shown): ${show(fn.returns)}`\n"
            if left.nonEmpty then b ++= s"   * Left out, being optional: ${left.mkString(", ")}.\n"
            b ++= "   */\n"
            val tpClause = if mm.tparams.isEmpty then "" else mm.tparams.mkString("[", ", ", "]")
            b ++= s"  def ${ident(fn.name)}$tpClause(${mm.params.mkString(", ")}): Either[Condition, ${mm.out}] ! ForeignEval =\n"
            b ++= s"    Ts.fn[${mm.out}](\"$module:${fn.name}\")(${mm.args.mkString(", ")})\n"
      b.result()

  /** `dir module Object package [scala imports…]`: prints the source */
  def main(args: Array[String]): Unit = args.toList match
    case dir :: module :: obj :: pkg :: imports =>
      declarations(Path.of(dir), module).flatMap(render(obj, pkg, module, _, imports)) match
        case Right(src) => print(src)
        case Left(why) => System.err.println(why); sys.exit(1)
    case _ =>
      System.err.println("usage: TsFacade <dir> <module> <Object> <package> [scala imports…]")
      sys.exit(2)
