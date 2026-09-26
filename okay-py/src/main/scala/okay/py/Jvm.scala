package okay.py

import okay.{!, Free}
import okay.codec.Schema
import scala.jdk.CollectionConverters.*

/**
 * The JVM languages' side of the callbacks (foreign-one-ops,
 * specs/foreign-one.md Decision 19): the SAME `Foreign.callback`s that
 * serve Python, TypeScript, Go, Rust, Haskell and R answer a Frege or
 * Clojure program, so an operation of the caller's is declared once, in
 * Scala, for every language.
 *
 * {{{
 * val cbs = Foreign.callbacks(priceOf, discount)
 * Frege.run[Reader % Prices, Double](Shop.quote("tea", 3L), calls = Jvm.calls(cbs))
 * }}}
 *
 * A program performs `okay.Foreign.Call(name, arg)` — what the generated
 * bindings make: `Jvm.frege` writes a Frege module, one typed operation
 * per callback, and `Jvm.clojure` a Clojure namespace, one `defn` each, as
 * `Hs.ops` writes Haskell's. The walker hands the call to `calls`, which
 * runs the callback under the caller's handlers. The argument is decoded
 * by the callback's own Schema, so a wrong one is refused by name — in
 * Clojure at run time; in Frege the generated type refuses it first.
 */
object Jvm:

  /** the callbacks, as the JVM walker asks them */
  def calls[F[+_]](cbs: Foreign.Callbacks[F]): okay.Foreign.Calls[F] = new okay.Foreign.Calls[F]:
    def names: Seq[String] = cbs.names
    def apply(name: String, arg: AnyRef): Option[AnyRef ! F] = cbs.get(name).map { cb =>
      value(arg) match
        case Left(what) => refused(name, what)
        case Right(v) => cb.run(Vector(v)).flatMap {
          case Right(answer) => okay.pure[F, AnyRef](jvm(answer))
          case Left(c) => refused(name, s"${c.kind}: ${c.message}")
        }
    }

  private def refused[F[+_]](name: String, why: String): AnyRef ! F =
    Free.delay(() => throw IllegalArgumentException(s"the callback '$name' refused its argument: $why"))

  /** how deep a list may nest, either way: a limit written down rather than
   * a stack the value's shape decides (Arrow C++'s kMaxNestingDepth) */
  val MaxNesting = 64

  /** a JVM value as the wire's: what a Frege or Clojure program passes */
  def value(x: Any, depth: Int = 0): Either[String, PyValue] = x match
    case _ if depth > MaxNesting => Left(s"a value nested past $MaxNesting levels")
    case null => Right(PyValue.PyNone)
    case b: java.lang.Boolean => Right(PyValue.Bool(b))
    case n: (java.lang.Long | java.lang.Integer | java.lang.Short | java.lang.Byte) => Right(PyValue.I64(n.longValue))
    case n: (java.lang.Double | java.lang.Float) => Right(PyValue.F64(n.doubleValue))
    case s: String => Right(PyValue.Str(s))
    case s: java.lang.Iterable[?] => all(s.asScala, depth + 1)
    // a Java array (Frege's `JArray`, a `long[]` or a `String[]`): its elements, boxed
    case a: Array[?] => all((0 until java.lang.reflect.Array.getLength(a)).map(java.lang.reflect.Array.get(a, _)), depth + 1)
    case s: Iterable[?] => all(s, depth + 1)
    case other => Left(s"a ${other.getClass.getName} has no wire value")

  private def all(xs: Iterable[?], depth: Int): Either[String, PyValue] =
    val out = Vector.newBuilder[PyValue]
    val it = xs.iterator
    var err: Option[String] = None
    while err.isEmpty && it.hasNext do value(it.next(), depth) match
      case Right(v) => out += v
      case Left(e) => err = Some(e)
    err.toLeft(PyValue.Arr(out.result()))

  /** a wire value as the JVM's: what the program's continuation is handed —
   * boxed numbers, strings, a `java.util.List` for a list */
  def jvm(v: PyValue, depth: Int = 0): AnyRef = v match
    case _ if depth > MaxNesting => throw IllegalArgumentException(s"an answer nested past $MaxNesting levels")
    case PyValue.PyNone | PyValue.NA(_) => null
    case PyValue.Bool(b) => java.lang.Boolean.valueOf(b)
    case PyValue.I64(n) => java.lang.Long.valueOf(n)
    case PyValue.BigI(n) => n.bigInteger
    case PyValue.F64(d) => java.lang.Double.valueOf(d)
    case PyValue.Str(s) => s
    case PyValue.Bytes(b) => b
    case PyValue.Arr(xs) => xs.map(jvm(_, depth + 1)).asJava
    case PyValue.Dict(kv) => kv.map((k, x) => k -> jvm(x, depth + 1)).toMap.asJava
    case PyValue.Ref(r) => r

  /** the Frege type of a Schema, where the JVM value `jvm` answers is that
   * type's: integers are `Long` (a boxed `java.lang.Long`), doubles
   * `Double`, and so on; None where no Frege type holds it */
  def fregeType(s: Schema[?]): Option[String] = s match
    case Schema.SInt | Schema.SLong => Some("Long")
    case Schema.SDouble => Some("Double")
    case Schema.SBool => Some("Bool")
    case Schema.SString => Some("String")
    case i: Schema.SIso[?, ?] => fregeType(i.under())
    // a list crosses from Frege as a Java array: `arrayFromList [1, 2]`
    case l: Schema.SList[?] => fregeType(l.of()).map(t => s"JArray $t")
    case v: Schema.SVector[?] => fregeType(v.of()).map(t => s"JArray $t")
    case _ => None

  /** `price_of` -> `priceOf`: a Frege or Clojure-free function name */
  private def camel(op: String): String =
    val c = Hs.constructor(op)
    c.head.toLower.toString + c.tail

  /**
   * The Frege module `module` declaring these callbacks as operations, each
   * typed by its argument and answer:
   *
   * {{{
   * priceOf :: String -> Operation Double
   * priceOf a = callOp "price_of" a
   * }}}
   *
   * A callback whose Schemas have no Frege type is refused by name: a
   * generated module that types it `a` would accept anything, and the
   * point of generating it is that Frege's checker refuses a wrong call.
   */
  def frege[F[+_]](module: String, cbs: Foreign.Callbacks[F]): String =
    val ops = cbs.all.map { c =>
      val typed = c.types.flatMap((a, r) => fregeType(a).zip(fregeType(r)))
      val (a, r) = typed.getOrElse(throw IllegalArgumentException(
        s"the callback '${c.name}' has no Frege type: " +
          c.types.fold("made without its Schemas")((a, r) => s"its Schemas are $a and $r")))
      val fn = camel(c.name)
      s"""--- ${c.name}: $a -> $r
         |$fn :: $a -> Operation $r
         |$fn a = callOp "${c.name}" a""".stripMargin
    }
    s"""--- Generated by okay.py.Jvm from the Scala callbacks: regenerate it, do not edit it.
       |module $module where
       |
       |import okay.frege.Prog (Operation)
       |
       |private pure native callOp okay.frege.Ops.call {a, b} :: String -> a -> Operation b
       |
       |${ops.mkString("\n\n")}
       |""".stripMargin

  /** the Clojure namespace `ns` declaring these callbacks, one `defn` each
   * making the operation `ok/perform` runs */
  def clojure[F[+_]](ns: String, cbs: Foreign.Callbacks[F]): String =
    val defns = cbs.all.map { c =>
      def named(s: Schema[?]) = fregeType(s).getOrElse(s.toString)
      val doc = c.types.fold(c.name)((a, r) => s"${c.name}: ${named(a)} -> ${named(r)}")
      s"""(defn ${c.name.replace('_', '-')}
         |  "$doc"
         |  [a] (okay.clojure.Ops/call "${c.name}" a))""".stripMargin
    }
    s""";; Generated by okay.py.Jvm from the Scala callbacks: regenerate it, do not edit it.
       |(ns $ns)
       |
       |${defns.mkString("\n\n")}
       |""".stripMargin
