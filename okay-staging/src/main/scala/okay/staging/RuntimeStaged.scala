package okay.staging

import okay.codec.{Json, JsonCodec, Schema, Staged}

import scala.quoted.*
import scala.quoted.staging.{Compiler, run}

/**
 * Run-time staging: the staged fold over a `Schema` VALUE
 * (specs/codecs.md, staged-runtime).
 *
 * `Staged.json[A]` folds a type's shape at COMPILE time and emits
 * straight-line code. A schema that exists only at run time — a
 * Postgres composite read from the catalog, a tool's parameters as an
 * MCP server declared them, a JSON frame handed over by R or Python —
 * has no type for that macro to see, and until now only the
 * interpreter (`Json.encode/decode`, a match per node per value).
 * This is the same generator over the schema as a VALUE, run inside
 * `scala.quoted.staging.run`: the shape decides the code at
 * generation time, and the generated code reaches the value's own
 * functions (`make`, `parts`, `caseOf`, an iso's `to`/`from`, the
 * defaults) through a table of the schema's nodes handed to it once.
 *
 * OPTIONAL, BY CONSTRUCTION. This is its own module with the compiler
 * dependency; nothing else in okay depends on it, and a program that
 * does not add it has no compiler on its classpath. A program that
 * does add it can still switch it off at launch — `-Dokay.staging=off`
 * or `OKAY_STAGING=off` — and every door here then answers the
 * interpreter without ever creating a `Compiler`. And a door never
 * throws for want of staging: a generation that fails (a shape the
 * generator does not know, a compiler that cannot run) answers the
 * interpreter and records why in `lastFailure`.
 *
 * WHAT IT COSTS, honestly: the Scala 3 compiler in the running
 * process (`scala3-staging` and its compiler jar, tens of MB, JVM
 * only — this module does not cross to JS or Native, which is why it
 * is not okay-codec), one compilation per schema at first use (the
 * benchmark prints it), and casts — a run-time schema is erased, so
 * the generated code bridges `Any` to each node's type in exactly one
 * place, `Unsafe`, each cast licensed by the node kind the generator
 * read when it emitted the call.
 *
 * WHAT IT PROMISES: the generated codec agrees with the interpreter —
 * encode byte for byte, decode Left for Left with the fold's own
 * refusal words on every cold path — over the whole node vocabulary
 * (`TestRuntimeStaged`). A type met again inside itself delegates to
 * the fold, as the compile-time generator does.
 */
object RuntimeStaged {

  /** the launch switch: absent or anything but off/false/0 is ON */
  def enabled: Boolean = forced.getOrElse {
    val v = sys.props.get("okay.staging").orElse(sys.env.get("OKAY_STAGING")).map(_.trim.toLowerCase)
    !v.exists(Set("off", "false", "0", "no").contains)
  }
  @volatile private var forced: Option[Boolean] = None
  /** for a test, or a program that decides at run time: `Some(false)`
   * makes every door the interpreter, `None` returns to the switch */
  def force(on: Option[Boolean]): Unit = forced = on

  /** the last generation that fell back to the interpreter, and why */
  @volatile var lastFailure: Option[(Schema[?], Throwable)] = None

  private lazy val compiler: Compiler = Compiler.make(getClass.getClassLoader)
  private val cache = new java.util.concurrent.ConcurrentHashMap[AnyRef, JsonCodec[Any]]()
  private val lock = new Object

  /** the interpreter, as a codec — what a door answers when staging is
   * off or refused */
  def interpreted[A](s: Schema[A]): JsonCodec[A] = new JsonCodec[A]:
    def encode(a: A): String = Json.encode(s)(a)
    def decode(j: Json): Either[String, A] = Json.decode(s)(j)

  /** the staged codec for a schema value — generated once per schema
   * (by identity), the interpreter when staging is off or the
   * generation fails */
  def json[A](s: Schema[A]): JsonCodec[A] =
    if !enabled then interpreted(s)
    else
      val key = IdentityKey(s)
      val got = cache.get(key)
      if got != null then Unsafe.codec[A](got)
      else lock.synchronized {
        val again = cache.get(key)
        if again != null then Unsafe.codec[A](again)
        else
          try
            val made = generate(s)
            cache.put(key, made)
            Unsafe.codec[A](made)
          catch case e: Throwable =>
            lastFailure = Some((s, e))
            interpreted(s)
      }

  /** whether `s` has a generated codec already (a test's question) */
  def isStaged(s: Schema[?]): Boolean = cache.containsKey(IdentityKey(s))

  /** schemas are case classes over functions, so `==` is already
   * identity in practice; this makes it identity by contract */
  private final class IdentityKey(val s: Schema[?]):
    override def hashCode: Int = System.identityHashCode(s)
    override def equals(o: Any): Boolean = o match
      case k: IdentityKey => k.s eq s
      case _ => false

  /**
   * The one place a run-time schema's erasure is bridged. Every cast
   * here is licensed by the node kind the generator read when it
   * emitted the call: `int(v)` is emitted only under an `SInt` node,
   * `product(s)` only for an index the generator recorded as an
   * `SProduct`. Nothing outside this object casts.
   */
  object Unsafe {
    def product(s: Schema[?]): Schema.SProduct[Any] = s.asInstanceOf[Schema.SProduct[Any]]
    def sum(s: Schema[?]): Schema.SSum[Any] = s.asInstanceOf[Schema.SSum[Any]]
    def iso(s: Schema[?]): Schema.SIso[Any, Any] = s.asInstanceOf[Schema.SIso[Any, Any]]
    def any(s: Schema[?]): Schema[Any] = s.asInstanceOf[Schema[Any]]
    def int(v: Any): Int = v.asInstanceOf[Int]
    def long(v: Any): Long = v.asInstanceOf[Long]
    def double(v: Any): Double = v.asInstanceOf[Double]
    def bool(v: Any): Boolean = v.asInstanceOf[Boolean]
    def string(v: Any): String = v.asInstanceOf[String]
    def option(v: Any): Option[Any] = v.asInstanceOf[Option[Any]]
    def list(v: Any): List[Any] = v.asInstanceOf[List[Any]]
    def vector(v: Any): Vector[Any] = v.asInstanceOf[Vector[Any]]
    def codec[A](c: JsonCodec[Any]): JsonCodec[A] = c.asInstanceOf[JsonCodec[A]]
    /** the fold, for a cold path or a node the generator delegates */
    def encodeAny(s: Schema[?], v: Any): String = Json.encode(any(s))(v)
    def decodeAny(s: Schema[?], j: Json): Either[String, Any] = Json.decode(any(s))(j)
  }

  // ---- the generator: the schema's nodes as a table, the code from their kinds ----

  /** the schema's nodes, by identity, each thunk forced ONCE: a
   * derived schema's thunks may build a fresh instance per call, so
   * the generator never calls one — it reads the children recorded
   * here, the same instances the table hands the generated code */
  private final class Nodes(root: Schema[?]):
    private val index = new java.util.IdentityHashMap[Schema[?], Integer]()
    private val kids = new java.util.IdentityHashMap[Schema[?], Vector[Schema[?]]]()
    val all = scala.collection.mutable.ArrayBuffer.empty[Schema[?]]
    private def visit(s: Schema[?]): Unit =
      if !index.containsKey(s) then
        // a schema whose thunks build fresh instances at every level
        // would walk forever; past this many nodes it is not a shape
        // worth compiling, and the door answers the interpreter
        if all.length >= 4096 then throw new IllegalStateException("RuntimeStaged: more than 4096 nodes — a thunk that never returns the same schema?")
        index.put(s, all.length); all += s
        val children: Vector[Schema[?]] = s match
          case Schema.SOption(of) => Vector(of())
          case Schema.SList(of) => Vector(of())
          case Schema.SVector(of) => Vector(of())
          case p: Schema.SProduct[?] => p.fields.map(_._2())
          case su: Schema.SSum[?] => su.cases.map(_._2())
          case Schema.SIso(u, _, _) => Vector(u())
          case _ => Vector.empty
        kids.put(s, children)
        children.foreach(visit)
    visit(root)
    def at(s: Schema[?]): Int =
      val i = index.get(s)
      if i == null then throw new IllegalStateException(s"RuntimeStaged: a node the walk did not record: $s")
      i.intValue
    def kid(s: Schema[?], i: Int): Schema[?] = kids.get(s)(i)

  private def generate[A](s: Schema[A]): JsonCodec[Any] =
    given Compiler = compiler
    val nodes = new Nodes(s)
    val table: Array[Schema[?]] = nodes.all.toArray
    val make: Array[Schema[?]] => JsonCodec[Any] = run {
      '{ (ns: Array[Schema[?]]) => ${ Gen(nodes, 'ns).codec(s) } }
    }
    make(table)

  private final class Gen(nodes: Nodes, ns: Expr[Array[Schema[?]]])(using Quotes):

    private def node(s: Schema[?]): Expr[Schema[?]] = '{ $ns(${ Expr(nodes.at(s)) }) }

    def codec(root: Schema[?]): Expr[JsonCodec[Any]] =
      '{ new JsonCodec[Any] {
           def encode(a: Any): String = {
             val sb = new java.lang.StringBuilder(64)
             ${ emit(root, 'a, 'sb, Nil) }
             sb.toString
           }
           def decode(j: Json): Either[String, Any] = ${ read(root, 'j, Nil) }
         } }

    // ---- encode ----

    def emit(s: Schema[?], v: Expr[Any], sb: Expr[java.lang.StringBuilder], seen: List[Schema[?]]): Expr[Unit] =
      if seen.exists(_ eq s) then '{ $sb.append(Unsafe.encodeAny(${ node(s) }, $v)): Unit }
      else s match
        case Schema.SInt => '{ $sb.append(Unsafe.int($v)): Unit }
        case Schema.SLong => '{ $sb.append(Unsafe.long($v)): Unit }
        case Schema.SDouble => '{ $sb.append(Unsafe.double($v)): Unit }
        case Schema.SBool => '{ $sb.append(Unsafe.bool($v)): Unit }
        case Schema.SString => '{ $sb.append('"').append(Json.escape(Unsafe.string($v))).append('"'): Unit }
        case Schema.SOption(_) =>
          val inner = nodes.kid(s, 0)
          '{ Unsafe.option($v) match
               case Some(y) => ${ emit(inner, 'y, sb, seen) }
               case None => $sb.append("null"): Unit }
        case Schema.SList(_) => emitSeq(nodes.kid(s, 0), '{ Unsafe.list($v).iterator }, sb, seen)
        case Schema.SVector(_) => emitSeq(nodes.kid(s, 0), '{ Unsafe.vector($v).iterator }, sb, seen)
        case p: Schema.SProduct[?] =>
          val here = s :: seen
          val pe = '{ Unsafe.product(${ node(s) }) }
          val fields = p.fields.zipWithIndex.map { (nf, i) =>
            val key = Expr((if i == 0 then "\"" else ",\"") + nf._1 + "\":")
            (key, nodes.kid(s, i))
          }
          '{ val it = $pe.parts($v).iterator
             $sb.append('{'): Unit
             ${ Expr.block(fields.toList.map { (key, fs) =>
                  '{ $sb.append($key): Unit; val fv = it.next(); ${ emit(fs, 'fv, sb, here) } } }, '{ () }) }
             $sb.append('}'): Unit }
        case su: Schema.SSum[?] =>
          val here = s :: seen
          val se = '{ Unsafe.sum(${ node(s) }) }
          def chain(rest: List[(Int, String, Schema[?])], k: Expr[Int]): Expr[Unit] = rest match
            case Nil => '{ $sb.append(Unsafe.encodeAny(${ node(s) }, $v)): Unit }
            case (i, name, cs) :: more =>
              val key = Expr("{\"" + name + "\":")
              '{ if $k == ${ Expr(i) } then {
                   $sb.append($key): Unit; ${ emit(cs, v, sb, here) }; $sb.append('}'): Unit
                 } else ${ chain(more, k) } }
          val cases = su.cases.zipWithIndex.map((nc, i) => (i, nc._1, nodes.kid(s, i))).toList
          '{ val k = $se.caseOf($v); ${ chain(cases, 'k) } }
        case Schema.SIso(_, _, _) =>
          val ie = '{ Unsafe.iso(${ node(s) }) }
          emit(nodes.kid(s, 0), '{ $ie.from($v) }, sb, seen)
        case _ => '{ $sb.append(Unsafe.encodeAny(${ node(s) }, $v)): Unit }   // SChar, SBytes: the fold's own spelling

    private def emitSeq(of: Schema[?], it: Expr[Iterator[Any]], sb: Expr[java.lang.StringBuilder], seen: List[Schema[?]]): Expr[Unit] =
      '{ val i = $it
         $sb.append('['): Unit
         var first = true
         while i.hasNext do
           if !first then $sb.append(','): Unit
           first = false
           val y = i.next()
           ${ emit(of, 'y, sb, seen) }
         $sb.append(']'): Unit }

    // ---- decode ----

    def read(s: Schema[?], j: Expr[Json], seen: List[Schema[?]]): Expr[Either[String, Any]] =
      if seen.exists(_ eq s) then '{ Unsafe.decodeAny(${ node(s) }, $j) }
      else s match
        case Schema.SInt => '{ $j match
          case Json.JNum(n) => Right(n.toInt)
          case got => Json.decode(Schema.SInt)(got) }
        case Schema.SLong => '{ $j match
          case Json.JNum(n) => Right(n.toLong)
          case got => Json.decode(Schema.SLong)(got) }
        case Schema.SDouble => '{ $j match
          case Json.JNum(n) => Right(n)
          case got => Json.decode(Schema.SDouble)(got) }
        case Schema.SBool => '{ $j match
          case Json.JBool(b) => Right(b)
          case got => Json.decode(Schema.SBool)(got) }
        case Schema.SString => '{ $j match
          case Json.JStr(x) => Right(x)
          case got => Json.decode(Schema.SString)(got) }
        case Schema.SOption(_) =>
          val inner = nodes.kid(s, 0)
          '{ $j match
               case Json.JNull => Right(None)
               case v => ${ read(inner, 'v, seen) }.map(Some(_)) }
        case Schema.SList(_) =>
          val inner = nodes.kid(s, 0)
          '{ $j match
               case Json.JArr(vs) => Staged.elems[Any](vs)(v => ${ read(inner, 'v, seen) })
               case got => Unsafe.decodeAny(${ node(s) }, got) }
        case Schema.SVector(_) =>
          val inner = nodes.kid(s, 0)
          '{ $j match
               case Json.JArr(vs) => Staged.elemsV[Any](vs)(v => ${ read(inner, 'v, seen) })
               case got => Unsafe.decodeAny(${ node(s) }, got) }
        case p: Schema.SProduct[?] =>
          val here = s :: seen
          val pe = '{ Unsafe.product(${ node(s) }) }
          val fields = p.fields.zipWithIndex.map((nf, i) => (nf._1, nodes.kid(s, i))).toList
          def fieldOf(i: Int, fs: Expr[Vector[(String, Json)]]): Expr[Either[String, Any]] =
            val (name, fs0) = fields(i)
            val isOpt = fs0.isInstanceOf[Schema.SOption[?]]
            val hasDefault = p.defaults.lift(i).flatten.isDefined
            val nameE = Expr(name)
            val absent: Expr[Either[String, Any]] =
              if hasDefault then '{ Right($pe.defaults(${ Expr(i) }).get()) }
              else if isOpt then '{ Right(None) }
              else '{ Left(${ Expr("missing field '" + name + "' in " + p.name) }) }
            if isOpt then
              '{ Staged.lookup($fs, $nameE) match
                   case None => $absent
                   case Some(Json.JErr(_)) => $absent
                   case Some(v) => ${ read(fs0, 'v, here) } }
            else
              '{ Staged.lookup($fs, $nameE) match
                   case None => $absent
                   case Some(v) => ${ read(fs0, 'v, here) } }
          def go(i: Int, acc: List[Expr[Any]], fs: Expr[Vector[(String, Json)]]): Expr[Either[String, Any]] =
            if i == fields.length then '{ Right($pe.make(${ Expr.ofSeq(acc) })) }
            else
              '{ ${ fieldOf(i, fs) } match
                   case Left(e) => Left(e)
                   case Right(x) => ${ go(i + 1, acc :+ 'x, fs) } }
          '{ $j match
               case Json.JObj(fs) => ${ go(0, Nil, 'fs) }
               case got => Unsafe.decodeAny(${ node(s) }, got) }
        case su: Schema.SSum[?] =>
          val here = s :: seen
          val cases = su.cases.zipWithIndex.map((nc, i) => (nc._1, nodes.kid(s, i))).toList
          def chain(rest: List[(String, Schema[?])], name: Expr[String], v: Expr[Json]): Expr[Either[String, Any]] = rest match
            case Nil => '{ Unsafe.decodeAny(${ node(s) }, $j) }   // an unknown case: the fold's own words
            case (n, cs) :: more =>
              '{ if $name == ${ Expr(n) } then ${ read(cs, v, here) } else ${ chain(more, name, v) } }
          '{ $j match
               case Json.JObj(fs) if fs.length == 1 =>
                 val (name, v) = fs(0)
                 ${ chain(cases, 'name, 'v) }
               case got => Unsafe.decodeAny(${ node(s) }, got) }
        case Schema.SIso(_, _, _) =>
          val ie = '{ Unsafe.iso(${ node(s) }) }
          '{ ${ read(nodes.kid(s, 0), j, seen) }.flatMap(b => $ie.to(b)) }
        case _ => '{ Unsafe.decodeAny(${ node(s) }, $j) }   // SChar, SBytes: the fold
}
