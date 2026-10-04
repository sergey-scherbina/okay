package okay2.codec

import scala.collection.mutable
import scala.reflect.macros.blackbox

/**
 * The generators behind `Staged.json`/`cbor`/`strict` (okay-codec's
 * `jsonImpl`/`cborImpl`/`strictImpl`, which read a `Mirror` in a quote):
 * the class is read here as `SchemaMacro` reads it, once per type, and
 * each format's code is written as trees. The shape checks and the
 * schemas are vals hoisted before the codec, one per type met, so each
 * staged node reads a once-computed boolean.
 *
 * Every recursion here walks the TYPE the codec is generated for, and a
 * type met again inside itself is not walked again (`seen`: it becomes
 * a run-time fold), so the depth is bounded by the number of distinct
 * types in the declaration.
 */
class StagedMacro(val c: blackbox.Context) {
  import c.universe._

  private val ST = tq"_root_.okay2.codec.Schema"
  private val J = q"_root_.okay2.codec.Json"
  private val JT = tq"_root_.okay2.codec.Json"
  private val St = q"_root_.okay2.codec.Staged"
  private val Cb = q"_root_.okay2.codec.Cbor"
  private val InT = tq"_root_.okay2.codec.Cbor.In"
  private val RT = tq"_root_.okay2.codec.JsonStrict.Reader"
  private val N = q"_root_.okay2.codec.Numbers"
  private val Str = tq"_root_.java.lang.String"
  private val Right = q"_root_.scala.util.Right"
  private val Left = q"_root_.scala.util.Left"
  private val AnyT = tq"_root_.scala.Any"
  private val Fields = tq"_root_.scala.collection.immutable.Vector[(_root_.java.lang.String, _root_.okay2.codec.Json)]"

  // ---------------------------------------------------------------- the class, read once

  /** a product: its fields in order, how to make one from a tree per
   * field, and each field's absence (default, None, or the refusal) */
  private final class Prod(val name: String, val fields: List[(String, Type)], val make: List[Tree] => Tree,
                           val absent: List[Tree])
  /** a sum: its cases in order, each with the type its pattern tests */
  private final class Sum(val name: String, val cases: List[(String, Type, Tree)])

  private def fresh(s: String): TermName = TermName(c.freshName(s))

  private def isOption(t: Type) = t.typeSymbol == definitions.OptionClass
  private def isList(t: Type) = t.typeSymbol == symbolOf[List[Any]]
  private def isVector(t: Type) = t.typeSymbol == symbolOf[Vector[Any]]
  private def arg(t: Type): Type = t.typeArgs.head

  /** the class's shape, as `SchemaMacro.derive` reads it; None where the
   * schema is not derived from the class (a standard type, a plain
   * class), which the codec leaves to the fold */
  private def shapeOf(t0: Type): Option[Either[Prod, Sum]] = {
    val t = t0.dealias
    val sym = t.typeSymbol
    if (!sym.isClass) None
    else {
      val cls = sym.asClass
      val full = cls.fullName
      val tuple = full.startsWith("scala.Tuple") && cls.isCaseClass
      val prefix = t match {
        case TypeRef(pre, _, _) => pre
        case SingleType(pre, _) => pre
        case _ => NoPrefix
      }
      def ref(s: Symbol): Tree = c.internal.gen.mkAttributedRef(prefix, s)
      val name = cls.name.decodedName.toString
      if (!tuple && (full.startsWith("scala.") || full.startsWith("java."))) None
      else if (cls.isModuleClass && cls.isCaseClass) {
        val obj = ref(cls.module)
        Some(scala.util.Left(new Prod(name, Nil, _ => obj, Nil)))
      } else if (cls.isCaseClass) {
        cls.primaryConstructor.asMethod.paramLists match {
          case ps :: rest if rest.forall(_.forall(_.isImplicit)) =>
            val types = ps.map(_.typeSignature.substituteTypes(cls.typeParams, t.typeArgs))
            if (types.exists(_.typeSymbol == definitions.RepeatedParamClass)) None
            else {
              val names = ps.map(_.name.decodedName.toString)
              val companion = if (cls.companion != NoSymbol) ref(cls.companion) else Ident(cls.name.toTermName)
              val absent = ps.zip(types).zipWithIndex.map { case ((p, ft), i) =>
                if (p.asTerm.isParamWithDefault) {
                  val m = TermName("$lessinit$greater$default$" + (i + 1))
                  val call = if (t.typeArgs.isEmpty) q"$companion.$m" else q"$companion.$m[..${t.typeArgs}]"
                  q"$Right($call: $ft)"
                } else if (isOption(ft)) q"$Right(_root_.scala.None)"
                else q"$Left(${s"missing field '${names(i)}' in $name"})"
              }
              Some(scala.util.Left(new Prod(name, names.zip(types), args => q"new $t(..$args)", absent)))
            }
          case _ => None
        }
      } else if (cls.isSealed && (cls.isTrait || cls.isAbstract)) {
        val _ = cls.typeSignature // completes the class, so its children are known
        val subs = cls.knownDirectSubclasses.toList.sortBy(s => (s.pos.line, s.pos.column))
        val cases = subs.map { s =>
          val sc = s.asClass
          val ct =
            if (sc.typeParams.isEmpty) sc.toType
            else appliedType(sc.toType.typeConstructor, t.typeArgs)
          val pattern =
            if (sc.typeParams.isEmpty) tq"$ct"
            else tq"${c.internal.existentialAbstraction(sc.typeParams, sc.toType)}"
          (sc.name.decodedName.toString, ct, pattern)
        }
        if (cases.isEmpty) None else Some(scala.util.Right(new Sum(name, cases)))
      } else None
    }
  }

  // ---------------------------------------------------------------- hoisted vals

  /** the vals written before the codec: a schema and a shape check per
   * type, created on first use */
  private final class Hoist {
    val vals = mutable.ListBuffer.empty[Tree]
    private val schemas = mutable.Map.empty[String, TermName]
    private val oks = mutable.Map.empty[String, TermName]

    def schema(t: Type): Tree = Ident(schemas.getOrElseUpdate(t.toString, {
      val n = fresh("schema")
      vals += q"val $n: $ST[$t] = _root_.scala.Predef.implicitly[$ST[$t]]"
      n
    }))

    def ok(t: Type, shape: Either[Prod, Sum]): Tree = {
      val s = schema(t)
      Ident(oks.getOrElseUpdate(t.toString, {
        val n = fresh("ok")
        val check = shape match {
          case scala.util.Left(p) => q"$St.productShape($s, _root_.scala.List(..${p.fields.map(_._1)}))"
          case scala.util.Right(su) => q"$St.sumShape($s, _root_.scala.List(..${su.cases.map(_._1)}))"
        }
        vals += q"val $n: _root_.scala.Boolean = $check"
        n
      }))
    }

    def around(codec: Tree): Tree = q"{ ..$vals; $codec }"
  }

  private def seenBefore(t: Type, seen: List[Type]): Boolean = seen.exists(_ =:= t)

  // ---------------------------------------------------------------- JSON

  private final class JsonGen(h: Hoist) {

    def emit(v: Tree, t: Type, sb: Tree, seen: List[Type]): Tree =
      if (t =:= typeOf[Int] || t =:= typeOf[Long] || t =:= typeOf[Double] || t =:= typeOf[Boolean])
        q"{ $sb.append($v.toString); () }"
      else if (t =:= typeOf[String]) q"$St.jText($sb, $v)"
      else if (isOption(t)) {
        val y = fresh("y")
        q"""$v match {
              case _root_.scala.Some($y) => ${emit(Ident(y), arg(t), sb, seen)}
              case _root_.scala.None => { $sb.append("null"); () }
            }"""
      } else if (isList(t) || isVector(t)) {
        val it = fresh("it")
        val first = fresh("first")
        val y = fresh("y")
        q"""{
              val $it = $v.iterator
              $sb.append('[')
              var $first = true
              while ($it.hasNext) {
                if (!$first) $sb.append(',')
                $first = false
                val $y = $it.next()
                ${emit(Ident(y), arg(t), sb, seen)}
              }
              $sb.append(']')
              ()
            }"""
      } else {
        val s = h.schema(t)
        val x = fresh("x")
        def fold = q"{ $sb.append($J.encode($s)($x)); () }"
        val body = if (seenBefore(t, seen)) fold else {
          val here = t :: seen
          shapeOf(t) match {
            case Some(shape @ scala.util.Left(p)) =>
              val fields = p.fields.zipWithIndex.map { case ((name, ft), i) =>
                val key = (if (i == 0) "\"" else ",\"") + name + "\":"
                q"$sb.append($key); ${emit(q"$x.${TermName(name).encodedName.toTermName}", ft, sb, here)}"
              }
              q"if (${h.ok(t, shape)}) { $sb.append('{'); ..$fields; $sb.append('}'); () } else $fold"
            case Some(shape @ scala.util.Right(su)) =>
              val cases = su.cases.map { case (name, ct, pat) =>
                val y = fresh("y")
                cq"""$y: $pat => { $sb.append(${"{\"" + name + "\":"}); ${emit(Ident(y), ct, sb, here)}; $sb.append('}'); () }"""
              }
              q"if (${h.ok(t, shape)}) $x match { case ..$cases } else $fold"
            case None => fold
          }
        }
        q"{ val $x: $t = $v; $body }"
      }

    def read(j: Tree, t: Type, seen: List[Type]): Tree =
      if (t =:= typeOf[Int]) q"$St.jInt($j)"
      else if (t =:= typeOf[Long]) q"$St.jLong($j)"
      else if (t =:= typeOf[Double]) q"$St.jDouble($j)"
      else if (t =:= typeOf[Boolean]) q"$St.jBool($j)"
      else if (t =:= typeOf[String]) q"$St.jString($j)"
      else if (isOption(t)) {
        val v = fresh("v")
        q"$St.jOption[${arg(t)}]($j)(($v: $JT) => ${read(Ident(v), arg(t), seen)})"
      } else if (isList(t) || isVector(t)) {
        val v = fresh("v")
        val door = if (isList(t)) TermName("jList") else TermName("jVector")
        q"$St.$door[${arg(t)}]($j, ${h.schema(t)})(($v: $JT) => ${read(Ident(v), arg(t), seen)})"
      } else {
        val s = h.schema(t)
        def fold = q"$J.decode($s)($j)"
        if (seenBefore(t, seen)) fold
        else {
          val here = t :: seen
          shapeOf(t) match {
            case Some(shape @ scala.util.Left(p)) =>
              val fs = fresh("fs")
              def chain(rest: List[((String, Type), Tree)], made: List[Tree]): Tree = rest match {
                case Nil => q"$Right(${p.make(made.reverse)})"
                case ((name, ft), absent) :: more =>
                  val v = fresh("v")
                  val x = fresh("x")
                  q"""$St.jField[$ft]($fs, $name, ${isOption(ft)}, $absent)(($v: $JT) => ${read(Ident(v), ft, here)})
                        .flatMap(($x: $ft) => ${chain(more, Ident(x) :: made)})"""
              }
              q"if (${h.ok(t, shape)}) $St.jObject[$t]($j, $s)(($fs: $Fields) => ${chain(p.fields.zip(p.absent), Nil)}) else $fold"
            case Some(shape @ scala.util.Right(su)) =>
              val name = fresh("name")
              val v = fresh("v")
              val chain = su.cases.foldRight[Tree](q"""$Left("unknown case '" + $name + ${s"' of ${su.name}"})""") {
                case ((label, ct, _), rest) => q"if ($name == $label) ${read(Ident(v), ct, here)} else $rest"
              }
              q"if (${h.ok(t, shape)}) $St.jSum[$t]($j, $s)(($name: $Str, $v: $JT) => $chain) else $fold"
            case None => fold
          }
        }
      }
  }

  // ---------------------------------------------------------------- CBOR

  private final class CborGen(h: Hoist) {

    def emit(v: Tree, t: Type, out: Tree, seen: List[Type]): Tree =
      if (t =:= typeOf[Int]) q"$out.integer($v.toLong)"
      else if (t =:= typeOf[Long]) q"$out.integer($v)"
      else if (t =:= typeOf[Double]) q"$out.double($v)"
      else if (t =:= typeOf[Boolean]) q"$out.bool($v)"
      else if (t =:= typeOf[String]) q"$out.text($v)"
      else if (isOption(t)) {
        val y = fresh("y")
        q"""$v match {
              case _root_.scala.Some($y) => ${emit(Ident(y), arg(t), out, seen)}
              case _root_.scala.None => $out.nul()
            }"""
      } else if (isList(t) || isVector(t)) {
        val xs = fresh("xs")
        val it = fresh("it")
        val y = fresh("y")
        q"""{
              val $xs = $v
              $out.arrayHeader($xs.length.toLong)
              val $it = $xs.iterator
              while ($it.hasNext) {
                val $y = $it.next()
                ${emit(Ident(y), arg(t), out, seen)}
              }
            }"""
      } else {
        val s = h.schema(t)
        val x = fresh("x")
        def fold = q"$Cb.encodeItem($out, $x)($s)"
        val body = if (seenBefore(t, seen)) fold else {
          val here = t :: seen
          shapeOf(t) match {
            case Some(shape @ scala.util.Left(p)) =>
              val fields = p.fields.map { case (name, ft) =>
                q"$out.text($name); ${emit(q"$x.${TermName(name).encodedName.toTermName}", ft, out, here)}"
              }
              q"if (${h.ok(t, shape)}) { $out.mapHeader(${p.fields.length.toLong}); ..$fields } else $fold"
            case Some(shape @ scala.util.Right(su)) =>
              val cases = su.cases.map { case (name, ct, pat) =>
                val y = fresh("y")
                cq"$y: $pat => { $out.text($name); ${emit(Ident(y), ct, out, here)} }"
              }
              q"if (${h.ok(t, shape)}) { $out.mapHeader(1); $x match { case ..$cases } } else $fold"
            case None => fold
          }
        }
        q"{ val $x: $t = $v; $body }"
      }

    def read(in: Tree, t: Type, seen: List[Type]): Tree =
      if (t =:= typeOf[Int]) q"$in.intItem().flatMap(($N.int(_: _root_.scala.Long)))"
      else if (t =:= typeOf[Long]) q"$in.intItem()"
      else if (t =:= typeOf[Double]) q"$in.doubleItem()"
      else if (t =:= typeOf[Boolean]) q"$in.boolItem()"
      else if (t =:= typeOf[String]) q"$in.textItem()"
      else if (isOption(t)) {
        val cur = fresh("cur")
        q"$St.cOption[${arg(t)}]($in)(($cur: $InT) => ${read(Ident(cur), arg(t), seen)})"
      } else if (isList(t) || isVector(t)) {
        val cur = fresh("cur")
        val n = fresh("n")
        val door = if (isList(t)) TermName("cborElems") else TermName("cborElemsV")
        q"$in.arrayHeader().flatMap(($n: _root_.scala.Long) => $St.$door[${arg(t)}]($in, $n)(($cur: $InT) => ${read(Ident(cur), arg(t), seen)}))"
      } else {
        val s = h.schema(t)
        def fold = q"$Cb.decodeItem($in)($s)"
        if (seenBefore(t, seen)) fold
        else {
          val here = t :: seen
          shapeOf(t) match {
            case Some(shape @ scala.util.Left(p)) =>
              val n = fresh("n")
              val made = productArrays(p, here, (cur, ft) => read(cur, ft, here), InT)
              q"""if (${h.ok(t, shape)})
                    $in.mapHeader().flatMap(($n: _root_.scala.Long) =>
                      $St.cborProduct[$t]($in, $n, ${made._1}, ${made._2}, ${made._3}, ${made._4}))
                  else $fold"""
            case Some(shape @ scala.util.Right(su)) =>
              val name = fresh("name")
              q"if (${h.ok(t, shape)}) $St.cborSum[$t]($in)(($name: $Str) => ${caseChain(su, name, ct => read(in, ct, here))}) else $fold"
            case None => fold
          }
        }
      }
  }

  // ---------------------------------------------------------------- strict JSON

  private final class StrictGen(h: Hoist) {

    def read(r: Tree, t: Type, seen: List[Type]): Tree =
      if (t =:= typeOf[Int]) q"$r.number().flatMap(($N.int(_: _root_.scala.Double)))"
      else if (t =:= typeOf[Long]) q"$r.number().flatMap($N.long)"
      else if (t =:= typeOf[Double]) q"$r.number()"
      else if (t =:= typeOf[Boolean]) q"$r.bool()"
      else if (t =:= typeOf[String]) q"$r.string()"
      else if (isOption(t)) {
        val cur = fresh("cur")
        q"$St.sOption[${arg(t)}]($r)(($cur: $RT) => ${read(Ident(cur), arg(t), seen)})"
      } else if (isList(t) || isVector(t)) {
        val cur = fresh("cur")
        val door = if (isList(t)) TermName("strictElems") else TermName("strictElemsV")
        q"$St.$door[${arg(t)}]($r)(($cur: $RT) => ${read(Ident(cur), arg(t), seen)})"
      } else {
        val s = h.schema(t)
        def fold = q"$r.get($s)"
        if (seenBefore(t, seen)) fold
        else {
          val here = t :: seen
          shapeOf(t) match {
            case Some(shape @ scala.util.Left(p)) =>
              val made = productArrays(p, here, (cur, ft) => read(cur, ft, here), RT)
              q"if (${h.ok(t, shape)}) $St.strictProduct[$t]($r, ${made._1}, ${made._2}, ${made._3}, ${made._4}) else $fold"
            case Some(shape @ scala.util.Right(su)) =>
              val name = fresh("name")
              q"if (${h.ok(t, shape)}) $St.strictSum[$t]($r)(($name: $Str) => ${caseChain(su, name, ct => read(r, ct, here))}) else $fold"
            case None => fold
          }
        }
      }
  }

  // ---------------------------------------------------------------- shared by CBOR and strict

  /** a product read by name into slots: the names, a reader per field,
   * each field's absence, and `make` from the filled slots. `make`
   * reads each slot back at its field's type — the slot at index i was
   * written by field i's own reader, as `SchemaMacro`'s `make` reads
   * the fold's parts */
  private def productArrays(p: Prod, here: List[Type], readOne: (Tree, Type) => Tree, cursor: Tree): (Tree, Tree, Tree, Tree) = {
    val names = q"_root_.scala.Array[$Str](..${p.fields.map(_._1)})"
    val readers = p.fields.map { case (_, ft) =>
      val cur = fresh("cur")
      val x = fresh("x")
      q"($cur: $cursor) => ${readOne(Ident(cur), ft)}.map(($x: $ft) => ($x: $AnyT))"
    }
    val readersArr = q"_root_.scala.Array[$cursor => _root_.scala.util.Either[$Str, $AnyT]](..$readers)"
    val absents = q"_root_.scala.Array[_root_.scala.util.Either[$Str, $AnyT]](..${p.absent})"
    val xs = fresh("xs")
    val args = p.fields.zipWithIndex.map { case ((_, ft), i) => q"$xs($i).asInstanceOf[$ft]" }
    val make = q"($xs: _root_.scala.Array[$AnyT]) => ${p.make(args)}"
    (names, readersArr, absents, make)
  }

  /** the case a name selects, each read at its own type; the fold's
   * words for an unknown one */
  private def caseChain(su: Sum, name: TermName, readCase: Type => Tree): Tree =
    su.cases.foldRight[Tree](q"""$Left("unknown case '" + $name + ${s"' of ${su.name}"})""") {
      case ((label, ct, _), rest) => q"if ($name == $label) ${readCase(ct)} else $rest"
    }

  // ---------------------------------------------------------------- the three doors

  def json[A: c.WeakTypeTag]: c.Expr[JsonCodec[A]] = {
    val A = weakTypeOf[A].dealias
    val h = new Hoist
    val g = new JsonGen(h)
    val a = fresh("a")
    val sb = fresh("sb")
    val j = fresh("j")
    val enc = g.emit(Ident(a), A, Ident(sb), Nil)
    val dec = g.read(Ident(j), A, Nil)
    c.Expr[JsonCodec[A]](h.around(q"""
      new _root_.okay2.codec.JsonCodec[$A] {
        def encode($a: $A): $Str = {
          val $sb = new _root_.scala.collection.mutable.StringBuilder(64)
          $enc
          $sb.toString
        }
        def decode($j: $JT): _root_.scala.util.Either[$Str, $A] = $dec
      }"""))
  }

  def cbor[A: c.WeakTypeTag]: c.Expr[CborCodec[A]] = {
    val A = weakTypeOf[A].dealias
    val h = new Hoist
    val g = new CborGen(h)
    val a = fresh("a")
    val out = fresh("out")
    val bytes = fresh("bytes")
    val in = fresh("in")
    val enc = g.emit(Ident(a), A, Ident(out), Nil)
    val dec = g.read(Ident(in), A, Nil)
    c.Expr[CborCodec[A]](h.around(q"""
      new _root_.okay2.codec.CborCodec[$A] {
        def encode($a: $A): _root_.scala.Array[_root_.scala.Byte] = {
          val $out = new $Cb.Out
          $enc
          $out.toArray
        }
        def decode($bytes: _root_.scala.Array[_root_.scala.Byte]): _root_.scala.util.Either[$Str, $A] = {
          val $in = new $Cb.In($bytes)
          $dec
        }
      }"""))
  }

  def strict[A: c.WeakTypeTag]: c.Expr[StrictJsonCodec[A]] = {
    val A = weakTypeOf[A].dealias
    val h = new Hoist
    val g = new StrictGen(h)
    val text = fresh("text")
    val r = fresh("r")
    val x = fresh("x")
    val dec = g.read(Ident(r), A, Nil)
    c.Expr[StrictJsonCodec[A]](h.around(q"""
      new _root_.okay2.codec.StrictJsonCodec[$A] {
        def decode($text: $Str): _root_.scala.util.Either[$Str, $A] = {
          val $r = new _root_.okay2.codec.JsonStrict.Reader($text)
          $r.skipWs()
          $dec.flatMap { ($x: $A) =>
            $r.skipWs()
            if ($r.at == $text.length) $Right($x) else $Left("trailing input at " + $r.at)
          }
        }
      }"""))
  }
}
