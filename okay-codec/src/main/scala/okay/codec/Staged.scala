package okay.codec

import scala.quoted.*
import scala.deriving.Mirror

/** a codec as CODE: encode and decode for one type, generated at
 * compile time from the type's structure — see Staged below */
trait JsonCodec[A]:
  def encode(a: A): String
  def decode(j: Json): Either[String, A]

/** the CBOR twin of JsonCodec: bytes in, bytes out */
trait CborCodec[A]:
  def encode(a: A): Array[Byte]
  def decode(bytes: Array[Byte]): Either[String, A]

/**
 * The STAGED fold mode specs/codecs.md promised: the same algebra as
 * Json.encode/decode and Cbor.write/read, but folded over the type's
 * shape at COMPILE time, emitting straight-line field access — the
 * Spark ExpressionEncoder trick, and P6's whole-stage codegen applied
 * to a data shape instead of a pipeline.
 *
 * Where the interpreted fold walks the Schema GADT per value (a thunk
 * call, a polymorphic lambda and a String per field; a Map and a
 * Vector[Any] per decoded object), `Staged.json[A]`/`Staged.cbor[A]`
 * walk the Mirror ONCE, at expansion, and what remains at run time is
 * what a person would write by hand: appends into one StringBuilder
 * (or CBOR item primitives into one `Cbor.Out`), a lookup per field,
 * the constructor call.
 *
 * The one rule that keeps either mode FAITHFUL to its fold: structure
 * is staged exactly where the schema has the Mirror's shape. That
 * cannot be read at expansion (a `given Schema[T] = Schema.derived`
 * is a val reference there, and a hand-written given looks the
 * same), so it is checked ONCE at construction: for each product or
 * sum the codec meets, a `val ok_T` compares the run-time schema's
 * field (or case) names with the Mirror's, and each staged node is
 * `if ok_T then <straight-line code> else <the fold with that
 * schema>`. So an Iso from Schema.wrap/refine, a hand-written
 * instance, Char, bytes and a type met again inside itself
 * (recursion stays a run-time fold, not an infinite expansion) all
 * travel exactly as the fold sends them, and a derived schema pays
 * one stable boolean read per node. Agreement is a test, not a hope:
 * TestStaged/TestStagedCbor check the two modes byte-for-byte (or
 * item-for-item) and Left-for-Left over the fixture space, and that
 * the staged path is the one TAKEN for a derived schema.
 *
 * Decode keeps each fold's totality rules verbatim: an absent field
 * takes its declared default, then None if optional, then the missing
 * refusal; a shape the staged code does not recognise goes to that
 * format's fold, so even the refusal MESSAGES are the fold's own.
 * CBOR's primitive items (`Cbor.In.intItem` and friends) are already
 * total Either-returning readers shared with the interpreted `get`,
 * so a primitive field needs no separate fallback at all — only
 * products, sums and sequences (whose LENGTH is a run-time value, not
 * something to unroll) carry a staged/fold branch.
 *
 * This is the module's third macro (Defaults.scala and the CST parser
 * choice in Json.scala are not; this and json-value-parser's sibling
 * staging effort earn the exception the same way: they read what the
 * compiler or the input already say and hand back an ordinary value).
 */
object Staged {

  /** the staged JSON codec for A: `val c = Staged.json[Order]` */
  inline def json[A]: JsonCodec[A] = ${ jsonImpl[A] }

  /** the staged CBOR codec for A: `val c = Staged.cbor[Order]` */
  inline def cbor[A]: CborCodec[A] = ${ cborImpl[A] }

  // ---- run-time helpers the generated JSON code calls ----

  /** the field of that name, as the fold's `fs.toMap.get` sees it:
   * toMap keeps the LAST duplicate, so does this */
  def lookup(fs: Vector[(String, Json)], name: String): Option[Json] =
    var i = 0
    var found: Option[Json] = None
    while i < fs.length do
      if fs(i)._1 == name then found = Some(fs(i)._2)
      i += 1
    found

  /** the fold's list rule: damaged elements are skipped, the rest
   * decode in order, the first Left ends it */
  def elems[X](vs: Vector[Json])(f: Json => Either[String, X]): Either[String, List[X]] =
    val b = List.newBuilder[X]
    var i = 0
    var failed: Option[String] = None
    while failed.isEmpty && i < vs.length do
      vs(i) match
        case Json.JErr(_) => ()
        case v => f(v) match
          case Right(x) => b += x
          case Left(e) => failed = Some(e)
      i += 1
    failed.toLeft(b.result())

  def elemsV[X](vs: Vector[Json])(f: Json => Either[String, X]): Either[String, Vector[X]] =
    elems(vs)(f).map(_.toVector)

  // ---- run-time helpers the generated CBOR code calls ----

  /** CBOR's list rule (unlike JSON's): a sequential read of the
   * declared count, the first Left ends it — there is no sentinel
   * item to skip, so no damaged-element rule to keep */
  def cborElems[X](in: Cbor.In, n: Long)(f: Cbor.In => Either[String, X]): Either[String, List[X]] =
    val b = List.newBuilder[X]
    var i = 0L
    var failed: Option[String] = None
    while failed.isEmpty && i < n do
      f(in) match
        case Right(x) => b += x
        case Left(e) => failed = Some(e)
      i += 1
    failed.toLeft(b.result())

  def cborElemsV[X](in: Cbor.In, n: Long)(f: Cbor.In => Either[String, X]): Either[String, Vector[X]] =
    cborElems(in, n)(f).map(_.toVector)

  /** CBOR maps carry no field ORDER guarantee (unlike a JSON object
   * this generator's own encoder writes in Mirror order, a document
   * from elsewhere may not) — so decode reads `n` (key, value) pairs
   * by NAME, each value at its OWN staged reader, into slots indexed
   * like the fold's Map build, then fills absences the same way:
   * declared default, then None-if-optional, then the refusal */
  def cborProduct[T](in: Cbor.In, n: Long, names: Array[String],
                     readers: Array[Cbor.In => Either[String, Any]],
                     absents: Array[Either[String, Any]],
                     tname: String,
                     make: Array[Any] => T): Either[String, T] =
    val slots = new Array[Any](names.length)
    val filled = new Array[Boolean](names.length)
    var i = 0L
    var err: Option[String] = None
    while err.isEmpty && i < n do
      in.textItem() match
        case Left(e) => err = Some(e)
        case Right(k) =>
          val idx = names.indexOf(k)
          if idx < 0 then err = Some(s"unknown field '$k' of $tname")
          else readers(idx)(in) match
            case Left(e) => err = Some(e)
            case Right(v) => slots(idx) = v; filled(idx) = true
      i += 1
    err match
      case Some(e) => Left(e)
      case None =>
        var j = 0
        var missing: Option[String] = None
        while missing.isEmpty && j < names.length do
          if !filled(j) then
            absents(j) match
              case Right(d) => slots(j) = d
              case Left(e) => missing = Some(e)
          j += 1
        missing.toLeft(()).map(_ => make(slots))

  // ---- the construction-time shape checks, shared by both formats ----

  /** does this schema have the Mirror's shape (the names, in order)? */
  def productShape(s: Schema[?], names: List[String]): Boolean = s match
    case p: Schema.SProduct[?] => p.fields.map(_._1).toList == names
    case _ => false
  def sumShape(s: Schema[?], names: List[String]): Boolean = s match
    case su: Schema.SSum[?] => su.cases.map(_._1).toList == names
    case _ => false

  // ---- the generators ----

  def jsonImpl[A: Type](using Quotes): Expr[JsonCodec[A]] =
    val g = new JsonGen
    val codec = '{
      new JsonCodec[A]:
        def encode(a: A): String =
          val sb = new java.lang.StringBuilder(64)
          ${ g.emit[A]('a, 'sb, Nil) }
          sb.toString
        def decode(j: Json): Either[String, A] = ${ g.read[A]('j, Nil) }
    }
    g.hoisted(codec)

  def cborImpl[A: Type](using Quotes): Expr[CborCodec[A]] =
    val g = new CborGen
    val codec = '{
      new CborCodec[A]:
        def encode(a: A): Array[Byte] =
          val out = new Cbor.Out
          ${ g.emit[A]('a, 'out, Nil) }
          out.toArray
        def decode(bytes: Array[Byte]): Either[String, A] =
          val in = new Cbor.In(bytes)
          ${ g.read[A]('in, Nil) }
    }
    g.hoisted(codec)

  private enum Shape:
    case Product, Sum

  /** the reflection any format's generator needs: the Mirror walked
   * once per type, and the ok_T booleans hoisted before the codec —
   * shared so JsonGen and CborGen read the SAME structure the SAME
   * way, and only their emit/read differ */
  private abstract class Reflect(using val q: Quotes):
    import q.reflect.*

    private val vals = scala.collection.mutable.ListBuffer.empty[ValDef]
    private val oks = scala.collection.mutable.Map.empty[String, Symbol]

    /** the ok_T vals first, the codec object after them: each staged
     * node reads its own once-computed boolean */
    def hoisted[C: Type](codec: Expr[C]): Expr[C] =
      Block(vals.toList, codec.asTerm).asExprOf[C]

    /** the boolean for T: created on first use, a val before the codec */
    protected def okFor[T: Type](shape: Shape, names: List[String], schema: Expr[Schema[T]]): Expr[Boolean] =
      val key = TypeRepr.of[T].show
      val sym = oks.getOrElseUpdate(key, {
        val s = Symbol.newVal(Symbol.spliceOwner, "ok_" + TypeRepr.of[T].typeSymbol.name,
          TypeRepr.of[Boolean], Flags.EmptyFlags, Symbol.noSymbol)
        val rhs: Expr[Boolean] = shape match
          case Shape.Product => '{ Staged.productShape($schema, ${ Expr(names) }) }
          case Shape.Sum => '{ Staged.sumShape($schema, ${ Expr(names) }) }
        vals += ValDef(s, Some(rhs.asTerm.changeOwner(s)))
        s
      })
      Ref(sym).asExprOf[Boolean]

    protected def schemaOf[T: Type]: Expr[Schema[T]] =
      Expr.summon[Schema[T]].getOrElse(
        report.errorAndAbort(s"Staged: no Schema for ${Type.show[T]}"))

    protected def tupleTypes[T: Type]: List[TypeRepr] =
      Type.of[T] match
        case '[h *: t] => TypeRepr.of[h] :: tupleTypes[t]
        case '[EmptyTuple] => Nil

    protected def labels[T: Type]: List[String] =
      tupleTypes[T].map {
        case ConstantType(StringConstant(s)) => s
        case other => report.errorAndAbort(s"Staged: a label that is not a string literal: ${other.show}")
      }

    /** the Mirror's structure, read once: the element types, the
     * labels, and the mirror itself (decode's constructor) */
    protected def mirrorOf[T: Type]: Option[(Shape, List[TypeRepr], List[String], Expr[Mirror.Of[T]])] =
      Expr.summon[Mirror.Of[T]] match
        case Some('{ $m: Mirror.ProductOf[T] { type MirroredElemTypes = ets; type MirroredElemLabels = ls } }) =>
          Some((Shape.Product, tupleTypes[ets], labels[ls], m))
        case Some('{ $m: Mirror.SumOf[T] { type MirroredElemTypes = ets; type MirroredElemLabels = ls } }) =>
          Some((Shape.Sum, tupleTypes[ets], labels[ls], m))
        case _ => None

    protected def seenBefore[T: Type](seen: List[TypeRepr]): Boolean =
      seen.exists(_ =:= TypeRepr.of[T])

  /** the JSON emitter: a schema-shaped value becomes StringBuilder
   * appends; a Json AST value becomes a constructor call */
  private class JsonGen(using Quotes) extends Reflect:
    import q.reflect.*

    def emit[T: Type](v: Expr[T], sb: Expr[java.lang.StringBuilder], seen: List[TypeRepr]): Expr[Unit] =
      Type.of[T] match
        case '[Int] => '{ $sb.append(${ v.asExprOf[Int] }): Unit }
        case '[Long] => '{ $sb.append(${ v.asExprOf[Long] }): Unit }
        case '[Double] => '{ $sb.append(${ v.asExprOf[Double] }): Unit }
        case '[Boolean] => '{ $sb.append(${ v.asExprOf[Boolean] }): Unit }
        case '[String] => '{ $sb.append('"').append(Json.escape(${ v.asExprOf[String] })).append('"'): Unit }
        case '[Option[x]] =>
          '{ ${ v.asExprOf[Option[x]] } match
               case Some(y) => ${ emit[x]('y, sb, seen) }
               case None => $sb.append("null"): Unit }
        case '[List[x]] => emitSeq[x]('{ ${ v.asExprOf[List[x]] }.iterator }, sb, seen)
        case '[Vector[x]] => emitSeq[x]('{ ${ v.asExprOf[Vector[x]] }.iterator }, sb, seen)
        case _ =>
          val schema = schemaOf[T]
          val fold = '{ $sb.append(Json.encode($schema)($v)): Unit }
          if seenBefore[T](seen) then fold
          else
            val here = TypeRepr.of[T] :: seen
            mirrorOf[T] match
              case Some((Shape.Product, types, names, _)) =>
                val ok = okFor[T](Shape.Product, names, schema)
                val fields = types.zip(names).zipWithIndex.map { case ((tpe, name), i) =>
                  tpe.asType match
                    case '[f] =>
                      val fv = Select.unique(v.asTerm, name).asExprOf[f]
                      val key = Expr((if i == 0 then "\"" else ",\"") + name + "\":")
                      '{ $sb.append($key): Unit; ${ emit[f](fv, sb, here) } }
                }
                '{ if $ok then {
                     $sb.append('{'): Unit
                     ${ Expr.block(fields, '{ () }) }
                     $sb.append('}'): Unit
                   } else $fold }
              case Some((Shape.Sum, types, names, _)) =>
                val ok = okFor[T](Shape.Sum, names, schema)
                // a type-test chain in case order, the Mirror's own
                // exhaustiveness; the tail is the fold, never reached
                def chain(rest: List[(TypeRepr, String)], x: Expr[T]): Expr[Unit] = rest match
                  case Nil => '{ $sb.append(Json.encode($schema)($x)): Unit }
                  case (tpe, name) :: more => tpe.asType match
                    case '[c] =>
                      val key = Expr("{\"" + name + "\":")
                      '{ $x match
                           case y: c => $sb.append($key): Unit; ${ emit[c]('y, sb, here) }; $sb.append('}'): Unit
                           case other => ${ chain(more, 'other) } }
                '{ if $ok then ${ chain(types.zip(names), v) } else $fold }
              case _ => fold

    private def emitSeq[X: Type](it: Expr[Iterator[X]], sb: Expr[java.lang.StringBuilder], seen: List[TypeRepr]): Expr[Unit] =
      '{ val i = $it
         $sb.append('['): Unit
         var first = true
         while i.hasNext do
           if !first then $sb.append(','): Unit
           first = false
           val y = i.next()
           ${ emit[X]('y, sb, seen) }
         $sb.append(']'): Unit }

    def read[T: Type](j: Expr[Json], seen: List[TypeRepr]): Expr[Either[String, T]] =
      Type.of[T] match
        case '[Int] => '{ $j match
          case Json.JNum(n) => Right(n.toInt)
          case got => Json.decode(Schema.SInt)(got) }.asExprOf[Either[String, T]]
        case '[Long] => '{ $j match
          case Json.JNum(n) => Right(n.toLong)
          case got => Json.decode(Schema.SLong)(got) }.asExprOf[Either[String, T]]
        case '[Double] => '{ $j match
          case Json.JNum(n) => Right(n)
          case got => Json.decode(Schema.SDouble)(got) }.asExprOf[Either[String, T]]
        case '[Boolean] => '{ $j match
          case Json.JBool(b) => Right(b)
          case got => Json.decode(Schema.SBool)(got) }.asExprOf[Either[String, T]]
        case '[String] => '{ $j match
          case Json.JStr(s) => Right(s)
          case got => Json.decode(Schema.SString)(got) }.asExprOf[Either[String, T]]
        case '[Option[x]] => '{ $j match
          case Json.JNull => Right(None)
          case v => ${ read[x]('v, seen) }.map(Some(_)) }.asExprOf[Either[String, T]]
        case '[List[x]] =>
          val schema = schemaOf[T]
          '{ $j match
            case Json.JArr(vs) => Staged.elems[x](vs)(v => ${ read[x]('v, seen) })
            case got => Json.decode($schema)(got) }.asExprOf[Either[String, T]]
        case '[Vector[x]] =>
          val schema = schemaOf[T]
          '{ $j match
            case Json.JArr(vs) => Staged.elemsV[x](vs)(v => ${ read[x]('v, seen) })
            case got => Json.decode($schema)(got) }.asExprOf[Either[String, T]]
        case _ =>
          val schema = schemaOf[T]
          val fold = '{ Json.decode($schema)($j) }
          if seenBefore[T](seen) then fold
          else
            val here = TypeRepr.of[T] :: seen
            mirrorOf[T] match
              case Some((Shape.Product, types, names, mirror)) =>
                val ok = okFor[T](Shape.Product, names, schema)
                val m = mirror.asExprOf[Mirror.ProductOf[T]]
                val tname = TypeRepr.of[T].typeSymbol.name
                val comp = TypeRepr.of[T].typeSymbol.companionModule
                def fieldOf(i: Int, fs: Expr[Vector[(String, Json)]]): Expr[Either[String, Any]] =
                  types(i).asType match
                    case '[f] =>
                      val name = names(i)
                      val isOpt = Type.of[f] match { case '[Option[?]] => true; case _ => false }
                      val absent: Expr[Either[String, f]] =
                        comp.methodMember("$lessinit$greater$default$" + (i + 1)) match
                          case d :: Nil if d.paramSymss.isEmpty =>
                            val dv = Ref(comp).select(d).asExprOf[f]
                            '{ Right($dv) }
                          case _ if isOpt => '{ Right(None) }.asExprOf[Either[String, f]]
                          case _ => '{ Left(${ Expr("missing field '" + name + "' in " + tname) }) }
                      val nameE = Expr(name)
                      val present: Expr[Either[String, f]] =
                        if isOpt then
                          '{ Staged.lookup($fs, $nameE) match
                               case None => $absent
                               case Some(Json.JErr(_)) => $absent
                               case Some(v) => ${ read[f]('v, here) } }
                        else
                          '{ Staged.lookup($fs, $nameE) match
                               case None => $absent
                               case Some(v) => ${ read[f]('v, here) } }
                      present.asExprOf[Either[String, Any]]
                def go(i: Int, acc: List[Expr[Any]], fs: Expr[Vector[(String, Json)]]): Expr[Either[String, T]] =
                  if i == types.size then
                    '{ Right($m.fromProduct(${ Expr.ofTupleFromSeq(acc) })) }
                  else
                    types(i).asType match
                      case '[f] =>
                        '{ ${ fieldOf(i, fs).asExprOf[Either[String, f]] }.flatMap(x => ${ go(i + 1, acc :+ 'x, fs) }) }
                '{ if $ok then {
                     $j match
                       case Json.JObj(fs) => ${ go(0, Nil, 'fs) }
                       case got => Json.decode($schema)(got)
                   } else $fold }
              case Some((Shape.Sum, types, names, _)) =>
                val ok = okFor[T](Shape.Sum, names, schema)
                val tname = TypeRepr.of[T].typeSymbol.name
                def chain(rest: List[(TypeRepr, String)], name: Expr[String], v: Expr[Json]): Expr[Either[String, T]] =
                  rest match
                    case Nil => '{ Left("unknown case '" + $name + "' of " + ${ Expr(tname) }) }
                    case (tpe, label) :: more => tpe.asType match
                      case '[c] =>
                        '{ if $name == ${ Expr(label) } then ${ read[c](v, here) }
                           else ${ chain(more, name, v) } }.asExprOf[Either[String, T]]
                '{ if $ok then {
                     $j match
                       case Json.JObj(Vector((name, v))) => ${ chain(types.zip(names), 'name, 'v) }
                       case got => Json.decode($schema)(got)
                   } else $fold }
              case _ => fold

  /** the CBOR emitter: primitives call the shared item primitives on
   * `Cbor.Out`/`Cbor.In` directly — the same code the interpreted
   * fold calls, so a primitive field needs no fold branch at all;
   * only products, sums and sequences (a run-time-valued length) do */
  private class CborGen(using Quotes) extends Reflect:
    import q.reflect.*

    def emit[T: Type](v: Expr[T], out: Expr[Cbor.Out], seen: List[TypeRepr]): Expr[Unit] =
      Type.of[T] match
        case '[Int] => '{ $out.integer(${ v.asExprOf[Int] }.toLong) }
        case '[Long] => '{ $out.integer(${ v.asExprOf[Long] }) }
        case '[Double] => '{ $out.double(${ v.asExprOf[Double] }) }
        case '[Boolean] => '{ $out.bool(${ v.asExprOf[Boolean] }) }
        case '[String] => '{ $out.text(${ v.asExprOf[String] }) }
        case '[Option[x]] =>
          '{ ${ v.asExprOf[Option[x]] } match
               case Some(y) => ${ emit[x]('y, out, seen) }
               case None => $out.nul() }
        case '[List[x]] => emitSeq[x]('{ ${ v.asExprOf[List[x]] }.iterator }, '{ ${ v.asExprOf[List[x]] }.length }, out, seen)
        case '[Vector[x]] => emitSeq[x]('{ ${ v.asExprOf[Vector[x]] }.iterator }, '{ ${ v.asExprOf[Vector[x]] }.length }, out, seen)
        case _ =>
          val schema = schemaOf[T]
          val fold = '{ Cbor.encodeItem($out, $v)(using $schema) }
          if seenBefore[T](seen) then fold
          else
            val here = TypeRepr.of[T] :: seen
            mirrorOf[T] match
              case Some((Shape.Product, types, names, _)) =>
                val ok = okFor[T](Shape.Product, names, schema)
                val fields = types.zip(names).map { case (tpe, name) =>
                  tpe.asType match
                    case '[f] =>
                      val fv = Select.unique(v.asTerm, name).asExprOf[f]
                      '{ $out.text(${ Expr(name) }); ${ emit[f](fv, out, here) } }
                }
                '{ if $ok then {
                     $out.mapHeader(${ Expr(types.length.toLong) })
                     ${ Expr.block(fields, '{ () }) }
                   } else $fold }
              case Some((Shape.Sum, types, names, _)) =>
                val ok = okFor[T](Shape.Sum, names, schema)
                def chain(rest: List[(TypeRepr, String)], x: Expr[T]): Expr[Unit] = rest match
                  case Nil => fold
                  case (tpe, name) :: more => tpe.asType match
                    case '[c] =>
                      '{ $x match
                           case y: c => $out.text(${ Expr(name) }); ${ emit[c]('y, out, here) }
                           case other => ${ chain(more, 'other) } }
                '{ if $ok then { $out.mapHeader(1); ${ chain(types.zip(names), v) } } else $fold }
              case _ => fold

    private def emitSeq[X: Type](it: Expr[Iterator[X]], len: Expr[Int], out: Expr[Cbor.Out], seen: List[TypeRepr]): Expr[Unit] =
      '{ $out.arrayHeader($len.toLong)
         val i = $it
         while i.hasNext do
           val y = i.next()
           ${ emit[X]('y, out, seen) } }

    def read[T: Type](in: Expr[Cbor.In], seen: List[TypeRepr]): Expr[Either[String, T]] =
      Type.of[T] match
        case '[Int] => '{ $in.intItem().map(_.toInt) }.asExprOf[Either[String, T]]
        case '[Long] => '{ $in.intItem() }.asExprOf[Either[String, T]]
        case '[Double] => '{ $in.doubleItem() }.asExprOf[Either[String, T]]
        case '[Boolean] => '{ $in.boolItem() }.asExprOf[Either[String, T]]
        case '[String] => '{ $in.textItem() }.asExprOf[Either[String, T]]
        case '[Option[x]] => '{
          if $in.isNull then { $in.skipNull(); Right(None) }
          else ${ read[x](in, seen) }.map(Some(_)) }.asExprOf[Either[String, T]]
        case '[List[x]] =>
          '{ $in.arrayHeader().flatMap(n => Staged.cborElems[x]($in, n)(cur => ${ read[x]('cur, seen) })) }
            .asExprOf[Either[String, T]]
        case '[Vector[x]] =>
          '{ $in.arrayHeader().flatMap(n => Staged.cborElemsV[x]($in, n)(cur => ${ read[x]('cur, seen) })) }
            .asExprOf[Either[String, T]]
        case _ =>
          val schema = schemaOf[T]
          val fold = '{ Cbor.decodeItem($in)(using $schema) }
          if seenBefore[T](seen) then fold
          else
            val here = TypeRepr.of[T] :: seen
            mirrorOf[T] match
              case Some((Shape.Product, types, names, mirror)) =>
                val ok = okFor[T](Shape.Product, names, schema)
                val m = mirror.asExprOf[Mirror.ProductOf[T]]
                val tname = TypeRepr.of[T].typeSymbol.name
                val comp = TypeRepr.of[T].typeSymbol.companionModule
                val readerExprs: List[Expr[Cbor.In => Either[String, Any]]] =
                  types.map { tpe =>
                    tpe.asType match
                      case '[f] => '{ (cur: Cbor.In) => ${ read[f]('cur, here) }.map(x => x: Any) }
                  }
                val absentExprs: List[Expr[Either[String, Any]]] =
                  types.zip(names).zipWithIndex.map { case ((tpe, name), i) =>
                    tpe.asType match
                      case '[f] =>
                        val isOpt = Type.of[f] match { case '[Option[?]] => true; case _ => false }
                        comp.methodMember("$lessinit$greater$default$" + (i + 1)) match
                          case d :: Nil if d.paramSymss.isEmpty =>
                            val dv = Ref(comp).select(d).asExprOf[f]
                            '{ Right($dv: Any) }
                          case _ if isOpt => '{ Right(None: Any) }
                          case _ => '{ Left(${ Expr("missing field '" + name + "' in " + tname) }) }
                  }
                val namesArr = '{ Array(${ Varargs(names.map(Expr(_))) }*) }
                val readersArr = '{ Array(${ Varargs(readerExprs) }*) }
                val absentsArr = '{ Array(${ Varargs(absentExprs) }*) }
                '{ if $ok then {
                     $in.mapHeader().flatMap(n =>
                       Staged.cborProduct[T]($in, n, $namesArr, $readersArr, $absentsArr,
                         ${ Expr(tname) }, xs => $m.fromProduct(Tuple.fromArray(xs))))
                   } else $fold }
              case Some((Shape.Sum, types, names, _)) =>
                val ok = okFor[T](Shape.Sum, names, schema)
                val tname = TypeRepr.of[T].typeSymbol.name
                def chain(rest: List[(TypeRepr, String)], name: Expr[String]): Expr[Either[String, T]] =
                  rest match
                    case Nil => '{ Left("unknown case '" + $name + "' of " + ${ Expr(tname) }) }
                    case (tpe, label) :: more => tpe.asType match
                      case '[c] =>
                        '{ if $name == ${ Expr(label) } then ${ read[c](in, here) }
                           else ${ chain(more, name) } }.asExprOf[Either[String, T]]
                '{ if $ok then {
                     $in.mapHeader().flatMap {
                       case 1 => $in.textItem().flatMap(name => ${ chain(types.zip(names), 'name) })
                       case n => Left("expected a one-entry map, got " + n + " entries")
                     }
                   } else $fold }
              case _ => fold
}
