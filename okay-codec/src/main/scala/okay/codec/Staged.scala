package okay.codec

import scala.quoted.*
import scala.deriving.Mirror

/** a codec as CODE: encode and decode for one type, generated at
 * compile time from the type's structure — see Staged below */
trait JsonCodec[A]:
  def encode(a: A): String
  def decode(j: Json): Either[String, A]

/**
 * The STAGED fold mode specs/codecs.md promised: the same JSON
 * algebra as Json.encode/decode, but folded over the type's shape at
 * COMPILE time, emitting straight-line field access — the Spark
 * ExpressionEncoder trick, and P6's whole-stage codegen applied to a
 * data shape instead of a pipeline.
 *
 * Where the interpreted fold walks the Schema GADT per value (a thunk
 * call, a polymorphic lambda and a String per field; a Map and a
 * Vector[Any] per decoded object), `Staged.json[A]` walks the Mirror
 * ONCE, at expansion, and what remains at run time is what a person
 * would write by hand: appends into one StringBuilder, a lookup per
 * field, the constructor call.
 *
 * The one rule that keeps it FAITHFUL to the fold: structure is
 * staged exactly where the schema has the Mirror's shape. That cannot
 * be read at expansion (a `given Schema[T] = Schema.derived` is a val
 * reference there, and a hand-written given looks the same), so it is
 * checked ONCE at construction: for each product or sum the codec
 * meets, a `val ok_T` compares the run-time schema's field (or case)
 * names with the Mirror's, and each staged node is `if ok_T then
 * <straight-line code> else <the fold with that schema>`. So an Iso
 * from Schema.wrap/refine, a hand-written instance, Char, bytes and a
 * type met again inside itself (recursion stays a run-time fold, not
 * an infinite expansion) all travel exactly as the fold sends them,
 * and a derived schema pays one stable boolean read per node.
 * Agreement is a test, not a hope: TestStaged checks the two modes
 * byte-for-byte and Left-for-Left over the fixture space, and that
 * the staged path is the one TAKEN for a derived schema.
 *
 * Decode keeps the fold's totality rules verbatim: an absent field
 * takes its declared default, then None if optional, then the missing
 * refusal; a damaged optional counts as absent; damaged array
 * elements are skipped; a shape the staged code does not recognise
 * goes to the fold, so even the refusal MESSAGES are the fold's own.
 *
 * This is the module's second macro (Defaults.scala is the first),
 * and it earns the exception the same way: it reads what the compiler
 * already knows and hands back an ordinary value.
 */
object Staged {

  /** the staged codec for A: `val c = Staged.json[Order]` */
  inline def json[A]: JsonCodec[A] = ${ jsonImpl[A] }

  // ---- run-time helpers the generated code calls (monomorphic, small) ----

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

  /** the construction-time shape checks: does this schema have the
   * Mirror's shape (the names, in order)? */
  def productShape(s: Schema[?], names: List[String]): Boolean = s match
    case p: Schema.SProduct[?] => p.fields.map(_._1).toList == names
    case _ => false
  def sumShape(s: Schema[?], names: List[String]): Boolean = s match
    case su: Schema.SSum[?] => su.cases.map(_._1).toList == names
    case _ => false

  // ---- the generator ----

  def jsonImpl[A: Type](using Quotes): Expr[JsonCodec[A]] =
    val g = new Gen
    val codec = '{
      new JsonCodec[A]:
        def encode(a: A): String =
          val sb = new java.lang.StringBuilder(64)
          ${ g.emit[A]('a, 'sb, Nil) }
          sb.toString
        def decode(j: Json): Either[String, A] = ${ g.read[A]('j, Nil) }
    }
    g.hoisted(codec)

  private enum Shape:
    case Product, Sum

  /** one expansion's state: the hoisted shape checks, one per type */
  private class Gen(using val q: Quotes):
    import q.reflect.*

    private val vals = scala.collection.mutable.ListBuffer.empty[ValDef]
    private val oks = scala.collection.mutable.Map.empty[String, Symbol]

    /** the ok_T vals first, the codec object after them: each staged
     * node reads its own once-computed boolean */
    def hoisted[A: Type](codec: Expr[JsonCodec[A]]): Expr[JsonCodec[A]] =
      Block(vals.toList, codec.asTerm).asExprOf[JsonCodec[A]]

    /** the boolean for T: created on first use, a val before the codec */
    private def okFor[T: Type](shape: Shape, names: List[String], schema: Expr[Schema[T]]): Expr[Boolean] =
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

    private def schemaOf[T: Type]: Expr[Schema[T]] =
      Expr.summon[Schema[T]].getOrElse(
        report.errorAndAbort(s"Staged.json: no Schema for ${Type.show[T]}"))

    private def tupleTypes[T: Type]: List[TypeRepr] =
      Type.of[T] match
        case '[h *: t] => TypeRepr.of[h] :: tupleTypes[t]
        case '[EmptyTuple] => Nil

    private def labels[T: Type]: List[String] =
      tupleTypes[T].map {
        case ConstantType(StringConstant(s)) => s
        case other => report.errorAndAbort(s"Staged.json: a label that is not a string literal: ${other.show}")
      }

    /** the Mirror's structure, read once: the element types, the
     * labels, and the mirror itself (decode's constructor) */
    private def mirrorOf[T: Type]: Option[(Shape, List[TypeRepr], List[String], Expr[Mirror.Of[T]])] =
      Expr.summon[Mirror.Of[T]] match
        case Some('{ $m: Mirror.ProductOf[T] { type MirroredElemTypes = ets; type MirroredElemLabels = ls } }) =>
          Some((Shape.Product, tupleTypes[ets], labels[ls], m))
        case Some('{ $m: Mirror.SumOf[T] { type MirroredElemTypes = ets; type MirroredElemLabels = ls } }) =>
          Some((Shape.Sum, tupleTypes[ets], labels[ls], m))
        case _ => None

    private def seenBefore[T: Type](seen: List[TypeRepr]): Boolean =
      seen.exists(_ =:= TypeRepr.of[T])

    // ---- encode ----

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

    // ---- decode ----

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
                // the field's value from the object: present, absent, or
                // damaged-optional — the fold's three doors, in its order
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
                // nested flatMaps, one per field, the constructor at the bottom
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
}
