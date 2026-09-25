package okay.arrow

import okay.codec.{Cbor, Columns, Json, Schema}
import okay.codec.Columns.{ColType, Row}

/**
 * Typed rows as Arrow tables and back (specs/okay-arrow.md stage 5), for
 * any datatype with a `Schema`:
 *
 * {{{
 * val bytes = OkayArrow.encode(orders)        // Seq[Order] -> one Arrow stream
 * OkayArrow.decode[Order](bytes)              // Either[String, Vector[Order]]
 * }}}
 *
 * The WRITE is okay-codec's `Columns` — the one place the tabular reading
 * of a `Schema` is decided, which Spark, DuckDB and Delta already use —
 * translated to the model: a product's fields are the table's columns, a
 * nested product a struct, a list or vector a list, `Option` a null, an
 * enum of field-less cases its case NAME as text, a sum with payloads a
 * struct of `kind` and one nullable branch per case, `BigInt`
 * decimal(38, 0), a recursive type its CBOR beside its JSON.
 *
 * The READ is a fold over the same `Schema` making the same decisions
 * backwards, so what one writes the other reads (TestRows, on every
 * platform). A column that is not what the schema expects, a null where
 * the schema has no `Option`, a case name the sum does not have, is a
 * `Left` naming the row and the column.
 */
object Rows:

  /** rows as a table: one column per field of `A` (or one `value` column
   * when `A` is not a product) */
  def table[A](rows: Seq[A])(using s: Schema[A]): Table =
    val (fields, row) = Columns.table[A]
    val rs = rows.iterator.map(row).toVector
    Table(fields.zipWithIndex.map((f, i) => f.name -> column(f.tpe, rs.map(_.values(i)), f.name)), Vector.empty)

  /** a table as rows of `A`; Left names the first row and column that do not fit */
  def rows[A](t: Table)(using s: Schema[A]): Either[String, Vector[A]] =
    // a table of no rows is no rows, whatever its columns say: a column
    // without a value has no kind on the JSON frame road (it comes back
    // Nulls(0)), and there is nothing in it to refuse (foreign-facade-2)
    if t.rows == 0 then Right(Vector.empty)
    else try
      val reader = Schema.fold(s)(Read(Columns.recursiveNames(s)))
      val root = Columns.column(s).tpe match
        case ColType.Struct(_) if !Columns.column(s).nullable =>
          Column.Struct(t.cols, Array.fill(t.rows)(true))
        case _ => t.cols.collectFirst { case ("value", c) => c }
          .getOrElse(throw Mismatch(s"no 'value' column (the table has ${t.cols.map(_._1).mkString(", ")})"))
      val at = reader.at(root, "")
      Right(Vector.tabulate(t.rows)(i =>
        try at(i) catch case Mismatch(why) => throw Mismatch(s"row $i: $why")))
    catch case Mismatch(why) => Left(why)

  private final case class Mismatch(why: String) extends RuntimeException(why, null, false, false)

  // ---- the write: Columns' plain values into the model ---------------------

  private def column(t: ColType, vs: Vector[Any], name: String): Column =
    val ok = vs.iterator.map(_ != null).toArray
    def bad(v: Any): Nothing =
      throw IllegalArgumentException(s"column '$name': ${v.getClass.getSimpleName} where Columns promised $t")
    def each[X: scala.reflect.ClassTag](zero: X)(pick: PartialFunction[Any, X]): Array[X] =
      vs.iterator.map(v => if v == null then zero else pick.applyOrElse(v, bad)).toArray
    t match
      case ColType.Int32 => Column.Ints(32, true, each(0L) { case i: Int => i.toLong }, ok)
      case ColType.Int64 => Column.Int64(each(0L) { case l: Long => l }, ok)
      case ColType.Float64 => Column.Float64(each(0.0) { case d: Double => d }, ok)
      case ColType.Bool => Column.Bool(each(false) { case b: Boolean => b }, ok)
      case ColType.Text => Column.Utf8(each("") { case s: String => s }, ok)
      case ColType.Binary => Column.Binary(each(Array.emptyByteArray) { case b: Array[Byte] => b }, ok)
      case ColType.Decimal(p, sc) => Column.Decimal(p, sc, each(BigInt(0)) {
        case b: BigInt => b
        case d: BigDecimal => d.bigDecimal.movePointRight(sc).toBigIntegerExact
      }, ok)
      case ColType.Json => Column.Utf8(each("") { case j: Json => Json.print(j) }, ok)
      case ColType.Arr(elem, _) =>
        val lists = vs.map {
          case null => Vector.empty
          case xs: Iterable[?] => xs.toVector
          case other => bad(other)
        }
        val offs = lists.scanLeft(0)(_ + _.length).toArray
        Column.ListOf(offs, column(elem, lists.flatten, s"$name.item"), ok)
      case ColType.Struct(fs) =>
        val rows = vs.map {
          case null => null
          case r: Row => r
          case other => bad(other)
        }
        Column.Struct(fs.zipWithIndex.map((f, i) =>
          f.name -> column(f.tpe, rows.map(r => if r == null then null else r.values(i)), s"$name.${f.name}")), ok)

  // ---- the read: a fold over the Schema -------------------------------------

  /** a reader: given the column a value sits in, the value at a row */
  private final case class R[A](at: (Column, String) => Int => A)

  private final class Read(recursive: Set[String]) extends Schema.Algebra[R]:

    private def fail(where: String, why: String): Nothing =
      throw Mismatch(s"column '${if where.isEmpty then "(root)" else where}': $why")

    private def present(ok: Array[Boolean], i: Int, where: String, what: String): Unit =
      if !ok(i) then fail(where, s"null where the schema has no Option around $what")

    private def longs(c: Column, where: String, what: String): (Array[Long], Array[Boolean]) = c match
      case Column.Int64(v, ok) => (v, ok)
      case Column.Ints(_, _, v, ok) => (v, ok)
      case other => fail(where, s"${Column.describe(other)} where the schema has $what")

    private def within(lo: Long, hi: Long, what: String): R[Long] = R { (c, w) =>
      val (v, ok) = longs(c, w, what)
      i => { present(ok, i, w, what); if v(i) < lo || v(i) > hi then fail(w, s"${v(i)} is past $what") else v(i) }
    }

    def int = { val r = within(Int.MinValue, Int.MaxValue, "an Int"); R((c, w) => { val f = r.at(c, w); i => f(i).toInt }) }
    def long = within(Long.MinValue, Long.MaxValue, "a Long")
    def double = R { (c, w) => c match
      case Column.Float64(v, ok) => i => { present(ok, i, w, "a Double"); v(i) }
      case Column.Float32(v, ok) => i => { present(ok, i, w, "a Double"); v(i).toDouble }
      case other => fail(w, s"${Column.describe(other)} where the schema has a Double")
    }
    def bool = R { (c, w) => c match
      case Column.Bool(v, ok) => i => { present(ok, i, w, "a Boolean"); v(i) }
      case other => fail(w, s"${Column.describe(other)} where the schema has a Boolean")
    }
    def string = R { (c, w) => c match
      case Column.Utf8(v, ok) => i => { present(ok, i, w, "a String"); v(i) }
      case other => fail(w, s"${Column.describe(other)} where the schema has a String")
    }
    def char = R { (c, w) =>
      val s = string.at(c, w)
      i => { val x = s(i); if x.length != 1 then fail(w, s"\"$x\" where the schema has a Char") else x.charAt(0) }
    }
    def bytes = R { (c, w) => c match
      case Column.Binary(v, ok) => i => { present(ok, i, w, "bytes"); v(i) }
      case Column.FixedBinary(_, v, ok) => i => { present(ok, i, w, "bytes"); v(i) }
      case other => fail(w, s"${Column.describe(other)} where the schema has bytes")
    }
    def bigInt = R { (c, w) => c match
      case Column.Decimal(_, 0, v, ok) => i => { present(ok, i, w, "a BigInt"); v(i) }
      case other => longs(other, w, "a BigInt") match
        case (v, ok) => i => { present(ok, i, w, "a BigInt"); BigInt(v(i)) }
    }

    def option[A](o: Schema.SOption[A], of: () => R[A]) = R { (c, w) =>
      val inner = of().at(c, w)
      val ok = c.validity
      i => if ok(i) then Some(inner(i)) else None
    }

    private def seq[A](of: () => R[A], c: Column, w: String): Int => Iterator[A] = c match
      case Column.ListOf(offs, child, ok) =>
        val inner = of().at(child, s"$w.item")
        i => { present(ok, i, w, "a sequence"); (offs(i) until offs(i + 1)).iterator.map(inner) }
      case other => fail(w, s"${Column.describe(other)} where the schema has a sequence")
    def list[A](l: Schema.SList[A], of: () => R[A]) = R((c, w) => { val f = seq(of, c, w); i => f(i).toList })
    def vector[A](v: Schema.SVector[A], of: () => R[A]) = R((c, w) => { val f = seq(of, c, w); i => f(i).toVector })

    def iso[A, B](iso: Schema.SIso[A, B], under: () => R[B]) = R { (c, w) =>
      val u = under().at(c, w)
      i => iso.to(u(i)).fold(why => fail(w, why), identity)
    }

    /** a recursive type: its CBOR, beside the JSON Columns writes for engines */
    private def viaCbor[A](s: Schema[A]): R[A] = R { (c, w) => c match
      case Column.Struct(fs, ok) => fs.collectFirst { case ("cbor", Column.Binary(b, _)) => b } match
        case Some(b) => i => { present(ok, i, w, "a recursive value"); Cbor.read(b(i))(using s).fold(why => fail(w, why), identity) }
        case None => fail(w, "a recursive value without its cbor field")
      case other => fail(w, s"${Column.describe(other)} where the schema has a recursive value")
    }

    def product[A](p: Schema.SProduct[A], fields: Vector[(String, Schema.Edge[R, Any])]) =
      if recursive(p.name) then viaCbor(p)
      else if p.fields.isEmpty then R((_, _) => _ => p.make(Nil))       // Columns wrote `true`
      else R { (c, w) => c match
        case Column.Struct(fs, ok) =>
          val byName = fs.toMap
          val readers = fields.zipWithIndex.map { case ((name, e), j) =>
            byName.get(name) match
              case Some(col) => e().at(col, if w.isEmpty then name else s"$w.$name")
              case None => p.defaults.lift(j).flatten match
                case Some(d) => (_: Int) => d()
                case None => fail(w, s"no field '$name' for ${p.name}, and no default")
          }
          i => { present(ok, i, w, p.name); p.make(readers.map(_(i))) }
        case other => fail(w, s"${Column.describe(other)} where the schema has ${p.name}")
      }

    private def fieldless(s: Schema[?]): Boolean = s match
      case p: Schema.SProduct[?] => p.fields.isEmpty
      case _ => false

    def sum[A](su: Schema.SSum[A], cases: Vector[(String, Schema.Edge[R, A])]) =
      if recursive(su.name) then viaCbor(su)
      else
        val shapes = su.cases.map((n, c) => (n, c()))
        def index(name: String, w: String): Int =
          val k = su.cases.indexWhere(_._1 == name)
          if k < 0 then fail(w, s"'$name' is not a case of ${su.name} (${su.cases.map(_._1).mkString(", ")})") else k
        if shapes.forall((_, s) => fieldless(s)) then R { (c, w) =>
          val names = string.at(c, w)
          i => { val k = index(names(i), w); cases(k)._2().at(c, w)(i) }
        }
        else R { (c, w) => c match
          case Column.Struct(fs, ok) =>
            val byName = fs.toMap
            val kinds = string.at(byName.getOrElse("kind", fail(w, s"${su.name} without its kind field")), s"$w.kind")
            val branches = cases.zipWithIndex.map { case ((n, e), k) =>
              if fieldless(shapes(k)._2) then e().at(c, w)
              else e().at(byName.getOrElse(n, fail(w, s"${su.name} without its branch '$n'")), s"$w.$n")
            }
            i => { present(ok, i, w, su.name); branches(index(kinds(i), w))(i) }
          case other => fail(w, s"${Column.describe(other)} where the schema has ${su.name}")
        }

    def ref[A](name: String) = throw IllegalStateException(s"$name is recursive and was not found by recursiveNames — a defect")
