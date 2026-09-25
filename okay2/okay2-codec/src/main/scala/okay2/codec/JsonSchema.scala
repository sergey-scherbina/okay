package okay2.codec

/**
 * A schema's JSON Schema (okay-codec's JsonSchema.scala), the first
 * algebra over `Schema.fold`: a product is an object with its fields
 * `required` unless optional or defaulted (a default is written), a sum
 * is a `oneOf` of one-entry objects (what `Json.write` produces), an iso
 * is its underlying schema plus `enum` when it has a vocabulary. A
 * RECURSIVE schema folds to a finite value: the back edge is a `$ref`,
 * and the node it names is declared under `$defs`.
 */
object JsonSchema {

  def of[A](s: Schema[A], vocabularies: Boolean = true): Json = {
    val alg = new Alg(vocabularies)
    val root = Schema.fold(s)(alg)
    if (alg.defs.isEmpty) root
    else root match {
      case Json.JObj(fs) => Json.JObj(fs :+ ("$defs" -> Json.JObj(alg.defs.toVector)))
      case other => other
    }
  }

  private type K[A] = Json

  private final class Alg(vocabularies: Boolean) extends Schema.Algebra[K] {
    val defs = scala.collection.mutable.LinkedHashMap.empty[String, Json]
    private val referenced = scala.collection.mutable.LinkedHashSet.empty[String]

    def int = obj("type" -> Json.JStr("integer"))
    def long = obj("type" -> Json.JStr("integer"))
    def double = obj("type" -> Json.JStr("number"))
    def bool = obj("type" -> Json.JStr("boolean"))
    def string = obj("type" -> Json.JStr("string"))
    def char = obj("type" -> Json.JStr("string"), "minLength" -> Json.JNum(1), "maxLength" -> Json.JNum(1))
    def bytes = obj("type" -> Json.JStr("string"), "contentEncoding" -> Json.JStr("base64"))
    def bigInt = obj("type" -> Json.JStr("string"), "pattern" -> Json.JStr("^-?[0-9]+$"))
    /** optionality is in the product's `required`, not here */
    def option[A](o: Schema.SOption[A], of: () => Json) = of()
    def list[A](l: Schema.SList[A], of: () => Json) = obj("type" -> Json.JStr("array"), "items" -> of())
    def vector[A](v: Schema.SVector[A], of: () => Json) = obj("type" -> Json.JStr("array"), "items" -> of())

    def product[A](p: Schema.SProduct[A], fields: Vector[(String, Schema.Edge[K, Any])]) = {
      def defaulted(i: Int) = p.defaults.lift(i).flatten
      val props = fields.zipWithIndex.map { case ((n, e), i) =>
        val base = e()
        (n, defaulted(i) match {
          case Some(_) => base match {
            case Json.JObj(fs) =>
              val written = p.defaultAt(i)(new Schema.DefaultFn[String] {
                def apply[X](sc: Schema[X], x: X): String = Json.encode(sc)(x)
              }).get
              Json.JObj(fs :+ ("default" -> Json.parse(written)))
            case other => other
          }
          case None => base
        })
      }
      val required = p.fields.zipWithIndex.collect {
        case ((n, f), i) if !f().isInstanceOf[Schema.SOption[_]] && defaulted(i).isEmpty => Json.JStr(n)
      }
      declared(p.name, obj("type" -> Json.JStr("object"), "properties" -> Json.JObj(props), "required" -> Json.JArr(required)))
    }

    def sum[A](su: Schema.SSum[A], cases: Vector[(String, Schema.Edge[K, A])]) =
      declared(su.name, obj("oneOf" -> Json.JArr(cases.map { case (n, e) =>
        obj("type" -> Json.JStr("object"), "properties" -> Json.JObj(Vector((n, e()))), "required" -> Json.JArr(Vector(Json.JStr(n))))
      })))

    def iso[A, B](iso: Schema.SIso[A, B], under: () => Json) = iso.vocabulary match {
      case Some(vs) if vocabularies => under() match {
        case Json.JObj(fs) => Json.JObj(fs :+ ("enum" -> Json.JArr(vs.map(v => Json.parse(Json.encode(iso.under())(v))))))
        case other => other
      }
      case _ => under()
    }

    def ref[A](name: String) = {
      referenced += name
      obj("$ref" -> Json.JStr(s"#/$$defs/$name"))
    }

    /** a named node another edge came back to is declared once, by name */
    private def declared(name: String, built: Json): Json = {
      if (referenced.contains(name)) defs(name) = built
      built
    }
  }

  private def obj(fs: (String, Json)*): Json = Json.JObj(fs.toVector)
}
