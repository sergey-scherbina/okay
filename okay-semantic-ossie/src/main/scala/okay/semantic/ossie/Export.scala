package okay.semantic.ossie

import okay.codec.Json
import Json.*
import okay.semantic.{Model, Kind, Calculation, TimeTransform, Origin, Dimension, Measure, Grain}

object Export:
  final case class Business(origin: Origin, grain: String, units: Map[String,String]):
    def bindings[A](dimensions: Map[FieldKey,Dimension[A]], measures: Map[FieldKey,Measure[A]]): Bindings[A] =
      Bindings(origin,grain,units,dimensions,measures)
  private def own(extensions: Vector[Extension]): Either[Vector[String],Json] =
    extensions.filter(_.vendor == "OKAY") match
      case Vector(one) => PortableSyntax.readJson(one.data)
      case Vector() => Left(Vector("missing OKAY extension"))
      case _ => Left(Vector("ambiguous OKAY extensions"))
  def business(document: Document): Either[Vector[String],Business] =
    own(document.extensions).flatMap { data =>
      val source = Read.string(data,"source"); val version = Read.string(data,"version"); val grain = Read.string(data,"grain")
      val errors = Vector.newBuilder[String]
      if Vector(source,version,grain).exists(_.trim.isEmpty) then errors += "OKAY business metadata requires source/version/grain"
      val units = document.metrics.flatMap { metric => own(metric.extensions) match
        case Left(why) => errors ++= why.map(w => s"metric ${metric.name}: $w"); None
        case Right(extension) =>
          val unit = Read.string(extension,"unit")
          if unit.trim.isEmpty then { errors += s"metric ${metric.name}: missing unit"; None }
          else Some(metric.name -> unit)
      }.toMap
      val found = errors.result()
      if found.nonEmpty then Left(found) else Right(Business(Origin(source,version),grain,units))
    }
  def temporal(field: Field): Either[Vector[String],Option[TimeTransform]] =
    own(field.extensions).flatMap { extension => Read.get(extension,"time") match
      case Some(JNull) | None => Right(None)
      case Some(value) => Read.string(value,"kind") match
        case "fixed" =>
          val width = Read.string(value,"width_micros").toLongOption
          val anchor = Read.string(value,"anchor_micros").toLongOption
          (width,anchor) match
            case (Some(w),Some(a)) if w > 0 => Right(Some(TimeTransform.Fixed(w,a)))
            case _ => Left(Vector(s"field ${field.name}: invalid fixed bucket metadata"))
        case "civil" =>
          val grain = Grain.values.find(_.toString == Read.string(value,"grain"))
          val zone = Read.string(value,"zone")
          if grain.isEmpty || zone.trim.isEmpty then Left(Vector(s"field ${field.name}: invalid civil metadata"))
          else Right(Some(TimeTransform.Civil(grain.get,zone)))
        case other => Left(Vector(s"field ${field.name}: unsupported time kind $other"))
    }
  private def obj(fields: (String,Json)*): Json = JObj(fields.toVector)
  private def array(values: Iterable[Json]): Json = JArr(values.toVector)
  private def expression(text: String): Json = obj("dialects" -> array(Vector(obj("dialect" -> JStr("ANSI_SQL"),"expression" -> JStr(text)))))
  private def quoted(name: String): String = "\"" + name.replace("\"","\"\"") + "\""
  private def extension(data: Json): Json = array(Vector(obj("vendor_name" -> JStr("OKAY"),"data" -> JStr(Json.print(data)))))

  /** Explicit columns describe storage; Scala extractor functions are never serialized. */
  def model[A](model: Model[A], source: String, columns: Map[String,String]): Either[Vector[String],Document] =
    val fieldNames = (model.dimensions.map(_.id) ++ model.measures.map(_.id)).distinct
    val errors = fieldNames.filterNot(columns.contains).map(n => s"export field $n: physical column required") ++
      Option.when(model.relations.nonEmpty)("export relationships: composite key bindings required; export the original Document instead").toVector ++
      Option.when(source.trim.isEmpty)("export source is blank").toVector
    val incompatible = model.dimensions.filter(d => model.measures.exists(_.id == d.id) && d.kind != Kind.Number)
      .map(d => s"export field ${d.id}: nonnumeric dimension conflicts with measure") ++
      columns.collect { case (name,column) if column.trim.isEmpty => s"export field $name: blank physical column" }.toVector
    if errors.nonEmpty || incompatible.nonEmpty then Left(errors ++ incompatible)
    else
      def field(name: String): String = quoted(model.id) + "." + quoted(name)
      val fields = fieldNames.map { name =>
        val dimension = model.dimensions.find(_.id == name)
        val measure = model.measures.find(_.id == name)
        val kind = dimension.map(_.kind).getOrElse(Kind.Number)
        val datatype = kind match
          case Kind.Text => "String"
          case Kind.Number => "Decimal"
          case Kind.Bool => "Boolean"
        val time = dimension.flatMap(_.time)
        val temporal: Json = time match
          case Some(TimeTransform.Fixed(width,anchor)) => obj("kind" -> JStr("fixed"),"width_micros" -> JStr(width.toString),"anchor_micros" -> JStr(anchor.toString))
          case Some(TimeTransform.Civil(grain,zone)) => obj("kind" -> JStr("civil"),"grain" -> JStr(grain.toString),"zone" -> JStr(zone))
          case None => JNull
        obj("name" -> JStr(name),"expression" -> expression(quoted(columns(name))),"datatype" -> JStr(datatype),
          "description" -> JStr(dimension.map(_.description).orElse(measure.map(_.description)).getOrElse(name)),
          "dimension" -> obj("is_time" -> JBool(time.nonEmpty)),"custom_extensions" -> extension(obj("time" -> temporal)))
      }
      val metrics = model.metrics.map { m =>
        def metric(name: String): String = quoted(name)
        val text = m.calculation match
          case Calculation.Constant(n) => n.bigDecimal.toPlainString
          case Calculation.Sum(n) => s"SUM(${field(n)})"
          case Calculation.Count(None) => "COUNT(*)"
          case Calculation.Count(Some(n)) => s"COUNT(${field(n)})"
          case Calculation.Average(n) => s"AVG(${field(n)})"
          case Calculation.Ratio(n,d) => s"SUM(${field(n)}) / SUM(${field(d)})"
          case Calculation.Minimum(n) => s"MIN(${field(n)})"
          case Calculation.Maximum(n) => s"MAX(${field(n)})"
          case Calculation.Distinct(n) => s"COUNT(DISTINCT ${field(n)})"
          case Calculation.Add(l,r) => s"${metric(l)} + ${metric(r)}"
          case Calculation.Subtract(l,r) => s"${metric(l)} - ${metric(r)}"
          case Calculation.Multiply(l,r) => s"${metric(l)} * ${metric(r)}"
          case Calculation.Divide(l,r) => s"${metric(l)} / ${metric(r)}"
          case Calculation.Scale(n,f) => s"${metric(n)} * ${f.bigDecimal.toPlainString}"
        obj("name" -> JStr(m.id),"expression" -> expression(text),"description" -> JStr(m.description),
          "datatype" -> JStr("Decimal"),"custom_extensions" -> extension(obj("unit" -> JStr(m.unit))))
      }
      Document.fromJson(obj("version" -> JStr(Document.version),"name" -> JStr(model.id),
        "datasets" -> array(Vector(obj("name" -> JStr(model.id),"source" -> JStr(source),"fields" -> array(fields)))),
        "metrics" -> array(metrics),"custom_extensions" -> extension(obj("grain" -> JStr(model.grain),
          "source" -> JStr(model.origin.source),"version" -> JStr(model.origin.version)))))
