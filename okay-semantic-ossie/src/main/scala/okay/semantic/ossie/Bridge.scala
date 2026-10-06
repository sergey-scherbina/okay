package okay.semantic.ossie

import okay.semantic.{Model, Dimension, Measure, Origin, Calculation, Value, Kind}
import okay.semantic.Metric as CoreMetric
import Expressions.*

final case class FieldKey(dataset: String, field: String)
final case class Bindings[A](origin: Origin, grain: String, units: Map[String,String],
                             dimensions: Map[FieldKey,Dimension[A]], measures: Map[FieldKey,Measure[A]])

object Bridge:
  private enum Term:
    case Named(id: String)
    case Constant(value: BigDecimal)

  /** Application bindings read already-typed logical fields; no source SQL is executed. */
  def bind[A](document: Document, dataset: String, bindings: Bindings[A],
              metricNames: Vector[String] = Vector.empty, dialect: String = "ANSI_SQL"): Either[Vector[String],Model[A]] =
    val errors = Vector.newBuilder[String]
    if !Set("ANSI_SQL","OSSIE_SQL_2026")(dialect) then errors += s"execution dialect $dialect is unsupported"
    val fact = document.datasets.find(_.name == dataset)
    if fact.isEmpty then errors += s"unknown fact dataset $dataset"
    def metric(ref: Ref): Either[String,String] =
      if ref.parts.size != 1 then Left("metric references must be unqualified")
      else document.metrics.filter(m => ref.parts.head.matches(m.name)) match
        case Vector(m) => Right(m.name)
        case Vector() => Left(s"unknown metric ${ref.parts.head.value}")
        case _ => Left(s"ambiguous metric ${ref.parts.head.value}")
    def field(ref: Ref): Either[String,FieldKey] =
      val part = ref.parts.last
      if ref.parts.size > 2 then Left("field reference has too many qualifiers")
      else if ref.parts.size == 2 && !ref.parts.head.matches(dataset) then Left(s"cross-dataset field ${ref.parts.head.value}.${part.value}: use checked Lookup enrichment")
      else fact.toVector.flatMap(_.fields).filter(f => part.matches(f.name)) match
        case Vector(f) => Right(FieldKey(dataset,f.name))
        case Vector() => Left(s"unknown field $dataset.${part.value}")
        case _ => Left(s"ambiguous field $dataset.${part.value}")
    val requested = if metricNames.isEmpty then document.metrics.map(_.name) else metricNames
    val roots = requested.flatMap(n => metric(Ref(Vector(Part(n,false)))) match
      case Right(id) => Some(id)
      case Left(why) => errors += why; None)
    val todo = scala.collection.mutable.Queue.from(roots)
    val parsed = scala.collection.mutable.LinkedHashMap.empty[String,Vector[Token]]
    while todo.nonEmpty do
      val id = todo.dequeue()
      if !parsed.contains(id) then
        val definition = document.metrics.find(_.name == id).get
        val tokens = definition.expression.in(dialect).flatMap(Expressions.parse)
        parsed.update(id,tokens.getOrElse(Vector.empty))
        tokens match
          case Left(why) => errors += s"metric $id: $why"
          case Right(ts) => ts.foreach {
            case Token.MetricRef(ref) => metric(ref) match
              case Right(n) => if !parsed.contains(n) then todo.enqueue(n)
              case Left(why) => errors += s"metric $id: $why"
            case _ => ()
          }
    val core = Vector.newBuilder[CoreMetric]
    val reserved = scala.collection.mutable.Set.from(document.metrics.map(_.name))
    val boundMeasures = scala.collection.mutable.Map.from(bindings.measures)
    val neededMeasures = scala.collection.mutable.Set.empty[FieldKey]
    val neededDimensions = scala.collection.mutable.Set.empty[FieldKey]
    var serial = 0
    parsed.foreach { (id,tokens) =>
      val definition = document.metrics.find(_.name == id).get
      if definition.datatype.exists(t => Set("String","Boolean","Date","Time","DateTime","DateTimeTz")(t)) then
        errors += s"metric $id: numeric execution cannot produce declared ${definition.datatype.get}"
      val unit = bindings.units.getOrElse(id,"")
      if unit.trim.isEmpty then errors += s"metric $id: explicit unit required"
      def created(calc: Calculation): Term.Named =
        var fresh = s"__ossie_$serial"; serial += 1
        while reserved(fresh) do { fresh = s"__ossie_$serial"; serial += 1 }
        reserved += fresh
        core += CoreMetric(fresh,s"Intermediate for $id",unit,calc)
        Term.Named(fresh)
      var stack = List.empty[Term]
      def bad(why: String): Unit = errors += s"metric $id: $why"
      def named(term: Term): String = term match
        case Term.Named(n) => n
        case Term.Constant(n) => created(Calculation.Constant(n)).id
      def operation(left: Term,right: Term,op: Char): Term =
        val a = named(left); val b = named(right)
        created(op match
          case '+' => Calculation.Add(a,b)
          case '-' => Calculation.Subtract(a,b)
          case '*' => Calculation.Multiply(a,b)
          case _ => Calculation.Divide(a,b))
      tokens.foreach {
        case Token.Number(n) => stack = Term.Constant(n) :: stack
        case Token.MetricRef(ref) => metric(ref) match
          case Right(n) => stack = Term.Named(n) :: stack
          case Left(why) => bad(why)
        case Token.Aggregate(function,ref,distinct) =>
          val resolved = ref.map(field).getOrElse(Right(FieldKey(dataset,"")))
          resolved match
            case Left(why) => bad(why)
            case Right(key) =>
              if function == "COUNT" && !distinct && ref.nonEmpty && !boundMeasures.contains(key) then
                bindings.dimensions.get(key).foreach { d =>
                  boundMeasures.update(key,Measure(key.field,s"Non-null ${key.field}",a =>
                    if d.read(a) == Value.Null then None else Some(BigDecimal(1))))
                }
              val calculation: Option[Calculation] =
                if ref.isEmpty then Some(Calculation.Count())
                else if distinct then
                  if !bindings.dimensions.contains(key) then { bad(s"missing dimension binding $key"); None }
                  else { neededDimensions += key; Some(Calculation.Distinct(key.field)) }
                else if !boundMeasures.contains(key) then { bad(s"missing measure binding $key"); None }
                else
                  val datatype = fact.toVector.flatMap(_.fields).find(_.name == key.field).flatMap(_.datatype)
                  if function != "COUNT" && datatype.exists(t => Set("String","Boolean","Date","Time","DateTime","DateTimeTz")(t)) then
                    bad(s"numeric aggregate $function conflicts with declared ${datatype.get} field $key")
                  neededMeasures += key
                  Some(function match
                    case "SUM" => Calculation.Sum(key.field)
                    case "COUNT" => Calculation.Count(Some(key.field))
                    case "AVG" => Calculation.Average(key.field)
                    case "MIN" => Calculation.Minimum(key.field)
                    case _ => Calculation.Maximum(key.field))
              calculation.foreach(c => stack = created(c) :: stack)
        case Token.Operator('~') => stack match
          case Term.Constant(n) :: tail => stack = Term.Constant(-n) :: tail
          case Term.Named(n) :: tail => stack = created(Calculation.Scale(n,BigDecimal(-1))) :: tail
          case _ => bad("missing unary operand")
        case Token.Operator(op) => stack match
          case right :: left :: tail => stack = operation(left,right,op) :: tail
          case _ => bad("missing binary operands")
      }
      stack match
        case List(Term.Named(n)) => core += CoreMetric(id,definition.description.filter(_.trim.nonEmpty).getOrElse(id),unit,Calculation.Scale(n,BigDecimal(1)))
        case List(Term.Constant(n)) => core += CoreMetric(id,definition.description.getOrElse(id),unit,Calculation.Constant(n))
        case _ => bad("incomplete expression")
    }
    val available = fact.toVector.flatMap(_.fields).map(f => FieldKey(dataset,f.name)).toSet
    (bindings.dimensions.keySet ++ bindings.measures.keySet).filterNot(available).foreach(k => errors += s"binding $k is outside fact dataset $dataset")
    bindings.dimensions.foreach { (key,binding) =>
      fact.toVector.flatMap(_.fields).find(_.name == key.field).flatMap(_.datatype).foreach { t =>
        val expected = t match
          case "String" => Some(Kind.Text)
          case "Boolean" => Some(Kind.Bool)
          case "Integer" | "Decimal" | "Float" => Some(Kind.Number)
          case _ => None
        if expected.exists(_ != binding.kind) then errors += s"binding $key: declared $t conflicts with ${binding.kind}"
      }
    }
    val dimensions = bindings.dimensions.toVector.sortBy(_._1.field).map((k,d) => d.copy(id = k.field))
    val measures = neededMeasures.toVector.sortBy(_.field).map(k => boundMeasures(k).copy(id = k.field))
    val found = errors.result()
    if found.nonEmpty then Left(found) else Model.build(dataset,bindings.origin,bindings.grain,dimensions,measures,core.result())
