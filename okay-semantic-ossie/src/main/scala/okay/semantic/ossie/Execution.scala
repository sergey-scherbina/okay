package okay.semantic.ossie

import okay.{Async, Bulk, Source, Tables}
import okay.freer.{!, Aggregator}
import okay.std.{Chunk, Writer}
import okay.codec.{Json, Schema}
import okay.semantic.{Calculation, Dimension, Group, Kind, Model, Request, Result, Value}
import okay.semantic.Metric as CoreMetric
import Program.*

/** Logical lookup rows; Table.of keeps each application's source type checked. */
final case class Table(dataset: String, rows: Vector[Map[String,Value]])
object Table:
  def of[A](dataset: String, rows: IterableOnce[A], fields: Map[String,A => Value], maxRows: Int = 1000000): Either[Vector[String],Table] =
    if maxRows < 0 then Left(Vector("negative table row budget"))
    else
      val it = rows.iterator
      val out = Vector.newBuilder[Map[String,Value]]
      var n = 0
      while it.hasNext && n < maxRows do { val a = it.next(); out += fields.map((k,f) => k -> f(a)); n += 1 }
      if it.hasNext then Left(Vector(s"table $dataset exceeds row budget $maxRows")) else Right(Table(dataset,out.result()))

private[ossie] enum Reference:
  case Field(key: FieldKey)
  case Metric(name: String)
private[ossie] final case class Bound(tree: Tree, references: Map[Int,Reference])

object Execution:
  def bind[A](document: Document, dataset: String, bindings: Bindings[A], metricNames: Vector[String] = Vector.empty,
              dialect: String = "ANSI_SQL", functions: Functions = Functions.portable,
              language: Language = Language.portable): Either[Vector[String],ExpressionModel[A]] =
    val errors = scala.collection.mutable.ArrayBuffer.empty[String]
    if !document.datasets.exists(_.name == dataset) then errors += s"unknown dataset $dataset"
    val todo = scala.collection.mutable.Queue.from(if metricNames.isEmpty then document.metrics.map(_.name) else metricNames)
    val parsed = scala.collection.mutable.LinkedHashMap.empty[String,Bound]
    val dependencies = scala.collection.mutable.Map.empty[String,Vector[String]]
    while todo.nonEmpty do
      val id = todo.dequeue()
      if !parsed.contains(id) then document.metrics.find(_.name == id) match
        case None => errors += s"unknown metric $id"
        case Some(m) =>
          val result = m.expression.in(dialect).flatMap(language.normalize(dialect,_)).flatMap(Program.parse)
          result match
            case Left(why) => errors += s"metric $id: $why"; parsed.update(id,Bound(Tree(Vector.empty,0),Map.empty))
            case Right(tree) =>
              val refs = scala.collection.mutable.Map.empty[Int,Reference]
              val work = scala.collection.mutable.Stack(tree.root -> false)
              while work.nonEmpty do
                val (index,row) = work.pop()
                tree.nodes(index) match
                  case Node.Reference(n) =>
                    resolve(document,dataset,n,row) match
                      case Left(why) => errors += s"metric $id: $why"
                      case Right(r) => refs.update(index,r)
                  case Node.Call(fn,args,distinct,filter,window) =>
                    val aggregate = aggregates(fn)
                    if !aggregate && !windows(fn) && !functions.accepts(fn,args.size) then errors += s"metric $id: unknown function/arity $fn/${args.size}"
                    if windows(fn) && window.isEmpty then errors += s"metric $id: $fn requires OVER"
                    if aggregate && (if fn == "COUNT" then args.size > 1 else if fn.startsWith("PERCENTILE_") then args.size != 2 else args.size != 1) then errors += s"metric $id: invalid aggregate arity $fn"
                    if distinct && (!aggregate || args.isEmpty) then errors += s"metric $id: DISTINCT needs an aggregate operand"
                    if filter.nonEmpty && !aggregate then errors += s"metric $id: FILTER needs an aggregate"
                    if window.exists(w => w.frame.exists((a,b) => a > b)) then errors += s"metric $id: reversed ROWS frame"
                    if window.nonEmpty && !aggregate && !windows(fn) then errors += s"metric $id: function $fn is not a window function"
                    if windows(fn) && !validWindowArity(fn,args.size) then errors += s"metric $id: invalid window arity $fn"
                    args.foreach(a => work.push(a -> (if aggregate && window.isEmpty then true else row)))
                    filter.foreach(f => work.push(f -> (window.isEmpty || row)))
                    window.foreach(w => (w.partition ++ w.order.map(_.node)).foreach(a => work.push(a -> false)))
                  case node => node.children.foreach(a => work.push(a -> row))
              if m.datatype.exists(Set("String","Boolean","Date","Time","DateTime","DateTimeTz")) then errors += s"metric $id: numeric result required by Result"
              val deps = refs.values.collect { case Reference.Metric(n) => n }.toVector.distinct
              dependencies.update(id,deps); deps.foreach(todo.enqueue(_))
              parsed.update(id,Bound(tree,refs.toMap))
              if bindings.units.get(id).forall(_.trim.isEmpty) then errors += s"metric $id: explicit unit required"
    val ready = scala.collection.mutable.Queue.from(parsed.keys.filter(n => dependencies.getOrElse(n,Vector.empty).isEmpty))
    val degrees = scala.collection.mutable.Map.from(parsed.keys.map(n => n -> dependencies.getOrElse(n,Vector.empty).size))
    val dependents = dependencies.toVector.flatMap((n,ds) => ds.map(_ -> n)).groupMap(_._1)(_._2)
    val order = Vector.newBuilder[String]
    var visited = 0
    while ready.nonEmpty do
      val n = ready.dequeue(); order += n; visited += 1
      dependents.getOrElse(n,Vector.empty).foreach { d => degrees(d) -= 1; if degrees(d) == 0 then ready.enqueue(d) }
    if visited != parsed.size then errors += "metric cycle"
    parsed.values.flatMap(_.references.values).collect { case Reference.Field(k) => k }.toSet.foreach { k =>
      if k.dataset == dataset && !bindings.dimensions.contains(k) && !bindings.measures.contains(k) then errors += s"missing field binding $k"
    }
    if errors.nonEmpty then Left(errors.toVector)
    else Right(new ExpressionModel(document,dataset,bindings,parsed.toMap,order.result(),functions))

  private def validWindowArity(fn: String, n: Int): Boolean = fn match
    case "ROW_NUMBER" | "RANK" | "DENSE_RANK" => n == 0
    case "LAG" | "LEAD" => n >= 1 && n <= 3
    case _ => n == 1
  private[ossie] def resolve(document: Document, dataset: String, name: Name, row: Boolean): Either[String,Reference] =
    val metrics = if row || name.parts.size != 1 then Vector.empty else document.metrics.filter(m => name.parts.head.matches(m.name))
    if metrics.size == 1 then Right(Reference.Metric(metrics.head.name))
    else if metrics.size > 1 then Left(s"ambiguous metric ${name.text}")
    else
      val ds = if name.parts.size == 1 then document.datasets.filter(_.name == dataset)
        else if name.parts.size == 2 then document.datasets.filter(d => name.parts.head.matches(d.name)) else Vector.empty
      val fields = ds.flatMap(d => d.fields.filter(f => name.parts.last.matches(f.name)).map(f => FieldKey(d.name,f.name)))
      if fields.size == 1 then Right(Reference.Field(fields.head))
      else Left(s"${if fields.isEmpty then "unknown" else "ambiguous"} field ${name.text}")

final class ExpressionModel[A] private[ossie] (val document: Document, val dataset: String, val bindings: Bindings[A],
    private[ossie] val programs: Map[String,Bound], private[ossie] val ordered: Vector[String], private[ossie] val functions: Functions) extends Serializable:
  def plan(request: Request, via: Vector[String] = Vector.empty, maxRows: Int = 1000000): Either[Vector[String],ExpressionPlan[A]] =
    val errors = scala.collection.mutable.ArrayBuffer.empty[String]
    val dimensionNames = (request.dimensions ++ request.filters.map(_.dimension)).distinct
    val dimensions = dimensionNames.flatMap { id =>
      Program.parse(id).flatMap(t => t.nodes(t.root) match
        case Node.Reference(n) => Execution.resolve(document,dataset,n,true)
        case _ => Left("dimension must be a field reference")) match
        case Right(Reference.Field(k)) =>
          val kind = bindings.dimensions.get(k).map(_.kind).getOrElse {
            val typ = document.datasets.find(_.name == k.dataset).toVector.flatMap(_.fields).find(_.name == k.field).flatMap(_.datatype)
            if typ.contains("Boolean") then Kind.Bool else if typ.exists(Set("Integer","Decimal","Float")) then Kind.Number else Kind.Text }
          Some((id,k,kind))
        case Right(_) => errors += s"dimension $id: expected field"; None
        case Left(e) => errors += s"dimension $id: $e"; None
    }
    val needed = scala.collection.mutable.Set.empty[String]
    val queue = scala.collection.mutable.Queue.from(request.metrics)
    while queue.nonEmpty do
      val n = queue.dequeue()
      if !needed(n) then
        needed += n
        programs.get(n) match
          case None => errors += s"unknown metric $n"
          case Some(b) => b.references.values.collect { case Reference.Metric(m) => m }.foreach(queue.enqueue(_))
    val selected = ordered.filter(needed)
    // Validate row/group levels before touching the source, including window grain.
    val levels = scala.collection.mutable.Map.empty[String,Int]
    val grouping = dimensions.filter(d => request.dimensions.contains(d._1)).map(_._2).toSet
    val fact = document.datasets.find(_.name == dataset).get
    val uniqueGrain = fact.primaryKey.nonEmpty && fact.primaryKey.forall(n => grouping(FieldKey(dataset,n)))
    val groupFields = if uniqueGrain then document.datasets.flatMap(d => d.fields.map(f => FieldKey(d.name,f.name))).toSet else grouping
    selected.foreach { id =>
      val b = programs(id)
      val ranks = scala.collection.mutable.ArrayBuffer.empty[Int]
      val units = scala.collection.mutable.ArrayBuffer.empty[Option[String]]
      b.tree.nodes.zipWithIndex.foreach { (node,index) =>
        val rank = node match
          case Node.Literal(_) => -1
          case Node.Reference(_) => b.references(index) match
            case Reference.Field(_) => 0
            case Reference.Metric(m) => levels.getOrElse(m,1)
          case Node.Call(fn,args,_,filter,window) if aggregates(fn) || window.nonEmpty =>
            if window.isEmpty && (args ++ filter.toVector).exists(a => ranks(a) > 0) then errors += s"metric $id: nested aggregate requires OVER"
            if window.nonEmpty && node.children.exists(a => ranks(a) == 2) then errors += s"metric $id: nested window requires another query stage"
            if window.isEmpty then 1 else 2
          case _ => node.children.map(ranks).maxOption.getOrElse(-1)
        if rank > 0 then node.children.foreach { a =>
          if ranks(a) == 0 && !(node match
            case Node.Call(fn,_,_,_,None) if aggregates(fn) => true
            case _ => false) then
            val work = scala.collection.mutable.Stack(a)
            while work.nonEmpty do b.tree.nodes(work.pop()) match
              case Node.Reference(n) => Execution.resolve(document,dataset,n,true).foreach {
                case Reference.Field(k) if !groupFields(k) => errors += s"metric $id: ungrouped field ${n.text}"
                case _ => () }
              case child => child.children.foreach(work.push(_))
        }
        val unit = node match
          case Node.Reference(_) => b.references(index) match
            case Reference.Metric(n) => bindings.units.get(n)
            case _ => None
          case Node.Binary(op,l,r) if op == "+" || op == "-" =>
            if units(l).nonEmpty && units(r).nonEmpty && units(l) != units(r) then errors += s"metric $id: incompatible additive units"
            units(l).orElse(units(r))
          case Node.Unary("-",c) => units(c)
          case Node.Call(fn,args,_,_,_) if Set("ABS","ROUND","TRUNC","TRUNCATE","FLOOR","CEIL","CEILING","SUM","AVG","MIN","MAX","MEDIAN","LAG","LEAD","FIRST_VALUE","LAST_VALUE")(fn) =>
            args.headOption.flatMap(units).orElse(if aggregates(fn) then bindings.units.get(id) else None)
          case _ => None
        units += unit
        ranks += rank
      }
      if ranks(b.tree.root) == 0 && !uniqueGrain then errors += s"metric $id: row-level metric requires an aggregate"
      if units(b.tree.root).exists(u => bindings.units.get(id).exists(_ != u)) then errors += s"metric $id: result unit differs from its operands"
      levels.update(id,ranks(b.tree.root).max(1))
    }
    val fields = selected.flatMap(n => programs(n).references.values.collect { case Reference.Field(k) => k }) ++ dimensions.map(_._2)
    val external = fields.map(_.dataset).filterNot(_ == dataset).distinct.filterNot(ds => fields.filter(_.dataset == ds).forall(k => bindings.dimensions.contains(k) || bindings.measures.contains(k)))
    val routes = external.flatMap(ds => Joins.route(document,dataset,ds,via) match
      case Left(es) => errors ++= es; None
      case Right(rs) => Some(ds -> rs))
    if maxRows < 0 || maxRows == Int.MaxValue then errors += "expression row budget must be in [0, Int.MaxValue)"
    val shell = Model.build[Unit](document.name,bindings.origin,bindings.grain,
      dimensions.map((id,_,kind) => Dimension[Unit](id,id,kind,_ => Value.Null)),Vector.empty,
      programs.keys.toVector.map(n => CoreMetric(n,n,bindings.units(n),Calculation.Constant(BigDecimal(0)))))
      .flatMap(_.plan(request))
    errors ++= shell.left.toOption.toVector.flatten
    if errors.nonEmpty then Left(errors.toVector.distinct)
    else Right(new ExpressionPlan(this,request,selected,dimensions.map((id,key,_) => id -> key).toMap,routes.toMap,shell.toOption.get,maxRows))

final class ExpressionPlan[A] private[ossie] (val model: ExpressionModel[A], val request: Request,
    selected: Vector[String], dimensions: Map[String,FieldKey], routes: Map[String,Vector[Relationship]],
    shell: okay.semantic.Plan[Unit], val maxRows: Int) extends Serializable:
  def explain: String = s"portable expression plan; dataset ${model.dataset}; row -> group -> window -> having/order/page; row budget $maxRows; metrics ${selected.mkString(", ")}; joins ${routes.values.flatten.map(_.name).toVector.distinct.mkString(", ")}"
  def run(rows: IterableOnce[A], tables: Map[String,Table] = Map.empty): Either[Vector[String],Result] =
    val errors = scala.collection.mutable.ArrayBuffer.empty[String]
    val relationships = routes.values.flatten.toVector.distinct
    val neededFields = (selected.flatMap(n => model.programs(n).references.values.collect { case Reference.Field(k) => k }) ++
      dimensions.values ++ relationships.flatMap(r => r.fromColumns.map(n => FieldKey(r.from,n)))).toSet
    val indexed = Joins.index(model.document,relationships,tables,maxRows,neededFields)
    indexed match
      case Left(es) => Left(es)
      case Right(indices) =>
        val records = Vector.newBuilder[Map[FieldKey,Value]]
        val it = rows.iterator
        var count = 0
        val primary = model.document.datasets.find(_.name == model.dataset).get.primaryKey
        val rowGrain = primary.nonEmpty && primary.forall(n => request.dimensions.exists(d => dimensions.get(d).contains(FieldKey(model.dataset,n))))
        val seenKeys = scala.collection.mutable.Set.empty[Vector[Value]]
        while it.hasNext && count < maxRows do
          val a = it.next()
          var record = model.bindings.dimensions.map((k,d) => k -> d.read(a)) ++
            model.bindings.measures.map((k,m) => k -> m.read(a).fold[Value](Value.Null)(Value.Number.apply))
          record.foreach { (k,v) =>
            if neededFields(k) then model.document.datasets.find(_.name == k.dataset).toVector.flatMap(_.fields).find(_.name == k.field).foreach { f =>
              if !Joins.accepts(f.datatype,v) then errors += s"row $count: field $k violates declared type ${f.datatype}"
            }
          }
          model.bindings.dimensions.foreach { (k,d) =>
            if !record(k).fits(d.kind) then errors += s"row $count: field $k expects ${d.kind}"
          }
          if rowGrain then
            val key = primary.map(n => record.getOrElse(FieldKey(model.dataset,n),Value.Null))
            if key.contains(Value.Null) || !seenKeys.add(key) then errors += s"row $count: primary key required/duplicate at requested row grain"
          routes.values.flatten.toVector.distinct.foreach { r =>
            val key = r.fromColumns.map(n => record.getOrElse(FieldKey(r.from,n),Value.Null))
            if r.fromColumns.exists(n => !record.contains(FieldKey(r.from,n))) then errors += s"missing join-key binding ${r.name}"
            val right = if key.contains(Value.Null) then None else indices(r.name).get(key)
            model.document.datasets.find(_.name == r.to).foreach(_.fields.foreach(f =>
              record = record.updated(FieldKey(r.to,f.name),right.flatMap(_.get(f.name)).getOrElse(Value.Null))))
          }
          if request.filters.forall(f => f.accepts(record.getOrElse(dimensions(f.dimension),Value.Null))) then records += record
          count += 1
        if it.hasNext then errors += s"input exceeds expression row budget $maxRows"
        val found = records.result()
        val grouped = scala.collection.mutable.LinkedHashMap.empty[Vector[Value],Vector[Int]]
        if request.dimensions.isEmpty then grouped.update(Vector.empty,Vector.empty)
        found.zipWithIndex.foreach { (r,i) =>
          val key = request.dimensions.map(d => r.getOrElse(dimensions(d),Value.Null))
          grouped.update(key,grouped.getOrElse(key,Vector.empty) :+ i)
        }
        val keys = grouped.keys.toVector
        val groups = grouped.values.toVector
        val values = scala.collection.mutable.Map.empty[String,Vector[Value]]
        selected.foreach { id =>
          Evaluate.run(model.programs(id),found,groups,values.toMap,model.functions) match
            case Left(es) => errors ++= es.map(e => s"metric $id: $e")
            case Right(vs) => values.update(id,vs)
        }
        if errors.nonEmpty then Left(errors.toVector.distinct)
        else
          val result = keys.indices.map { i =>
            val numbers = request.metrics.map(n => values(n)(i) match
              case Value.Number(v) => Some(v)
              case Value.Null => None
              case v => errors += s"metric $n: numeric result required, got $v"; None)
            Group(keys(i),numbers)
          }.toVector
          if errors.nonEmpty then Left(errors.toVector.distinct) else Right(shell.result(result))
  def source(rows: Source[A], tables: Map[String,Table] = Map.empty): Either[Vector[String],Result] ! Async =
    Writer.loopWith[A,Vector[A],Unit,Either[Vector[String],Result],Async](rows)(Vector.empty)(
      (acc,a) => if acc.size <= maxRows then acc :+ a else acc)((acc,_) => run(acc,tables))
  def chunks(rows: Source[Chunk[A]], tables: Map[String,Table] = Map.empty): Either[Vector[String],Result] ! Async =
    Writer.loopWith[Chunk[A],Vector[A],Unit,Either[Vector[String],Result],Async](rows)(Vector.empty)(
      (acc,c) => (acc ++ c.iterator.take((maxRows + 1 - acc.size).max(0))).take(maxRows + 1))((acc,_) => run(acc,tables))
  def bulk[D[_]](rows: D[A], tables: Map[String,Table] = Map.empty)(using backend: Bulk[D]): Either[Vector[String],Result] =
    backend.aggregate(rows)(aggregator(tables))
  def table(rows: Tables.Table[A], lookups: Map[String,Table] = Map.empty): Either[Vector[String],Result] ! Tables =
    rows.aggregate(aggregator(lookups))
  private def aggregator(tables: Map[String,Table]): Aggregator[A,Vector[A],Either[Vector[String],Result]] = new:
    def init: Vector[A] = Vector.empty
    def add(acc: Vector[A],a: A): Vector[A] = if acc.size <= maxRows then acc :+ a else acc
    def merge(a: Vector[A],b: Vector[A]): Vector[A] = (a ++ b.take((maxRows + 1 - a.size).max(0))).take(maxRows + 1)
    def present(acc: Vector[A]): Either[Vector[String],Result] = ExpressionPlan.this.run(acc,tables)
  def json(rows: IterableOnce[Json], tables: Map[String,Table] = Map.empty)(using schema: Schema[A]): Either[Vector[String],Result] =
    val decoded = rows.iterator.take(maxRows + 1).map(Json.decode(schema)).toVector
    val failures = decoded.zipWithIndex.flatMap((r,i) => r.left.toOption.map(e => s"JSON row $i: $e"))
    if failures.nonEmpty then Left(failures) else run(decoded.flatMap(_.toOption),tables)
