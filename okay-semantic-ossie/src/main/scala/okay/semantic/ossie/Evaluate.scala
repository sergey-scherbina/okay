package okay.semantic.ossie

import okay.semantic.Value
import Value.*
import Program.*

private[ossie] object Evaluate:
  private enum Column:
    case Constant(value: Value)
    case Rows(values: Vector[Value])
    case Groups(values: Vector[Value])
  def run(bound: Bound, rows: Vector[Map[FieldKey,Value]], groups: Vector[Vector[Int]], metrics: Map[String,Vector[Value]],
          functions: Functions): Either[Vector[String],Vector[Value]] =
    val errors = scala.collection.mutable.ArrayBuffer.empty[String]
    val columns = scala.collection.mutable.ArrayBuffer.empty[Column]
    def checked(value: Either[String,Value]): Value = value match
      case Right(v) => v
      case Left(e) => errors += e; Null
    def groupValues(column: Column): Vector[Value] = column match
      case Column.Constant(v) => Vector.fill(groups.size)(v)
      case Column.Groups(vs) => vs
      case Column.Rows(vs) => groups.map { indices =>
        val values = indices.map(vs).distinct
        if values.size > 1 then { errors += "row expression is not constant at requested group grain"; Null }
        else values.headOption.getOrElse(Null) }
    def rowValues(column: Column): Vector[Value] = column match
      case Column.Constant(v) => Vector.fill(rows.size)(v)
      case Column.Rows(vs) => vs
      case Column.Groups(_) => errors += "group/window result cannot be a row aggregate operand"; Vector.fill(rows.size)(Null)
    def scalar(args: Vector[Int])(f: Vector[Value] => Either[String,Value]): Column =
      val inputs = args.map(columns)
      if inputs.exists { case Column.Groups(_) => true; case _ => false } then
        val vs = inputs.map(groupValues)
        Column.Groups(groups.indices.map(i => checked(f(vs.map(_(i))))).toVector)
      else if inputs.exists { case Column.Rows(_) => true; case _ => false } then
        val vs = inputs.map(rowValues)
        Column.Rows(rows.indices.map(i => checked(f(vs.map(_(i))))).toVector)
      else Column.Constant(checked(f(inputs.collect { case Column.Constant(v) => v })))
    bound.tree.nodes.zipWithIndex.foreach { (node,index) =>
      val column = node match
        case Node.Literal(v) => Column.Constant(v)
        case Node.Reference(_) => bound.references(index) match
          case Reference.Field(k) =>
            if rows.exists(r => !r.contains(k)) then errors += s"missing field value $k"
            Column.Rows(rows.map(_.getOrElse(k,Null)))
          case Reference.Metric(n) =>
            metrics.get(n) match
              case Some(vs) => Column.Groups(vs)
              case None => errors += s"unavailable metric dependency $n"; Column.Groups(Vector.fill(groups.size)(Null))
        case Node.Unary(op,c) => scalar(Vector(c))(v => Scalar.unary(op,v.head))
        case Node.Binary(op,l,r) => scalar(Vector(l,r))(v => Scalar.binary(op,v(0),v(1)))
        case Node.Conditional(branches,otherwise) =>
          scalar(branches.flatMap((c,v) => Vector(c,v)) :+ otherwise) { values =>
            val conditions = branches.indices.map(i => values(2 * i))
            if conditions.exists { case Bool(_) | Null => false; case _ => true } then Left("CASE condition must be Boolean")
            else Right(conditions.indexOf(Bool(true)) match
              case -1 => values.last
              case i => values(2 * i + 1)) }
        case Node.Call(fn,args,distinct,filter,window) if window.nonEmpty =>
          val inputs = args.map(a => groupValues(columns(a)))
          val predicate = filter.map(a => groupValues(columns(a)))
          if predicate.exists(_.exists { case Bool(_) | Null => false; case _ => true }) then errors += "FILTER condition must be Boolean"
          val w = window.get
          val partition = w.partition.map(a => groupValues(columns(a)))
          val ordering = w.order.map(s => s -> groupValues(columns(s.node)))
          Column.Groups(windowValues(fn,inputs,distinct,predicate,w,partition,ordering,groups.size,checked))
        case Node.Call(fn,args,distinct,filter,_) if aggregates(fn) =>
          val inputs = args.map(a => rowValues(columns(a)))
          val predicate = filter.map(a => rowValues(columns(a)))
          if predicate.exists(_.exists { case Bool(_) | Null => false; case _ => true }) then errors += "FILTER condition must be Boolean"
          Column.Groups(groups.map { indices =>
            val selected = indices.filter(i => predicate.forall(_(i) == Bool(true)))
            if fn.startsWith("PERCENTILE_") then
              val p = inputs.headOption.flatMap(vs => selected.headOption.map(vs)).getOrElse(Null)
              if selected.exists(i => inputs.head(i) != p) then { errors += "percentile must be constant within group"; Null }
              else checked(aggregate(fn,inputs.lastOption.toVector.flatMap(vs => selected.map(vs)),distinct,p))
            else checked(aggregate(fn,if inputs.isEmpty then selected.map(_ => Number(BigDecimal(1))) else selected.map(inputs.head),distinct,Null))
          })
        case Node.Call(fn,args,_,_,_) => scalar(args)(functions.call(fn,_))
      columns += column
    }
    val result = groupValues(columns(bound.tree.root))
    if errors.nonEmpty then Left(errors.toVector.distinct) else Right(result)

  private def aggregate(fn: String, input: Vector[Value], distinct: Boolean, percentile: Value): Either[String,Value] =
    val values = (if distinct then input.distinct else input).filterNot(_ == Null)
    val ns = values.collect { case Number(n) => n }
    def numeric: Either[String,Vector[BigDecimal]] = if ns.size != values.size then Left(s"$fn requires numeric operands") else Right(ns)
    def sum(xs: Vector[BigDecimal]): BigDecimal = xs.foldLeft(BigDecimal(0))((a,b) => BigDecimal(a.bigDecimal.add(b.bigDecimal)))
    if fn == "COUNT" then Right(Number(BigDecimal(values.size)))
    else if values.isEmpty then Right(Null)
    else if fn == "MIN" || fn == "MAX" then
      if values.tail.exists(v => Value.compare(v,values.head).isEmpty) then Left(s"$fn incomparable operands")
      else Right(values.reduceLeft((a,b) => if (Value.compare(a,b).get >= 0) == (fn == "MAX") then a else b))
    else numeric.flatMap { numbers => fn match
      case "SUM" => Right(Number(sum(numbers)))
      case "AVG" => Right(Scalar.divide(sum(numbers),BigDecimal(numbers.size)))
      case "MEDIAN" | "PERCENTILE_CONT" | "PERCENTILE_DISC" =>
        val p = if fn == "MEDIAN" then Some(BigDecimal("0.5")) else percentile match
          case Number(n) => Some(n)
          case _ => None
        p.filter(n => n >= 0 && n <= 1).toRight("percentile must be a number in [0,1]").map { fraction =>
          val sorted = numbers.sorted
          if fn == "PERCENTILE_DISC" then
            val pos = BigDecimal(fraction.bigDecimal.multiply(java.math.BigDecimal.valueOf(sorted.size.toLong))).setScale(0,BigDecimal.RoundingMode.CEILING).toInt.max(1) - 1
            Number(sorted(pos))
          else
            val position = BigDecimal(fraction.bigDecimal.multiply(java.math.BigDecimal.valueOf((sorted.size - 1).toLong)))
            val low = position.setScale(0,BigDecimal.RoundingMode.FLOOR).toInt
            val high = position.setScale(0,BigDecimal.RoundingMode.CEILING).toInt
            val weight = BigDecimal(position.bigDecimal.subtract(java.math.BigDecimal.valueOf(low.toLong)))
            Number(BigDecimal(sorted(low).bigDecimal.add(sorted(high).bigDecimal.subtract(sorted(low).bigDecimal).multiply(weight.bigDecimal)))) }
      case _ =>
        val sample = !Set("STDDEV_POP","VAR_POP")(fn)
        val denominator = numbers.size - (if sample then 1 else 0)
        if denominator <= 0 then Right(Null)
        else
          val mean = Scalar.divide(sum(numbers),BigDecimal(numbers.size)) match
            case Number(n) => n
            case _ => BigDecimal(0)
          val squares = numbers.map(n => BigDecimal(n.bigDecimal.subtract(mean.bigDecimal).pow(2)))
          val variance = Scalar.divide(sum(squares),BigDecimal(denominator))
          if fn.startsWith("STDDEV") then variance match
            case Number(n) =>
              val root = math.sqrt(n.toDouble)
              if root.isNaN || root.isInfinity then Left(s"$fn exceeds finite floating-point range") else Right(Number(BigDecimal(root)))
            case _ => Right(Null)
          else Right(variance)
    }

  private def windowValues(fn: String, inputs: Vector[Vector[Value]], distinct: Boolean, predicate: Option[Vector[Value]],
      w: Window, partition: Vector[Vector[Value]], ordering: Vector[(Sort,Vector[Value])], count: Int,
      checked: Either[String,Value] => Value): Vector[Value] =
    val result = Array.fill[Value](count)(Null)
    val partitions = (0 until count).toVector.groupBy(i => partition.map(_(i)))
    def compare(a: Int,b: Int): Int =
      var c = 0
      val it = ordering.iterator
      while c == 0 && it.hasNext do
        val (sort,vs) = it.next(); val x = vs(a); val y = vs(b)
        if x == Null || y == Null then c = if x == y then 0 else if (x == Null) == sort.nullsFirst then -1 else 1
        else { c = Value.compare(x,y).getOrElse(checked(Left("window ordering operands are incomparable")) match { case _ => 0 }); if sort.descending then c = -c }
      c
    partitions.values.foreach { indices =>
      val sorted = if ordering.isEmpty then indices else indices.sortWith((a,b) => compare(a,b) < 0)
      var rank = 1
      var dense = 1
      sorted.indices.foreach { position =>
        val i = sorted(position)
        if position > 0 && compare(sorted(position - 1),i) != 0 then { rank = position + 1; dense += 1 }
        val bounds = w.frame.getOrElse(if ordering.isEmpty then (Int.MinValue,Int.MaxValue) else (Int.MinValue,0))
        def limit(bound: Int): Int = if bound == Int.MinValue then 0 else if bound == Int.MaxValue then sorted.size - 1 else (position.toLong + bound).max(0).min(sorted.size - 1).toInt
        val low = limit(bounds._1)
        // SQL default RANGE frame includes all ordering peers, not only this physical row.
        var high = limit(bounds._2)
        if w.frame.isEmpty && ordering.nonEmpty then
          while high + 1 < sorted.size && compare(sorted(high + 1),i) == 0 do high += 1
        val frame = if position.toLong + bounds._2 < 0 && bounds._2 != Int.MaxValue || position.toLong + bounds._1 >= sorted.size && bounds._1 != Int.MinValue then Vector.empty else sorted.slice(low,high + 1)
        result(i) = fn match
          case "ROW_NUMBER" => Number(BigDecimal(position + 1))
          case "RANK" => Number(BigDecimal(rank))
          case "DENSE_RANK" => Number(BigDecimal(dense))
          case "NTILE" => inputs.head(i) match
            case Number(n) if n.isWhole && n.isValidInt && n > 0 =>
              val buckets = n.toInt
              val size = sorted.size / buckets; val extra = sorted.size % buckets
              val largeRows = (size + 1) * extra
              Number(BigDecimal(if position < largeRows then position / (size + 1) + 1 else (position - largeRows) / size + extra + 1))
            case _ => checked(Left("NTILE needs a positive integral bucket count"))
          case "LAG" | "LEAD" =>
            val offset = if inputs.size < 2 then Some(1) else inputs(1)(i) match
              case Number(n) if n.isWhole && n.isValidInt && n >= 0 => Some(n.toInt)
              case _ => None
            offset match
              case None => checked(Left("LAG/LEAD requires nonnegative integral offset"))
              case Some(n) =>
                val target = position.toLong + (if fn == "LAG" then -n.toLong else n.toLong)
                if target < 0 || target >= sorted.size then if inputs.size == 3 then inputs(2)(i) else Null
                else inputs.head(sorted(target.toInt))
          case "FIRST_VALUE" => frame.headOption.fold[Value](Null)(inputs.head)
          case "LAST_VALUE" => frame.lastOption.fold[Value](Null)(inputs.head)
          case _ =>
            val selected = frame.filter(j => predicate.forall(_(j) == Bool(true)))
            val values = if inputs.isEmpty then selected.map(_ => Number(BigDecimal(1))) else selected.map(inputs.last)
            checked(aggregate(fn,values,distinct,if fn.startsWith("PERCENTILE_") then inputs.head(i) else Null))
      }
    }
    result.toVector
