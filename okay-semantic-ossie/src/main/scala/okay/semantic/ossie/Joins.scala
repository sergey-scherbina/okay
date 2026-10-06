package okay.semantic.ossie

import okay.semantic.{Cardinality, Catalog, Entity, Relation, Value}

private[ossie] object Joins:
  def route(doc: Document, from: String, to: String, via: Vector[String]): Either[Vector[String],Vector[Relationship]] =
    Catalog.build(doc.datasets.map(d => Entity(d.name,d.name)),doc.relationships.map(r =>
      Relation(r.name,r.from,r.to,Cardinality.ManyToOne,r.name))).flatMap(_.route(from,to,via))
      .map(rs => rs.flatMap(r => doc.relationships.find(_.name == r.id)))
  def index(doc: Document, relations: Vector[Relationship], tables: Map[String,Table], budget: Int, required: Set[FieldKey])
      : Either[Vector[String],Map[String,Map[Vector[Value],Map[String,Value]]]] =
    val errors = Vector.newBuilder[String]
    val out = relations.flatMap { r => tables.get(r.to) match
      case None => errors += s"relation ${r.name}: missing table ${r.to}"; None
      case Some(table) =>
        if table.dataset != r.to then errors += s"table ${r.to}: dataset name mismatch"
        if table.rows.size > budget then errors += s"table ${r.to}: row budget exceeded"
        val indexed = scala.collection.mutable.Map.empty[Vector[Value],Map[String,Value]]
        val fields = doc.datasets.find(_.name == r.to).toVector.flatMap(_.fields)
        table.rows.take(budget).zipWithIndex.foreach { (row,i) =>
          required.filter(_.dataset == r.to).filterNot(k => row.contains(k.field)).foreach(k => errors += s"table ${r.to}, row $i: missing field ${k.field}")
          r.toColumns.filterNot(row.contains).foreach(k => errors += s"relation ${r.name}, row $i: missing key $k")
          fields.foreach(f => row.get(f.name).foreach { v =>
            val valid = (f.datatype,v) match
              case (_,Value.Null) => true
              case (Some("Integer"),Value.Number(n)) => n.isWhole
              case (Some("Decimal" | "Float"),Value.Number(_)) => true
              case (Some("Boolean"),Value.Bool(_)) => true
              case (Some("String" | "Date" | "Time" | "DateTime" | "DateTimeTz"),Value.Text(_)) => true
              case (None | Some("Opaque"),_) => true
              case _ => false
            if !valid then errors += s"table ${r.to}, row $i: field ${f.name} violates declared type"
          })
          val key = r.toColumns.map(n => row.getOrElse(n,Value.Null))
          if !key.contains(Value.Null) then
            if indexed.contains(key) then errors += s"relation ${r.name}: duplicate right key $key; fanout requires allocation"
            else indexed.update(key,row)
        }
        Some(r.name -> indexed.toMap)
    }.toMap
    val found = errors.result()
    if found.nonEmpty then Left(found) else Right(out)
