package okay.semantic.ossie

import okay.codec.Json
import Json.*

/** Validator for the pinned core schema's finite vocabulary, not a general JSON Schema engine. */
private[ossie] object Validate:
  private lazy val schema = Json.parse(Pinned.schemaText)
  private case class Check(path: String, rule: Json, value: Json)
  private def matches(value: Json, kind: String): Boolean = (value,kind) match
    case (_: JObj,"object") | (_: JArr,"array") | (_: JStr,"string") | (_: JBool,"boolean") => true
    case _ => false
  def shape(value: Json): Vector[String] =
    val errors = Vector.newBuilder[String]
    var trees = List("$" -> value)
    while trees.nonEmpty do
      val (path,node) = trees.head; trees = trees.tail
      node match
        case JObj(fs) =>
          duplicates(path + " property",fs.map(_._1)).foreach(errors += _)
          trees = fs.map((k,v) => s"$path.$k" -> v).toList ::: trees
        case JArr(vs) => trees = vs.zipWithIndex.map((v,i) => s"$path[$i]" -> v).toList ::: trees
        case JErr(why) => errors += s"$path: $why"
        case JNum(n) if n.isNaN || n.isInfinity => errors += s"$path: nonfinite numeric metadata"
        case _ => ()
    var work = List(Check("$",schema,value))
    while work.nonEmpty do
      val check = work.head; work = work.tail
      val path = check.path; val node = check.value
      var rule = check.rule
      while Read.optionalString(rule,"$ref").nonEmpty do
        val ref = Read.optionalString(rule,"$ref").get
        rule = ref.stripPrefix("#/").split('/').foldLeft(schema)((v,k) => Read.get(v,k).getOrElse(JNull))
      val alternatives = Read.array(rule,"oneOf")
      if alternatives.nonEmpty then
        alternatives.find(r => Read.optionalString(r,"type").exists(matches(node,_))) match
          case Some(r) => work = Check(path,r,node) :: work
          case None => errors += s"$path: expected string or object"
      else
        val kind = Read.optionalString(rule,"type")
        if kind.exists(k => !matches(node,k)) then errors += s"$path: expected ${kind.get}"
        else
          Read.get(rule,"const").foreach(c => if c != node then errors += s"$path: expected ${Json.print(c)}")
          val enums = Read.array(rule,"enum")
          if enums.nonEmpty && !enums.contains(node) then errors += s"$path: unsupported value ${Json.print(node)}"
          node match
            case JObj(fs) =>
              val properties = Read.get(rule,"properties").getOrElse(JObj(Vector.empty))
              Read.strings(rule,"required").filterNot(k => fs.exists(_._1 == k)).foreach(k => errors += s"$path.$k: required")
              val strict = Read.get(rule,"additionalProperties").contains(JBool(false))
              fs.foreach { (key,v) => Read.get(properties,key) match
                case Some(r) => work = Check(s"$path.$key",r,v) :: work
                case None if strict => errors += s"$path.$key: unknown property"
                case _ => ()
              }
            case JArr(vs) =>
              Read.get(rule,"minItems").collect { case JNum(n) => n.toInt }.foreach(n => if vs.size < n then errors += s"$path: at least $n items required")
              Read.get(rule,"items").foreach(r => vs.zipWithIndex.foreach((v,i) => work = Check(s"$path[$i]",r,v) :: work))
            case JStr(s) =>
              Read.get(rule,"minLength").collect { case JNum(n) => n.toInt }.foreach(n => if s.length < n then errors += s"$path: empty identifier")
            case _ => ()
    errors.result()
  private def duplicates(label: String,names: Vector[String]): Vector[String] =
    names.groupMapReduce(identity)(_ => 1)(_ + _).toVector.collect { case (n,count) if count > 1 => s"$label: duplicate $n" }
  def references(doc: Document): Vector[String] =
    val errors = Vector.newBuilder[String]
    errors ++= duplicates("dataset",doc.datasets.map(_.name))
    errors ++= duplicates("relationship",doc.relationships.map(_.name))
    errors ++= duplicates("metric",doc.metrics.map(_.name))
    def expression(label: String,e: Expression): Unit = errors ++= duplicates(label + " dialect",e.dialects.map(_.dialect))
    doc.datasets.foreach { d =>
      errors ++= duplicates(s"dataset ${d.name} field",d.fields.map(_.name))
      d.fields.foreach(f => expression(s"field ${d.name}.${f.name}",f.expression))
      (Vector(d.primaryKey) ++ d.uniqueKeys).foreach { key =>
        errors ++= duplicates(s"dataset ${d.name} key",key)
        key.filterNot(k => d.fields.exists(_.name == k)).foreach(k => errors += s"dataset ${d.name}: unknown key field $k")
      }
      if d.uniqueKeys.exists(_.isEmpty) then errors += s"dataset ${d.name}: empty unique key"
    }
    doc.metrics.foreach(m => expression(s"metric ${m.name}",m.expression))
    doc.relationships.foreach { r =>
      if r.fromColumns.size != r.toColumns.size then errors += s"relationship ${r.name}: composite key arity mismatch"
      Vector((r.from,r.fromColumns),(r.to,r.toColumns)).foreach { (name,columns) =>
        doc.datasets.find(_.name == name) match
          case None => errors += s"relationship ${r.name}: unknown dataset $name"
          case Some(d) => columns.filterNot(k => d.fields.exists(_.name == k)).foreach(k => errors += s"relationship ${r.name}: unknown field $name.$k")
        errors ++= duplicates(s"relationship ${r.name} columns",columns)
      }
      doc.datasets.find(_.name == r.to).foreach { d =>
        val declared = (Vector(d.primaryKey).filter(_.nonEmpty) ++ d.uniqueKeys)
        if declared.nonEmpty && !declared.exists(k => k.toSet == r.toColumns.toSet) then errors += s"relationship ${r.name}: target is not a declared unique key"
      }
    }
    errors.result()
