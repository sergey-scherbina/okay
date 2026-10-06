package okay.semantic

/** A dimension lookup preserves the fact grain, including unmatched keys. */
final class Lookup[A, B, K] private (val relation: Relation, val model: Model[(A, Option[B])],
                                   val leftKey: A => Option[K], val rightKey: B => K) extends Serializable:
  def index(right: IterableOnce[B]): Either[Vector[String], Map[K, B]] =
    val indexed = scala.collection.mutable.Map.empty[K, B]
    val errors = Vector.newBuilder[String]
    right.iterator.foreach { b =>
      val key = rightKey(b)
      if indexed.contains(key) then errors += s"relation ${relation.id}: duplicate right key $key"
      else indexed.update(key, b)
    }
    val found = errors.result()
    if found.nonEmpty then Left(found) else Right(indexed.toMap)
  def enrich(row: A, index: Map[K, B]): (A, Option[B]) = row -> leftKey(row).flatMap(index.get)
  def rows(left: IterableOnce[A], right: IterableOnce[B]): Either[Vector[String], Vector[(A, Option[B])]] =
    val indexed = scala.collection.mutable.Map.empty[K, B]
    val errors = Vector.newBuilder[String]
    right.iterator.foreach { b =>
      val key = rightKey(b)
      if indexed.contains(key) then errors += s"relation ${relation.id}: duplicate right key $key"
      else indexed.update(key, b)
    }
    val seen = scala.collection.mutable.Set.empty[K]
    val out = left.iterator.map { a =>
      val key = leftKey(a)
      if relation.cardinality == Cardinality.OneToOne then key.foreach { k =>
        if !seen.add(k) then errors += s"relation ${relation.id}: duplicate left key $k"
      }
      a -> key.flatMap(indexed.get)
    }.toVector
    val found = errors.result()
    if found.nonEmpty then Left(found) else Right(out)
object Lookup:
  def build[A, B, K](left: Model[A], right: Model[B], relation: Relation,
                     leftKey: A => Option[K], rightKey: B => K,
                     dimensions: Vector[Dimension[B]]): Either[Vector[String], Lookup[A, B, K]] =
    val errors = Vector(
      Option.when((!Set(left.id).union(left.relations.map(_.to).toSet)(relation.from)) || relation.to != right.id)(s"relation ${relation.id}: model endpoints do not match"),
      Option.when(!Set(Cardinality.OneToOne, Cardinality.ManyToOne)(relation.cardinality))(s"relation ${relation.id}: fanout requires explicit allocation"),
      Option.when(relation.id.trim.isEmpty || relation.description.trim.isEmpty)("relation: blank id or description")).flatten ++
      dimensions.filterNot(d => right.dimensions.contains(d)).map(d => s"relation ${relation.id}: unknown right dimension ${d.id}")
    if errors.nonEmpty then Left(errors)
    else
      val dims = left.dimensions.map(d => Dimension[(A, Option[B])](d.id, d.description, d.kind, p => d.read(p._1), d.time)) ++
        dimensions.map(d => Dimension[(A, Option[B])](s"${relation.id}.${d.id}", d.description, d.kind,
          p => p._2.fold[Value](Value.Null)(d.read), d.time))
      val measures = left.measures.map(m => Measure[(A, Option[B])](m.id, m.description, p => m.read(p._1)))
      Model.build(left.id, Origin(s"${left.origin.source} + ${right.origin.source}", s"${left.origin.version} + ${right.origin.version}"),
        left.grain, dims, measures, left.metrics, left.relations :+ relation).map(m => new Lookup(relation, m, leftKey, rightKey))
