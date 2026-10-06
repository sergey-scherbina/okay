package okay.semantic

private[semantic] object Routes:
  def find(catalog: Catalog, from: String, to: String, via: Vector[String]): Either[Vector[String], Vector[Relation]] =
    val entities = catalog.entities.map(_.id).toSet
    if !entities(from) || !entities(to) then Left(Vector(s"route $from -> $to: unknown entity"))
    else if via.nonEmpty then
      val byName = catalog.relations.map(r => r.id -> r).toMap
      val errors = Vector.newBuilder[String]
      val out = Vector.newBuilder[Relation]
      var at = from
      via.foreach { id => byName.get(id) match
        case None => errors += s"route: unknown relation $id"
        case Some(r) =>
          if r.from != at then errors += s"relation $id: expected endpoint $at"
          if !safe(r) then errors += s"relation $id: fanout requires allocation"
          out += r
          at = r.to
      }
      if at != to then errors += s"route: ended at $at, expected $to"
      val found = errors.result()
      if found.nonEmpty then Left(found) else Right(out.result())
    else if from == to then Right(Vector.empty)
    else
      val edges = catalog.relations.filter(safe)
      val forward = edges.groupMap(_.from)(_.to)
      val backward = edges.groupMap(_.to)(_.from)
      val relevant = reachable(from, forward).intersect(reachable(to, backward))
      if !relevant(to) then Left(Vector(s"route $from -> $to: no fanout-safe path"))
      else
        val active = edges.filter(r => relevant(r.from) && relevant(r.to))
        val outgoing = active.groupBy(_.from)
        val degree = scala.collection.mutable.Map.from(relevant.toVector.map(n => n -> 0))
        active.foreach(r => degree(r.to) += 1)
        val queue = scala.collection.mutable.Queue.from(relevant.toVector.sorted.filter(n => degree(n) == 0))
        val count = scala.collection.mutable.Map(from -> 1)
        val parent = scala.collection.mutable.Map.empty[String, Relation]
        var visited = 0
        while queue.nonEmpty do
          val node = queue.dequeue()
          visited += 1
          outgoing.getOrElse(node, Vector.empty).foreach { r =>
            val paths = count.getOrElse(node, 0)
            if paths > 0 then
              count.update(r.to, math.min(2, count.getOrElse(r.to, 0) + paths))
              parent.update(r.to, r)
            degree(r.to) -= 1
            if degree(r.to) == 0 then queue.enqueue(r.to)
          }
        if visited != relevant.size then Left(Vector(s"route $from -> $to: cycle requires explicit relations"))
        else if count.getOrElse(to, 0) != 1 then Left(Vector(s"route $from -> $to: ambiguous paths; provide relation IDs"))
        else
          val path = scala.collection.mutable.ArrayBuffer.empty[Relation]
          var at = to
          while at != from do
            val relation = parent(at)
            path += relation
            at = relation.from
          Right(path.reverse.toVector)
  private def safe(r: Relation): Boolean = Set(Cardinality.OneToOne, Cardinality.ManyToOne)(r.cardinality)
  private def reachable(start: String, edges: Map[String, Vector[String]]): Set[String] =
    val seen = scala.collection.mutable.Set(start)
    val queue = scala.collection.mutable.Queue(start)
    while queue.nonEmpty do
      edges.getOrElse(queue.dequeue(), Vector.empty).foreach(n => if seen.add(n) then queue.enqueue(n))
    seen.toSet
