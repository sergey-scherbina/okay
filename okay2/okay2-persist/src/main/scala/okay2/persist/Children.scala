package okay2.persist

import okay2.codec.{Codecs, Schema}

/**
 * CHILD WORKFLOWS (okay-persist's Children.scala): which dialogue a
 * child belongs to, and what it finished with. A parent waits on a
 * child's id (`Wf.child`); the worker that finishes the child records
 * its result here, and the parent's next advance reads it.
 */
final class Children(val snapshots: Snapshots) {

  def link(child: String, parent: String, program: String): Unit = {
    val _ = snapshots.putValue(key("l", child), Children.Link(parent, program))
  }

  def completed(child: String, result: String): Unit = {
    val _ = snapshots.putValue(key("d", child), Children.Done(result))
  }

  def resultOf(child: String): Option[String] =
    snapshots.latestValue[Children.Done](key("d", child)).flatMap(_._2.toOption).map(_.result)

  def parentOf(child: String): Option[Children.Link] =
    snapshots.latestValue[Children.Link](key("l", child)).flatMap(_._2.toOption)

  /** a parent's children, each with its result if it has one */
  def of(parent: String): List[(String, Children.Link, Option[String])] = {
    val links = scala.collection.mutable.LinkedHashMap.empty[String, Option[Children.Link]]
    Keyed.foreach(snapshots.topic) { r =>
      val k = new String(r.key, "UTF-8")
      if (k.startsWith("l|")) {
        val child = k.substring(2)
        links(child) = if (r.value.isEmpty) None else Codecs.readCbor[Children.Link](r.value).toOption
      }
    }
    links.collect { case (child, Some(l)) if l.parent == parent => (child, l, resultOf(child)) }.toList
  }

  private def key(kind: String, id: String): Array[Byte] = s"$kind|$id".getBytes("UTF-8")
}

object Children {
  final case class Link(parent: String, program: String)
  implicit lazy val linkSchema: Schema[Link] = Schema.derived
  final case class Done(result: String)
  implicit lazy val doneSchema: Schema[Done] = Schema.derived

  def over(store: Store, name: String = "__children"): Children = new Children(Snapshots(store, name))
}
