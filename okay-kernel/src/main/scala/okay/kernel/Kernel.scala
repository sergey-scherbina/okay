package okay.kernel

import okay.{!, Resource, pure}
import scala.collection.mutable

/** everything that can stop a plugin set from running, as a value with
 * the sentence an operator reads (specs/kernel.md) */
enum Problem:
  case DuplicateId(id: String, versions: Vector[Version])
  case KernelMismatch(plugin: String, range: Range, api: Version)
  /** two `Port` values with one name and different versions */
  case PortConflict(port: String, versions: Vector[Version])
  /** a provision built against another major, or a newer contract than the host's */
  case Incompatible(plugin: String, port: String, built: Version, host: Version)
  case Missing(plugin: String, port: String, range: Range)
  /** providers exist; none was built against a contract the need accepts */
  case Unserved(plugin: String, port: String, range: Range, candidates: Vector[(String, Version)])
  case Ambiguous(port: String, providers: Vector[String])
  case UnknownChoice(port: String, plugin: String)
  case Cycle(path: Vector[String])
  case LoadFailed(what: String, why: String)
  case LawBroken(plugin: String, port: String, law: String, why: String)

  def says: String = this match
    case DuplicateId(id, vs) => s"two plugins are called '$id' (${vs.mkString(", ")})"
    case KernelMismatch(p, r, api) => s"$p was built for kernel $r; this kernel is $api"
    case PortConflict(port, vs) => s"port '$port' is defined twice, as ${vs.mkString(" and ")}"
    case Incompatible(p, port, b, h) =>
      if b.major != h.major then s"$p implements $port $b; this host's contract is $h — another major"
      else s"$p implements $port $b, newer than this host's contract $h"
    case Missing(p, port, r) => s"$p needs $port $r and nothing provides it"
    case Unserved(p, port, r, cs) =>
      s"$p needs $port $r; provided only as " + cs.map((id, v) => s"$v by $id").mkString(", ")
    case Ambiguous(port, ps) => s"$port takes one provider and has ${ps.size}: ${ps.mkString(", ")} — choose one"
    case UnknownChoice(port, p) => s"the choice for $port is $p, which does not provide it"
    case Cycle(path) => s"a cycle: ${path.mkString(" -> ")}"
    case LoadFailed(what, why) => s"could not load $what: $why"
    case LawBroken(p, port, law, why) => s"$p's $port breaks '$law': $why"

/** a plan that could not run, or a start a law stopped */
final class Refused(val problems: Vector[Problem])
  extends RuntimeException(problems.map(_.says).mkString("; "))

/** one provided implementation, as the kernel installed it */
final case class Installed(plugin: String, version: Version, port: String, built: Version)

/**
 * The answer to "can these plugins run together, and in what order",
 * computed without running any of them. `wired` is what each plugin's
 * needs resolved to; `serving` is what the host reads.
 */
final case class Plan(order: Vector[Plugin],
                      wired: Map[(String, String), Vector[(Plugin, Provision[?])]],
                      serving: Map[String, Vector[(Plugin, Provision[?])]]):
  def describe: Vector[String] =
    order.map(p => s"$p: provides ${p.provides.map(v => s"${v.port.name} ${v.built}").mkString(", ")}" +
      (if p.needs.isEmpty then "" else s"; needs ${p.needs.map(n => s"${n.port.name} ${n.range}").mkString(", ")}"))

/** the assembled program, as the host sees it: any port, every provider */
trait Running:
  def one[A](p: Port[A]): A
  def all[A](p: Port[A]): Vector[A]
  def maybe[A](p: Port[A]): Option[A]
  /** each provider's value with the id of the plugin that made it — for
   * a host that must say WHO provides what (two sources claiming one
   * name, a status page) */
  def providers[A](p: Port[A]): Vector[(String, A)]
  def installed: Vector[Installed]

object Kernel:
  /** the version of THIS API — what `Plugin.kernel` is checked against */
  val api: Version = Version(1, 0, 0)

  /**
   * Every problem at once, or the start order (specs/kernel.md).
   * `choose` names the provider of a `One` port that has several;
   * `disabled` removes plugins before anything is checked.
   */
  def plan(plugins: Seq[Plugin], choose: Map[String, String] = Map.empty,
           disabled: Set[String] = Set.empty): Either[Vector[Problem], Plan] =
    val active = plugins.filterNot(p => disabled(p.id)).sortBy(_.id).toVector
    val problems = Vector.newBuilder[Problem]

    active.groupBy(_.id).toVector.sortBy(_._1).foreach { (id, ps) =>
      if ps.size > 1 then problems += Problem.DuplicateId(id, ps.map(_.version))
    }
    active.foreach(p => if !p.kernel.accepts(api) then problems += Problem.KernelMismatch(p.id, p.kernel, api))

    // one name, one contract version
    val ports: Vector[Port[?]] = active.flatMap(p => p.needs.map(_.port) ++ p.provides.map(_.port))
    val host: Map[String, Version] = ports.groupBy(_.name).map { (name, ps) =>
      val vs = ps.map(_.version).distinct.sorted
      if vs.size > 1 then problems += Problem.PortConflict(name, vs)
      name -> vs.last
    }
    val arity: Map[String, Arity] = ports.map(p => p.name -> p.arity).toMap

    // what each port could be served by: the compatible provisions
    val offered: Map[String, Vector[(Plugin, Provision[?])]] =
      active.flatMap(p => p.provides.map(v => (p, v))).filter { (p, v) =>
        val h = host(v.port.name)
        val ok = v.built.major == h.major && v.built <= h
        if !ok then problems += Problem.Incompatible(p.id, v.port.name, v.built, h)
        ok
      }.groupBy(_._2.port.name)

    choose.toVector.sortBy(_._1).foreach { (port, id) =>
      if !offered.getOrElse(port, Vector.empty).exists(_._1.id == id) then
        problems += Problem.UnknownChoice(port, id)
    }
    val serving: Map[String, Vector[(Plugin, Provision[?])]] = offered.map { (port, vs) =>
      if arity(port) == Arity.One && vs.size > 1 then
        // a choice that names none of them is UnknownChoice above, and
        // the port is still ambiguous — not missing
        choose.get(port).filter(id => vs.exists(_._1.id == id)) match
          case Some(id) => port -> vs.filter(_._1.id == id)
          case None =>
            problems += Problem.Ambiguous(port, vs.map(_._1.id))
            port -> vs.take(1)
      else port -> vs
    }

    // each need, resolved: never to the plugin itself
    val wired = mutable.Map.empty[(String, String), Vector[(Plugin, Provision[?])]]
    val edges = mutable.Map.empty[String, Set[String]].withDefaultValue(Set.empty)
    for p <- active; n <- p.needs do
      val name = n.port.name
      val cs = serving.getOrElse(name, Vector.empty).filter(_._1.id != p.id)
      val fit = cs.filter((_, v) => n.range.accepts(v.built))
      if cs.isEmpty then { if !n.optional then problems += Problem.Missing(p.id, name, n.range) }
      else if fit.isEmpty then
        problems += Problem.Unserved(p.id, name, n.range, cs.map((q, v) => (q.id, v.built)))
      wired((p.id, name)) = fit
      edges(p.id) = edges(p.id) ++ fit.map(_._1.id)

    val order = topo(active, edges.toMap.withDefaultValue(Set.empty), problems)
    val found = problems.result()
    if found.nonEmpty then Left(found)
    else Right(Plan(order, wired.toMap, serving))

  /** providers before their users; where free, by id — the same set
   * gives the same order every run */
  private def topo(ps: Vector[Plugin], needsFrom: Map[String, Set[String]],
                   problems: mutable.Builder[Problem, Vector[Problem]]): Vector[Plugin] =
    val byId = ps.map(p => p.id -> p).toMap
    val waiting = mutable.Map.from(ps.map(p => p.id -> needsFrom(p.id).filter(byId.contains)))
    val out = Vector.newBuilder[Plugin]
    val ready = mutable.SortedSet.from(waiting.collect { case (id, ds) if ds.isEmpty => id })
    while ready.nonEmpty do
      val id = ready.head
      ready -= id
      waiting -= id
      out += byId(id)
      waiting.foreach { (other, ds) =>
        if ds(id) then
          val left = ds - id
          waiting(other) = left
          if left.isEmpty then ready += other
      }
    if waiting.nonEmpty then problems += Problem.Cycle(cycle(waiting.toMap))
    out.result()

  private def cycle(waiting: Map[String, Set[String]]): Vector[String] =
    // walk from the smallest id along the first remaining dependency
    // until a node repeats; the path from its first visit is the cycle
    var at = waiting.keys.min
    val path = mutable.ArrayBuffer.empty[String]
    while !path.contains(at) do
      path += at
      at = waiting(at).filter(waiting.contains).min
    (path.drop(path.indexOf(at)) :+ at).toVector

  /**
   * Make every provision in plan order under one `Resource`: released in
   * reverse, once, also when a later one throws. With `verify`, every
   * provided value is checked against its port's laws first, and a
   * broken law stops the start with `Refused(LawBroken)`.
   */
  def start(plan: Plan, verify: Boolean = true): Running ! Resource =
    val made = mutable.Map.empty[(String, String), scala.Any]

    def valuesOf(links: Vector[(Plugin, Provision[?])]): Vector[scala.Any] =
      links.map((p, v) => made((p.id, v.port.name)))

    def wiringFor(p: Plugin): Wiring = new Wiring:
      private def links(port: Port[?]): Vector[(Plugin, Provision[?])] =
        if !p.needs.exists(_.port.name == port.name) then
          throw IllegalStateException(s"${p.id} reads ${port.name}, which it does not declare in needs")
        plan.wired.getOrElse((p.id, port.name), Vector.empty)
      def one[A](port: Port[A]): A = valuesOf(links(port)).headOption.getOrElse(
        throw IllegalStateException(s"${p.id} asks for ${port.name} and it is optional and absent: use maybe")
      ).asInstanceOf[A]
      def all[A](port: Port[A]): Vector[A] = valuesOf(links(port)).asInstanceOf[Vector[A]]
      def maybe[A](port: Port[A]): Option[A] = valuesOf(links(port)).headOption.asInstanceOf[Option[A]]

    def makeAll(p: Plugin, vs: List[Provision[?]]): Unit ! Resource = vs match
      case Nil => pure(())
      case v :: rest =>
        makeOne(p, v).flatMap(_ => makeAll(p, rest))

    def makeOne[A](p: Plugin, v: Provision[A]): Unit ! Resource =
      v.make(wiringFor(p)).flatMap { a =>
        if verify then
          val broken = v.port.laws.flatMap(l =>
            (try l.check(a) catch case t: Throwable => Left(s"threw $t")).left.toOption
              .map(why => Problem.LawBroken(p.id, v.port.name, l.name, why)))
          if broken.nonEmpty then throw Refused(broken)
        made((p.id, v.port.name)) = a
        pure(())
      }

    def go(ps: List[Plugin]): Unit ! Resource = ps match
      case Nil => pure(())
      case p :: rest => makeAll(p, p.provides.toList).flatMap(_ => go(rest))

    go(plan.order.toList).map { _ =>
      new Running:
        def one[A](port: Port[A]): A = all(port).headOption.getOrElse(
          throw IllegalStateException(s"nothing provides ${port.name}"))
        def all[A](port: Port[A]): Vector[A] =
          valuesOf(plan.serving.getOrElse(port.name, Vector.empty)).asInstanceOf[Vector[A]]
        def maybe[A](port: Port[A]): Option[A] = all(port).headOption
        def providers[A](port: Port[A]): Vector[(String, A)] =
          plan.serving.getOrElse(port.name, Vector.empty).map((p, v) =>
            (p.id, made((p.id, v.port.name)).asInstanceOf[A]))
        val installed: Vector[Installed] = plan.order.flatMap(p =>
          p.provides.map(v => Installed(p.id, p.version, v.port.name, v.built)))
    }

  /** plan and start in one: a plan with problems is `Refused` */
  def assemble(plugins: Seq[Plugin], choose: Map[String, String] = Map.empty,
               disabled: Set[String] = Set.empty, verify: Boolean = true): Running ! Resource =
    plan(plugins, choose, disabled) match
      case Left(ps) => throw Refused(ps)
      case Right(p) => start(p, verify)
