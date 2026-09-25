package okay.deploy.sbt

import _root_.sbt._
import _root_.sbt.Keys._

import scala.collection.mutable

/**
 * Forbidden module edges, refused at load (okay's specs/kernel.md).
 *
 * A microkernel is only as small as its dependency graph lets it be, and
 * the graph drifted twice in one day without anything saying no:
 * okay-ops reached two database drivers through okay-docs
 * (ops-docs-edge) and okay-http reached an LLM client through okay-mcp
 * (http-mcp-agent-edge). A rule here is two regexes over project ids and
 * the reason; at load, the COMPILE-scope closure of every project
 * matching `from` — its `dependsOn` edges whose configuration maps from
 * compile or runtime, followed transitively — must reach no project
 * matching `to`. A test-scope edge is not a violation.
 *
 * A build states its rules with `OkayModules.settings(rules: _*)`; the
 * rules of every build loaded together are all checked, so a product
 * that carries okay as a source dependency (okay-watch) keeps okay's
 * rules as well as its own.
 */
object OkayModules {

  final case class Rule(from: String, to: String, why: String) {
    private[sbt] val fromR = from.r
    private[sbt] val toR = to.r
  }

  def forbid(from: String, to: String, why: String): Rule = Rule(from, to, why)

  /** whether an edge's configuration puts the dependency on the compile
   * or runtime classpath: none said (the default `compile->compile`), or
   * any `compile`/`runtime` on the left of a mapping */
  def compileScoped(configuration: Option[String]): Boolean = configuration match {
    case None => true
    case Some(c) => c.split(";").exists { piece =>
      val left = piece.split("->").head.trim
      left == "compile" || left == "runtime"
    }
  }

  /** every violation, as `path — why`: `deps` is project id to its
   * dependencies with their configuration */
  def violations(deps: Map[String, Seq[(String, Option[String])]], rules: Seq[Rule]): Seq[String] = {
    val compile: Map[String, Seq[String]] =
      deps.map { case (p, ds) => p -> ds.collect { case (d, c) if compileScoped(c) => d } }
    val out = mutable.ArrayBuffer.empty[String]
    for {
      rule <- rules
      start <- deps.keys.toSeq.sorted if rule.fromR.pattern.matcher(start).matches
    } {
      // breadth first, so the path named is a shortest one
      val parent = mutable.LinkedHashMap[String, String](start -> "")
      val queue = mutable.Queue(start)
      var hit: Option[String] = None
      while (queue.nonEmpty && hit.isEmpty) {
        val at = queue.dequeue()
        compile.getOrElse(at, Nil).sorted.foreach { d =>
          if (!parent.contains(d)) {
            parent(d) = at
            if (hit.isEmpty && rule.toR.pattern.matcher(d).matches) hit = Some(d)
            queue.enqueue(d)
          }
        }
      }
      hit.foreach { h =>
        val path = Iterator.iterate(h)(parent).takeWhile(_.nonEmpty).toVector.reverse
        out += s"${path.mkString(" -> ")} — ${rule.why}"
      }
    }
    out.toVector
  }

  /** the check `settings` installs: a violation fails the load, naming
   * the path and the reason */
  def check(rules: Seq[Rule]): State => State = { state =>
    val bd = Project.extract(state).get(Global / buildDependencies)
    val deps: Map[String, Seq[(String, Option[String])]] =
      bd.classpath.map { case (ref, ds) => ref.project -> ds.map(d => (d.project.project, d.configuration)) }
    val found = violations(deps, rules)
    if (found.nonEmpty)
      sys.error("forbidden module edges (OkayModules):\n  " + found.mkString("\n  "))
    state
  }

  /** a build's rules, checked when the build loads */
  def settings(rules: Rule*): Seq[Setting[_]] = Seq(
    Global / onLoad := (Global / onLoad).value.andThen(check(rules)))
}
