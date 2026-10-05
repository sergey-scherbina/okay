package okay.audit

import java.nio.file.Path

/** a reach from where none should be: a business module's reference under a rule */
final case class Finding(module: String, ref: Ref, rule: Rule)

/** what a handler or runtime module touches, by provider — the owner's top
  * package (`java.sql`, `com.zaxxer.hikari`): the DORA Art. 8 inventory line */
final case class Inventory(module: String, layer: Layer, byProvider: Map[String, Vector[Ref]])

final case class Report(
    inputs: Vector[(String, Path, String)],   // module, path, sha-256
    findings: Vector[Finding],                // business modules only; empty is the pass
    inventory: Vector[Inventory],             // handlers and runtime
    untracked: Vector[String],                // modules with no layer
    unusedAllows: Vector[Allow],
    allowed: Vector[(Allow, Finding)]):       // what the allows swallowed, so nothing is silent
  def passed: Boolean = findings.isEmpty

  def text: String =
    val sb = StringBuilder()
    sb ++= (if passed then "audit: PASS — no business module reaches past the boundary\n"
            else s"audit: FAIL — ${findings.size} reach(es) past the boundary\n")
    sb ++= s"carve-out (compiler bootstraps, never a finding): ${Boundary.Bootstraps.mkString(", ")}; bare class references to ${Boundary.BootstrapTypes.toVector.sorted.mkString(", ")}\n\n"
    if findings.nonEmpty then
      sb ++= "FINDINGS\n"
      findings.groupBy(_.module).toVector.sortBy(_._1).foreach { (m, fs) =>
        sb ++= s"  $m\n"
        fs.sortBy(f => (f.ref.from, f.ref.member)).foreach { f =>
          sb ++= s"    ${f.ref.from} -> ${f.ref.member}${desc(f.ref)}  [${f.rule.api}: ${f.rule.why}]\n"
        }
      }
      sb ++= "\n"
    if allowed.nonEmpty then
      sb ++= "ALLOWED (named exceptions)\n"
      allowed.sortBy((a, f) => (a.module, f.ref.member)).foreach { (a, f) =>
        sb ++= s"  ${a.module}: ${f.ref.from} -> ${f.ref.member}  allowed by ${a.owner}: ${a.reason}\n"
      }
      sb ++= "\n"
    if unusedAllows.nonEmpty then
      sb ++= "UNUSED ALLOWS\n"
      unusedAllows.foreach(a => sb ++= s"  ${a.module}: ${a.api} (${a.owner})\n")
      sb ++= "\n"
    sb ++= "INVENTORY (what the handlers and the runtime touch, by provider)\n"
    inventory.sortBy(i => (i.layer.ordinal, i.module)).foreach { inv =>
      sb ++= s"  ${inv.module} [${inv.layer.toString.toLowerCase}]\n"
      inv.byProvider.toVector.sortBy(_._1).foreach { (prov, refs) =>
        sb ++= s"    $prov: ${refs.map(_.member).distinct.sorted.mkString(", ")}\n"
      }
    }
    if untracked.nonEmpty then sb ++= s"\nUNTRACKED (no layer declared): ${untracked.sorted.mkString(", ")}\n"
    sb ++= "\nINPUTS\n"
    inputs.sortBy(i => (i._1, i._2.toString)).foreach((m, p, h) => sb ++= s"  $m  $p  sha256:$h\n")
    sb.result()

  def json: String =
    def s(x: String) = "\"" + x.flatMap {
      case '"' => "\\\""; case '\\' => "\\\\"; case '\n' => "\\n"; case c if c < ' ' => f"\\u${c.toInt}%04x"; case c => c.toString
    } + "\""
    def ref(r: Ref) = s"""{"from":${s(r.from)},"kind":${s(r.kind.toString)},"owner":${s(r.owner)},"name":${s(r.name)},"descriptor":${s(r.descriptor)}}"""
    val fs = findings.sortBy(f => (f.module, f.ref.from, f.ref.member)).map(f =>
      s"""{"module":${s(f.module)},"ref":${ref(f.ref)},"rule":${s(f.rule.api)},"why":${s(f.rule.why)}}""")
    val inv = inventory.sortBy(i => (i.layer.ordinal, i.module)).map { i =>
      val provs = i.byProvider.toVector.sortBy(_._1).map((p, rs) =>
        s"""${s(p)}:[${rs.map(_.member).distinct.sorted.map(s).mkString(",")}]""")
      s"""{"module":${s(i.module)},"layer":${s(i.layer.toString.toLowerCase)},"providers":{${provs.mkString(",")}}}"""
    }
    val ins = inputs.sortBy(i => (i._1, i._2.toString)).map((m, p, h) => s"""{"module":${s(m)},"path":${s(p.toString)},"sha256":${s(h)}}""")
    val al = allowed.sortBy((a, f) => (a.module, f.ref.member)).map((a, f) =>
      s"""{"module":${s(a.module)},"api":${s(a.api)},"owner":${s(a.owner)},"reason":${s(a.reason)},"ref":${ref(f.ref)}}""")
    val un = unusedAllows.map(a => s"""{"module":${s(a.module)},"api":${s(a.api)},"owner":${s(a.owner)}}""")
    s"""{"passed":$passed,"bootstraps":[${Boundary.Bootstraps.map(s).mkString(",")}],"findings":[${fs.mkString(",")}],""" +
      s""""allowed":[${al.mkString(",")}],"unusedAllows":[${un.mkString(",")}],"inventory":[${inv.mkString(",")}],""" +
      s""""untracked":[${untracked.sorted.map(s).mkString(",")}],"inputs":[${ins.mkString(",")}]}"""

  private def desc(r: Ref) = if r.kind == Ref.Kind.Native then " (native method)" else ""

object Audit:
  /** a refusal before any scan: an allow without its reason or owner */
  final class Refused(msg: String) extends IllegalArgumentException(msg)

  /** the owner's top package: `java.sql.Connection` -> `java.sql`; a bare name -> itself */
  def provider(owner: String): String =
    val parts = owner.split('.')
    if parts.length <= 2 then parts.dropRight(1).mkString(".") match { case "" => owner; case p => p }
    else
      // two segments, or three for the `com.x.y` / `org.x.y` / `io.x.y` shape
      val n = if Set("com", "org", "io", "net", "dev").contains(parts(0)) && parts.length > 3 then 3 else 2
      parts.take(n).mkString(".")

  /** `modules`: name -> the class directories and jars that ARE that module
    * (its own classes and its classpath; a jar inherits the module's layer) */
  def run(boundary: Boundary, modules: Map[String, Seq[Path]]): Report =
    boundary.allows.find(a => a.reason.trim.isEmpty || a.owner.trim.isEmpty).foreach { a =>
      throw Refused(s"allow ${a.module}: ${a.api} has no ${if a.reason.trim.isEmpty then "reason" else "owner"} — an exception is a named decision")
    }
    val inputs = Vector.newBuilder[(String, Path, String)]
    val findings = Vector.newBuilder[Finding]
    val inventory = Vector.newBuilder[Inventory]
    val untracked = Vector.newBuilder[String]
    val allowed = Vector.newBuilder[(Allow, Finding)]
    val used = scala.collection.mutable.Set.empty[Allow]
    modules.toVector.sortBy(_._1).foreach { (module, paths) =>
      val layer = boundary.layers.getOrElse(module, Layer.Untracked)
      layer match
        case Layer.Untracked => untracked += module
        case _ =>
          val refs = paths.toVector.flatMap { p =>
            inputs += ((module, p, Scan.digest(p)))
            Scan.path(p)
          }.filterNot(Boundary.isBootstrap)
          layer match
            case Layer.Business =>
              refs.foreach { r =>
                (if r.kind == Ref.Kind.Native then Some(Boundary.Native) else boundary.rules.find(_.matches(r))).foreach { rule =>
                  val f = Finding(module, r, rule)
                  boundary.allows.find(_.matches(module, r)) match
                    case Some(a) => used += a; allowed += ((a, f))
                    case None => findings += f
                }
              }
            case _ =>
              // the inventory lists what the module touches OUTSIDE itself and the JDK's pure core:
              // every reference under a rule is a reach; the rest is plain computation
              val reaches = refs.filter(r => r.kind == Ref.Kind.Native || boundary.rules.exists(_.matches(r)))
              inventory += Inventory(module, layer, reaches.groupBy(r => provider(r.owner)))
    }
    Report(inputs.result(), findings.result(), inventory.result(), untracked.result(),
      boundary.allows.filterNot(used.contains), allowed.result())
