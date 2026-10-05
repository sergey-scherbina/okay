package okay.audit

import java.nio.file.{Files, Path, Paths}

/** `okay.audit.Main <modules.tsv> <out-dir> [allows.tsv]`
  *
  * modules.tsv: one module a line, `name<TAB>layer<TAB>path1:path2:...[<TAB>pkg=layer;pkg=layer]`
  *   (layer: business | handlers | runtime | untracked; the fourth column
  *   classifies packages inside the module, specs/audit-ready.md stage 1)
  * allows.tsv:  `module<TAB>api<TAB>owner<TAB>reason`
  * writes `<out-dir>/report.txt` and `report.json`; exit 1 on a finding,
  * 2 on a refusal. This object is the one place the module itself does
  * I/O — okay-audit is a Handlers module of its own audit. */
object Main:
  def layer(s: String): Layer = s.trim.toLowerCase match
    case "business" => Layer.Business
    case "handlers" => Layer.Handlers
    case "runtime" => Layer.Runtime
    case _ => Layer.Untracked

  def readModules(file: Path): (Map[String, Layer], Map[String, Seq[Path]]) =
    val (l, m, _) = readManifest(file); (l, m)

  /** layers, paths, and the package prefixes' layers */
  def readManifest(file: Path): (Map[String, Layer], Map[String, Seq[Path]], Map[String, Map[String, Layer]]) =
    val lines = Files.readAllLines(file).toArray.map(_.toString).filter(_.trim.nonEmpty)
    val rows = lines.map(_.split('\t')).map(a => (a(0), layer(a(1)),
      if a.length > 2 then a(2).split(java.io.File.pathSeparatorChar).filter(_.nonEmpty).toSeq.map(Paths.get(_)) else Seq.empty,
      if a.length > 3 then a(3).split(';').filter(_.contains('=')).map { kv => val Array(k, v) = kv.split('=') ; k.trim -> layer(v) }.toMap else Map.empty[String, Layer]))
    (rows.map(r => r._1 -> r._2).toMap, rows.map(r => r._1 -> r._3).toMap, rows.map(r => r._1 -> r._4).toMap)

  def readAllows(file: Path): Vector[Allow] =
    Files.readAllLines(file).toArray.map(_.toString).filter(_.trim.nonEmpty).toVector.map { l =>
      val a = l.split('\t').padTo(4, "")
      Allow(a(0), a(1), a(3), a(2))
    }

  def main(args: Array[String]): Unit =
    if args.length < 2 then
      System.err.println("usage: okay.audit.Main <modules.tsv> <out-dir> [allows.tsv]"); System.exit(2)
    val (layers, modules, packages) = readManifest(Paths.get(args(0)))
    val allows = if args.length > 2 then readAllows(Paths.get(args(2))) else Vector.empty
    val out = Paths.get(args(1))
    Files.createDirectories(out)
    try
      val report = Audit.run(Boundary(layers, allows = allows, packages = packages), modules)
      Files.writeString(out.resolve("report.txt"), report.text)
      Files.writeString(out.resolve("report.json"), report.json)
      System.out.print(report.text)
      System.out.println(s"audit: report written to ${out.resolve("report.txt")}")
      if !report.passed then System.exit(1)
    catch
      case r: Audit.Refused => System.err.println(s"audit: refused — ${r.getMessage}"); System.exit(2)
