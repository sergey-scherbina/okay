package okay.audit

import java.nio.file.{Files, Path, Paths}

/** `okay.audit.Main --manifest audit.json --report out-dir`
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

  /** Runs either the standalone JSON contract or the legacy TSV input. */
  def run(args: Array[String]): Int =
    try
      args.toList match
        case "--manifest" :: manifest :: "--report" :: out :: Nil =>
          val input = Manifest.read(Paths.get(manifest))
          write(input, Paths.get(out))
        case manifest :: out :: allows =>
          if allows.size > 1 then throw Audit.Refused("usage: okay.audit.Main <modules.tsv> <out-dir> [allows.tsv]")
          val (layers, modules, packages) = readManifest(Paths.get(manifest))
          write(Manifest(layers, modules, packages, allows.headOption.map(Paths.get(_)).map(readAllows).getOrElse(Vector.empty)), Paths.get(out))
        case _ => throw Audit.Refused("usage: okay.audit.Main --manifest audit.json --report out-dir")
    catch
      case r: Audit.Refused =>
        System.err.println(s"audit: refused — ${r.getMessage}")
        2

  def main(args: Array[String]): Unit = System.exit(run(args))

  private def write(input: Manifest, out: Path): Int =
    Files.createDirectories(out)
    val report = Audit.run(Boundary(input.layers, allows = input.allows, packages = input.packages), input.modules)
    Files.writeString(out.resolve("report.txt"), report.text)
    Files.writeString(out.resolve("report.json"), report.json)
    System.out.print(report.text)
    System.out.println(s"audit: report written to ${out.resolve("report.txt")}")
    if report.passed then 0 else 1
