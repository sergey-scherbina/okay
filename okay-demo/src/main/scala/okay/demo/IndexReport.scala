package okay.demo

import okay.rag.*

/**
 * What the retriever sees, without a model in play. Useful on its
 * own — pointing this at a repository answers "is my code being
 * indexed, and as what?" — and useful as a demo, because every number
 * it prints comes from parsing, not from an embedding service.
 *
 *   sbt "okayDemo/runMain okay.demo.IndexReport /path/to/repo"
 */
object IndexReport {

  def main(args: Array[String]): Unit =
    val root = java.io.File(
      args.headOption.getOrElse(sys.env.getOrElse("OKAY_REPO", ".")))

    val t0 = System.nanoTime()
    val sources = RepoAgent.load(root, limit = 5000)
    val repo = RepoAgent.index(sources)
    val ms = (System.nanoTime() - t0) / 1000000.0
    val bytes = sources.map(_.text.length.toLong).sum

    println(f"${sources.size} files, ${bytes / 1024}%,d KB, " +
      f"${repo.index.names.size}%,d names, ${repo.index.defs.values.map(_.size).sum}%,d " +
      f"definitions in $ms%.0f ms (${bytes / 1024.0 / (ms / 1000)}%,.0f KB/s)")

    println("\nby language:")
    sources.groupBy(s => Language.of(s.id).map(_.name).getOrElse("text"))
      .toSeq.sortBy(-_._2.size).foreach { (lang, fs) =>
        val defs = fs.map(f => Symbols.source(f).defs.values.map(_.size).sum).sum
        println(f"  $lang%-12s ${fs.size}%4d files  $defs%6d definitions")
      }

    println("\nthe kinds of definition found:")
    repo.index.defs.values.flatten.groupBy(_.kind).toSeq.sortBy(-_._2.size)
      .take(12).foreach((k, v) => println(f"  $k%-12s ${v.size}%6d"))

    println("\nthe most-mentioned names (structural retrieval, no vectors):")
    repo.index.refs.toSeq.sortBy(-_._2.size).take(10)
      .foreach((n, rs) => println(f"  $n%-24s ${rs.size}%5d mentions" +
        (if repo.index.definition(n).nonEmpty then
          s"  defined in ${repo.index.definition(n).head.source}"
        else "  (no definition in this repo)")))
}
