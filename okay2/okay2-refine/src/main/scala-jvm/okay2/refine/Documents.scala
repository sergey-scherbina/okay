package okay2.refine

import java.nio.file.{Files, Path, Paths}
import scala.collection.concurrent.TrieMap
import scala.jdk.CollectionConverters._
import okay2.stream.Bulk

/**
 * Documents as `Bulk` input (okay's specs/refine.md, refine-bulk): a
 * DIRECTORY of whole files as (name, bytes), one element per file, each
 * read where it lands — the listing spread with `of`, each file read in
 * a `flatMap` (okay2's `Bulk` has no `read(path, Format)`; this is what
 * okay's `Bulk.read` does inside). On a cluster the executors must see
 * the same directory. The listing is taken once per JVM per directory.
 */
object Documents {
  private val listings = TrieMap.empty[String, Vector[Path]]

  /** the directory's regular files, recursively, in name order */
  def listing(dir: String): Vector[Path] =
    listings.getOrElseUpdate(dir, {
      val s = Files.walk(Paths.get(dir))
      try s.iterator.asScala.filter(Files.isRegularFile(_)).toVector.sortBy(_.toString)
      finally s.close()
    })

  def files[D[_]](dir: String)(implicit B: Bulk[D]): D[(String, Array[Byte])] =
    B.flatMap(B.of(listing(dir).indices.toVector)) { i =>
      val f = listing(dir)(i)
      Iterator.single((Paths.get(dir).relativize(f).toString, Files.readAllBytes(f)))
    }
}
