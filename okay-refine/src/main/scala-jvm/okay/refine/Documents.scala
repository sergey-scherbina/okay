package okay.refine

import java.nio.file.{Files, Path, Paths}
import scala.collection.concurrent.TrieMap
import scala.jdk.CollectionConverters.*
import okay.Bulk

/**
 * Documents as `Bulk` input (specs/refine.md, refine-bulk): a DIRECTORY
 * of whole files, one split per file, each read as (name, bytes) where
 * it lands — so `Bulk.read(dir, Documents.files)` is a folder of
 * documents on any `Bulk`: in one JVM, or spread over Spark's executors
 * (which must see the same directory: a shared file system on a cluster).
 * The listing is taken once per JVM per directory, not once per file.
 */
object Documents:
  private val listings = TrieMap.empty[String, Vector[Path]]

  /** the directory's regular files, recursively, in name order */
  def listing(dir: String): Vector[Path] =
    listings.getOrElseUpdate(dir, {
      val s = Files.walk(Paths.get(dir))
      try s.iterator.asScala.filter(Files.isRegularFile(_)).toVector.sortBy(_.toString)
      finally s.close()
    })

  val files: Bulk.Format[(String, Array[Byte])] = new Bulk.Format[(String, Array[Byte])]:
    def name: String = "files"
    def splits(path: String): Vector[Int] = listing(path).indices.toVector
    def read(path: String, split: Int): Iterator[(String, Array[Byte])] =
      val f = listing(path)(split)
      Iterator.single((Paths.get(path).relativize(f).toString, Files.readAllBytes(f)))
