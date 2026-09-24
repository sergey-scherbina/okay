package okay2.stream

import java.nio.file.Files
import scala.jdk.CollectionConverters._

/** the fixtures the table suites share, at the top level */
object TablesFixtures {
  final case class Sale(shop: Int, amount: Long)

  /** the local platform over real files */
  val localBulk: Bulk[Chunks] = Bulk.local(p => Files.lines(java.nio.file.Path.of(p)).iterator().asScala, p => Some(Files.size(java.nio.file.Path.of(p))))
}
