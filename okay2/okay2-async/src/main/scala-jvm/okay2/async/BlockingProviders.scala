package okay2.async

import java.util.ServiceLoader
import scala.jdk.CollectionConverters._

/** Discovery is explicit; existing lexical implicit defaults are unchanged. */
object BlockingProviders {
  def discover(): Vector[BlockingDefaults] =
    ServiceLoader.load(classOf[BlockingDefaults]).iterator().asScala.toVector
}
