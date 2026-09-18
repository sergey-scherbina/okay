package okay

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/**
 * The local `Bulk` on a JVM: `java.nio` turns a path into lines. That
 * is the platform's whole contribution — the instance itself is
 * `Bulk.local`, shared with every platform that can read a file.
 */
given localBulk: Bulk[Chunks] = Bulk.local(
  path => Files.lines(Path.of(path), UTF_8).iterator().asScala,
  path => { val f = Path.of(path); if Files.isRegularFile(f) then Some(Files.size(f)) else None })
