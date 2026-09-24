## okay2-cross-stream - okay2-stream on Scala.js and Scala Native; okay2-cross closed

okay2-stream is a crossProject, with no source change: the fast
channels' atomics are the javalibs' own. The suites that need real
threads or `java.nio.file` stay on the JVM by name, as a build setting,
so no file moved under the lane editing them. Every okay2 module except
cats/fs2/zio now runs on the JVM, Scala.js and Scala Native, and the
`okay2-cross` backlog item is closed (specs/okay2.md stage 35).

Docs: docs/okay2.md section 1.
