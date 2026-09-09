package okay.deploy

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/**
 * specs/data.md's last box, the grep-able half: no adapter's main
 * source prints, logs or journals a credential. Read from the
 * committed tree like TestDocsIndex: every line of an adapter module
 * that reaches a print/log sink is checked for a credential-named
 * token. As of adapter-stats (2026-09-09) the adapters have NO
 * print or log lines at all — the invariant is that this stays so.
 */
class TestNoCredentialLogs extends munit.FunSuite:
  private val root: Path = Deploy.repoRoot()

  private val adapters = Vector("okay-jdbc", "okay-pg", "okay-r2dbc", "okay-docs", "okay-docs-mongo",
    "okay-docs-dynamo", "okay-docs-cassandra", "okay-blob", "okay-kafka", "okay-cache", "okay-delta", "okay-sql")

  private val sink = "(println|print\\(|System\\.(out|err)|log(ger)?\\.(info|warn|debug|error|trace)|journal)".r
  private val credential = "(?i)(password|passwd|secret|accesskey|access_key|credential|creds|token|apikey|api_key)".r

  test("no adapter main source prints, logs or journals a credential-named value") {
    val offenders = adapters.flatMap { m =>
      val main = root.resolve(m).resolve("src/main")
      if !Files.isDirectory(main) then Vector.empty
      else Files.walk(main).iterator.asScala.filter(p => p.toString.endsWith(".scala")).toVector.flatMap { p =>
        Files.readAllLines(p).asScala.zipWithIndex.collect {
          case (line, i) if sink.findFirstIn(line).isDefined && credential.findFirstIn(line).isDefined =>
            s"${root.relativize(p)}:${i + 1}: ${line.trim}"
        }
      }
    }
    assertEquals(offenders, Vector.empty[String], "credential-named values reaching a print/log/journal sink")
  }
