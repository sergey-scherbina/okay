package okay.deploy

import java.nio.file.Path

/**
 * A measurement writes `src/jmh/history.d/<when>-<measure>.tsv` rather
 * than the tail of everyone's `src/jmh/history.tsv` (history-d,
 * 2026-09-25) — the third file of this repository to become a
 * directory for the same reason, after changelog.d and the boards.
 *
 * The guard is `scripts/history.sh --check` itself, run here rather than
 * re-stated: the naming, the eight TAB-separated columns every reader
 * splits on, and the archive's frozen length (a row appended out of
 * habit, or one dropped by an edit, turns the gate red with the command
 * to use instead). One rule, one place, and the gate reads its words.
 */
class TestHistoryEntries extends munit.FunSuite:

  private val root: Path = Deploy.repoRoot()

  test("history.d entries are well formed, and the archive is not appended to"):
    val pb = new ProcessBuilder("sh", root.resolve("scripts/history.sh").toString, "--check")
      .directory(root.toFile).redirectErrorStream(true)
    val p = pb.start()
    val out = new String(p.getInputStream.readAllBytes())
    val code = p.waitFor()
    assertEquals(code, 0, s"scripts/history.sh --check refused:\n$out")
    assert(out.contains("archive intact"), out)
