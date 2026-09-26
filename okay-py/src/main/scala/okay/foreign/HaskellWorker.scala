package okay.foreign

import java.nio.file.{Files, Path}

/**
 * A Haskell worker for okay's programs-as-data (remote-foreign,
 * specs/remote-foreign.md). A `Main.hs` imports the `Okay` module this jar
 * ships (`/okay/hs/Okay.hs`), writes its programs in `Prog`, and `serve`s
 * them by name; `build` compiles it with GHC, and
 * `ForeignWorker.speaking(Seq(binary.toString))` runs it — the same wire as
 * Python, so `Py.program` drives it unchanged:
 *
 * {{{
 * val bin = HaskellWorker.build(dirWithMainHs)
 * val w = ForeignWorker.speaking(Seq(bin.toString))
 * runChoice(Py.program[Long]("pairs").calling(Py.callbacks(choose))().program).runWith(using w.handler)
 * }}}
 *
 * A Haskell continuation is a pure function, so the multi-shot claim is
 * exact here: a `Choice` handler continues the same one twice, and each
 * branch is the program's own.
 */
object HaskellWorker:

  /** the `Okay` module's source, as this jar ships it */
  def library: String = resource("Okay.hs")

  /** the `OkayEff` module: programs typed by their effects (hs-typed-effects) */
  def effects: String = resource("OkayEff.hs")

  private def resource(name: String): String =
    val res = getClass.getResourceAsStream(s"/okay/hs/$name")
    if res == null then throw IllegalStateException(s"okay.foreign: /okay/hs/$name is missing from the jar")
    try String(res.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8) finally res.close()

  /**
   * Compile `dir/main` (default `Main.hs`) against the shipped `Okay`
   * module; answers the binary. `Okay.hs` and `OkayEff.hs` are written into `dir` beside it,
   * and GHC's objects go under `dir/.okay-build`. A compile error refuses
   * with GHC's own words.
   */
  def build(dir: Path, main: String = "Main.hs", ghc: String = "ghc"): Path =
    Files.writeString(dir.resolve("Okay.hs"), library): Unit
    Files.writeString(dir.resolve("OkayEff.hs"), effects): Unit
    val out = dir.resolve(".okay-build")
    Files.createDirectories(out): Unit
    val bin = out.resolve("worker")
    val pb = ProcessBuilder(ghc, "-O1", "-outputdir", out.toString, "-o", bin.toString, main)
      .directory(dir.toFile).redirectErrorStream(true)
    val p =
      try pb.start()
      catch case e: java.io.IOException =>
        throw IllegalStateException(s"okay.foreign: '$ghc' did not start (${e.getMessage}) — is GHC installed?")
    val log = String(p.getInputStream.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8)
    if p.waitFor() != 0 then
      throw IllegalStateException(s"okay.foreign: the Haskell worker did not compile:\n${log.linesIterator.toVector.takeRight(20).mkString("\n")}")
    bin
