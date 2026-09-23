package okay.r

import java.nio.file.{Files, Path, Paths}

/**
 * The R library DECLARED in code (foreign-managed-env) — okay-py's
 * `PyEnv` in R. `provision` fills a library directory keyed by a hash of
 * the declaration with the CRAN packages named, using a fixed script from
 * this jar (the names reach it as data); `start` hands the library to the
 * session as `R_LIBS` and REQUIRES the packages, so a drift refuses by
 * name. A second provision of the same declaration is the cache.
 */
final case class REnv(packages: Seq[String],
                      repos: String = "https://cloud.r-project.org",
                      rscript: String = "Rscript",
                      cache: Option[Path] = None):

  def key: String =
    val text = (s"repos=$repos" +: packages.sorted).mkString("\n")
    java.security.MessageDigest.getInstance("SHA-256").nn
      .digest(text.getBytes(java.nio.charset.StandardCharsets.UTF_8)).nn
      .map(b => f"${b & 0xff}%02x").mkString.take(20)

  def dir: Path =
    cache.getOrElse(Paths.get(System.getProperty("user.home"), ".cache", "okay", "r-libs")).resolve(key)

  /** fill the library unless it is filled; its path */
  def provision(): Path =
    val d = dir
    val ready = d.resolve(".okay-ready")
    if Files.exists(ready) then d
    else
      Files.createDirectories(d.getParent): Unit
      val channel = java.nio.channels.FileChannel.open(d.getParent.resolve(s".$key.lock"),
        java.nio.file.StandardOpenOption.CREATE, java.nio.file.StandardOpenOption.WRITE)
      try
        val lock = channel.lock()
        try
          if !Files.exists(ready) then
            Files.createDirectories(d): Unit
            if packages.nonEmpty then fill(d)
            Files.writeString(ready, packages.sorted.mkString("\n")): Unit
        finally lock.release()
      finally channel.close()
      d

  private def fill(d: Path): Unit =
    val script = Files.createTempFile("okay-r-provision", ".R")
    val res = getClass.getResourceAsStream("/okay/r/provision.R")
    if res == null then throw IllegalStateException("the provisioning script is missing from the jar")
    try Files.copy(res, script, java.nio.file.StandardCopyOption.REPLACE_EXISTING): Unit
    finally res.close()
    val pb = ProcessBuilder(rscript, "--vanilla", script.toString).redirectErrorStream(true)
    // a clean environment, as a session gets: only what the script reads
    pb.environment().clear()
    pb.environment().put("OKAY_R_LIB", d.toString): Unit
    pb.environment().put("OKAY_R_PACKAGES", packages.mkString(",")): Unit
    pb.environment().put("OKAY_R_REPOS", repos): Unit
    val p =
      try pb.start()
      catch case e: java.io.IOException =>
        throw IllegalStateException(s"okay.r: could not provision: '$rscript' did not start (${e.getMessage})")
    val out = String(p.getInputStream.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8)
    if p.waitFor() != 0 then
      throw IllegalStateException(s"okay.r: could not provision ${packages.mkString(", ")}:\n" +
        out.linesIterator.toVector.takeRight(12).mkString("\n"))

  /** provision, then a session with this library that REQUIRES the packages */
  def start(env: Map[String, String] = Map.empty, modules: Seq[RModule] = Nil): RSubprocess =
    val lib = provision()
    RSubprocess.start(rscript, env.updated("R_LIBS", lib.toString),
      require = packages.map(_ -> "").toMap, modules = modules)
