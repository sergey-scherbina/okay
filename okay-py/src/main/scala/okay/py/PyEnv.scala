package okay.py

import java.nio.file.{Files, Path, Paths}

/**
 * The Python environment DECLARED in code (foreign-managed-env,
 * specs/foreign-highlevel.md stage 8):
 *
 * {{{
 * val env = PyEnv(python = "3.12", packages = Map("numpy" -> ">=2,<3"))
 * val worker = env.start()
 * }}}
 *
 * `provision` builds it with `uv` — a venv of that Python, the packages
 * installed — into a cache directory keyed by a hash of the declaration,
 * and answers its interpreter. The same declaration is the same
 * directory: a fresh machine and CI get the same environment without a
 * README step, and the second provision is the cache. `verify` (which
 * exists) stays the check that the environment IS right; this is what
 * makes it right in the first place.
 *
 * A version spec is written as pip takes it (`>=2,<3`); a bare version
 * (`1.16.0`) means exactly that one.
 */
final case class PyEnv(python: String = "3.12",
                       packages: Map[String, String] = Map.empty,
                       uv: String = "uv",
                       cache: Option[Path] = None):

  /** the requirements as pip reads them, in a stable order */
  def requirements: Vector[String] =
    packages.toVector.sortBy(_._1).map { (name, spec) =>
      if spec.isEmpty then name
      else if spec.head.isDigit then s"$name==$spec"
      else s"$name$spec"
    }

  /** what the cache is keyed by: the Python and the requirements */
  def key: String =
    val text = (s"python=$python" +: requirements).mkString("\n")
    java.security.MessageDigest.getInstance("SHA-256").nn
      .digest(text.getBytes(java.nio.charset.StandardCharsets.UTF_8)).nn
      .map(b => f"${b & 0xff}%02x").mkString.take(20)

  def dir: Path =
    cache.getOrElse(Paths.get(System.getProperty("user.home"), ".cache", "okay", "py-envs")).resolve(key)

  /** build the environment unless it is built; its interpreter */
  def provision(): Path =
    val d = dir
    val interpreter = d.resolve("bin").resolve("python")
    val ready = d.resolve(".okay-ready")
    if Files.exists(ready) then interpreter
    else
      Files.createDirectories(d.getParent): Unit
      val lockFile = d.getParent.resolve(s".$key.lock")
      val channel = java.nio.channels.FileChannel.open(lockFile,
        java.nio.file.StandardOpenOption.CREATE, java.nio.file.StandardOpenOption.WRITE)
      try
        val lock = channel.lock()
        try
          // another process may have built it while this one waited
          if !Files.exists(ready) then
            // a directory WITHOUT the mark is a build that died: never trusted
            if Files.exists(d) then PyEnv.delete(d)
            PyEnv.run(Vector(uv, "venv", "--python", python, d.toString), "create the venv")
            if requirements.nonEmpty then
              PyEnv.run(Vector(uv, "pip", "install", "--python", interpreter.toString) ++ requirements,
                "install the packages")
            Files.writeString(ready, requirements.mkString("\n")): Unit
        finally lock.release()
      finally channel.close()
      interpreter

  /**
   * Provision, start a worker on it, and `verify` the declared packages:
   * a drift refuses by name, with the worker never handed out.
   */
  def start(env: Map[String, String] = Map.empty, modules: Seq[PyModule] = Nil)(using WireFormat, WireCompression): ForeignWorker =
    val w = ForeignWorker.start(provision().toString, env, modules)
    val drift = w.verify(packages.map((n, _) => n -> ""))
    if drift.isEmpty then w
    else
      w.close()
      throw IllegalStateException(s"okay.py: the provisioned environment is not the declared one:\n  ${drift.mkString("\n  ")}")

object PyEnv:
  private[py] def run(cmd: Vector[String], what: String): Unit =
    val pb = ProcessBuilder(cmd*).redirectErrorStream(true)
    val p =
      try pb.start()
      catch case e: java.io.IOException =>
        throw IllegalStateException(s"okay.py: could not $what: '${cmd.head}' did not start (${e.getMessage}) — is uv installed?")
    val out = String(p.getInputStream.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8)
    val code = p.waitFor()
    if code != 0 then
      throw IllegalStateException(s"okay.py: could not $what (exit $code):\n${out.linesIterator.toVector.takeRight(12).mkString("\n")}")

  private[py] def delete(d: Path): Unit =
    val all = Files.walk(d)
    try all.sorted(java.util.Comparator.reverseOrder()).forEach(p => Files.delete(p))
    finally all.close()
