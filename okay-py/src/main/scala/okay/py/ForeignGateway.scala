package okay.py

import java.nio.file.{Files, Path}

/** a worker's command line and its environment: what a process that okay
 * does not start itself needs to start one */
final case class WorkerCommand(command: Vector[String], env: Map[String, String])

/**
 * EVERY stdio worker on the network (polyglot-one-wire stage 7): the gateway
 * shipped in this jar (`/okay/py/gateway.py`, standard-library Python)
 * listens on TCP and, on every connection, starts the worker command and
 * relays the wire to it. TLS (`OKAY_TLS_CERT`, `OKAY_TLS_KEY`) and the HMAC
 * challenge (`OKAY_WIRE_SECRET`, `OKAY_WIRE_SECRET_FILE`) are the GATEWAY's,
 * so a Python, TypeScript or Haskell worker is reached with `connect`,
 * `WireAuth` and `WireSecurity` exactly as a Go or Rust server is, and
 * changes not at all.
 *
 * {{{
 * val (port, gateway) = ForeignGateway.start(ForeignWorker.pythonCommand(modules = Seq(m)),
 *   env = Map("OKAY_WIRE_SECRET" -> secret))
 * val w = ForeignWorker.connect("127.0.0.1", port)       // with a given WireAuth
 * }}}
 */
object ForeignGateway:

  /** the gateway script from this jar, as a file */
  def script(): Path =
    val f = Files.createTempFile("okay-gateway", ".py")
    val res = getClass.getResourceAsStream("/okay/py/gateway.py")
    if res == null then throw IllegalStateException("the gateway resource is missing from the jar")
    try Files.copy(res, f, java.nio.file.StandardCopyOption.REPLACE_EXISTING) finally res.close()
    f.toFile.deleteOnExit()
    f

  /** the gateway's own command line, for running it where okay is not */
  def command(worker: WorkerCommand, listen: String, python: String = "python3"): Vector[String] =
    Vector(onPath(python), script().toString, "--listen", listen, "--") ++ worker.command

  /** the gateway's environment is clean, so the interpreter is found HERE */
  private def onPath(exe: String): String =
    if exe.contains('/') then exe
    else sys.env.getOrElse("PATH", "").split(java.io.File.pathSeparator).iterator
      .map(d => java.nio.file.Paths.get(d, exe)).find(Files.isExecutable(_)).map(_.toString).getOrElse(exe)

  /**
   * Start a gateway serving `worker` on `listen` (port 0: any), with `env`
   * (its secret, its TLS files) beside the worker's own environment. Answers
   * the port it bound and its process. A gateway that cannot start says so
   * by name.
   */
  def start(worker: WorkerCommand, listen: String = "127.0.0.1:0", env: Map[String, String] = Map.empty,
            python: String = "python3"): (Int, Process) =
    val pb = ProcessBuilder(command(worker, listen, python)*)
    pb.environment().clear()                 // the clean-env rule, as for a worker
    (worker.env ++ env).foreach((k, v) => pb.environment().put(k, v))
    pb.redirectError(ProcessBuilder.Redirect.INHERIT)
    val p = pb.start()
    val first = java.io.BufferedReader(java.io.InputStreamReader(p.getInputStream, "UTF-8")).readLine()
    val port = Option(first).map(okay.codec.Json.parse).collect {
      case okay.codec.Json.JObj(fs) => fs.toMap.get("listening")
    }.flatten.collect { case okay.codec.Json.JStr(a) => a.split(":").last.toInt }
    port match
      case Some(n) => (n, p)
      case None =>
        p.destroy()
        throw IllegalStateException(s"the gateway did not say where it listens (it said: $first)")
