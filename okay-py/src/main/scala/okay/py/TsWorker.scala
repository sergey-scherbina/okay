package okay.py

import java.nio.file.{Files, Path, Paths}

/**
 * A TypeScript worker on the okay wire (specs/typescript.md, stage 1).
 *
 * Node runs `.ts` by stripping its types, so a TypeScript process speaks
 * the same line protocol as okay-py's Python shim: `start` writes the
 * worker (`worker.ts`) and its library (`okay.ts`) beside the user's
 * modules and runs `node worker.ts`, and the engine it answers is the one
 * Python uses. `Py.fn`, `Py.callback`, `Py.hold`, `Py.program`, `Durable`
 * — all of it drives TypeScript unchanged:
 *
 * {{{
 * val w = TsWorker.start(dir, modules = Seq("shop"))       // dir/shop.ts
 * Py.fn[Double]("shop:total").calling(Py.callbacks(priceOf))(order)
 * }}}
 *
 * A module imports what it uses from `./okay.ts`: `call` (a callback into
 * okay), and `done`, `perform`, `then` (programs as data, multi-shot).
 * `okay.codec.Stubs.typescriptWire` writes the types of the values it
 * receives.
 */
object TsWorker:

  private def resource(name: String): String =
    val res = getClass.getResourceAsStream(s"/okay/ts/$name")
    if res == null then throw IllegalStateException(s"okay.py: /okay/ts/$name is missing from the jar")
    try String(res.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8) finally res.close()

  /** the library a module imports, as this jar ships it */
  def library: String = resource("okay.ts")

  /** the worker process's source, as this jar ships it */
  def worker: String = resource("worker.ts")

  /**
   * Write `okay.ts` and `worker.ts` into `dir`, beside `dir/<module>.ts`
   * for each module named, and start `node worker.ts` there. The child's
   * environment is clean, plus `env` and the module list.
   */
  def start(dir: Path, modules: Seq[String], node: String = "node",
            env: Map[String, String] = Map.empty)(using WireFormat, WireCompression, WireDeadline): ForeignWorker =
    val c = command(dir, modules, node, env)
    ForeignWorker.speaking(c.command, c.env)

  /** the worker as a COMMAND, for a process okay does not start itself:
   * `ForeignGateway` runs one per connection (stage 7) */
  def command(dir: Path, modules: Seq[String], node: String = "node",
              env: Map[String, String] = Map.empty): WorkerCommand =
    Files.writeString(dir.resolve("okay.ts"), library): Unit
    Files.writeString(dir.resolve("worker.ts"), worker): Unit
    modules.foreach { m =>
      if !Files.exists(dir.resolve(s"$m.ts")) then
        throw IllegalArgumentException(s"okay.py: no module $m.ts in $dir")
    }
    val list = modules.map(m => s"$m=${dir.resolve(s"$m.ts")}").mkString(";")
    WorkerCommand(Vector(onPath(node), dir.resolve("worker.ts").toString), env.updated("OKAY_TS_MODULES", list))

  /** the child's environment is empty, so the executable is found HERE */
  private def onPath(exe: String): String =
    if exe.contains('/') then exe
    else sys.env.getOrElse("PATH", "").split(java.io.File.pathSeparator).iterator
      .map(d => Paths.get(d, exe)).find(Files.isExecutable(_)).map(_.toString).getOrElse(exe)
