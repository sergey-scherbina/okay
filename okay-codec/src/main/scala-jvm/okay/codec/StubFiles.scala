package okay.codec

import java.nio.file.{Files, Path}

/**
 * The other side's declarations WRITTEN, as a build step
 * (typescript-types T2, specs/typescript-types.md): the types are written
 * once, in Scala, and every build regenerates the TypeScript (or Python)
 * that describes them, into the other codebase's tree.
 *
 * {{{
 * object WriteTypes:
 *   def main(args: Array[String]): Unit =
 *     StubFiles.typescript(Paths.get(args(0)), summon[Schema[Order]], summon[Schema[Shape]]): Unit
 * }}}
 *
 * and in build.sbt, a task a frontend build (or CI) runs:
 *
 * {{{
 * lazy val tsTypes = taskKey[Unit]("regenerate the frontend's types")
 * tsTypes := (Compile / runMain).toTask(" my.app.WriteTypes ../frontend/src/model.ts").value
 * }}}
 *
 * A file is rewritten only when its text CHANGED, so an unchanged model
 * does not wake a frontend's watcher. Each answers whether it wrote.
 */
object StubFiles:

  /** `Stubs.typescript` — the JSON shape: an HTTP client, okay-ts, `Ts` */
  def typescript(file: Path, schemas: Schema[?]*): Boolean = write(file, Stubs.typescript(schemas*))

  /** `Stubs.python` — the shape okay-py sends a Python worker */
  def python(file: Path, schemas: Schema[?]*): Boolean = write(file, Stubs.python(schemas*))

  /** write `text` to `file` unless it already holds exactly that */
  def write(file: Path, text: String): Boolean =
    if Files.exists(file) && Files.readString(file) == text then false
    else
      Option(file.getParent).foreach(Files.createDirectories(_): Unit)
      Files.writeString(file, text): Unit
      true
