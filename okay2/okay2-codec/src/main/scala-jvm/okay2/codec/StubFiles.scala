package okay2.codec

import java.nio.file.{Files, Path}

/**
 * The other side's declarations WRITTEN, as a build step (okay-codec's
 * StubFiles, typescript-types T2): the types are written once, in Scala,
 * and every build regenerates the TypeScript (or Python) that describes
 * them, into the other codebase's tree:
 *
 * {{{
 * object WriteTypes {
 *   def main(args: Array[String]): Unit = {
 *     val _ = StubFiles.typescript(Paths.get(args(0)), Order.schema, Shape.schema)
 *   }
 * }
 * }}}
 *
 * A file is rewritten only when its text CHANGED, so an unchanged model
 * does not wake a frontend's watcher. Each answers whether it wrote.
 */
object StubFiles {

  /** `Stubs.typescript` — the JSON shape */
  def typescript(file: Path, schemas: Schema[_]*): Boolean = write(file, Stubs.typescript(schemas: _*))

  /** `Stubs.python` — the shape okay-py sends a Python worker */
  def python(file: Path, schemas: Schema[_]*): Boolean = write(file, Stubs.python(schemas: _*))

  /** write `text` to `file` unless it already holds exactly that */
  def write(file: Path, text: String): Boolean =
    if (Files.exists(file) && Files.readString(file) == text) false
    else {
      Option(file.getParent).foreach(p => { val _ = Files.createDirectories(p) })
      val _ = Files.writeString(file, text)
      true
    }
}
