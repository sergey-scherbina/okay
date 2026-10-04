package okay2.codec

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

/**
 * Are two TypeScript copies of the same types really the same?
 * (okay-codec's TsCheck, typescript-types T4) — for a team that keeps a
 * hand-written TypeScript model beside the Scala one.
 *
 * `tsc` decides, not okay: for every type the GENERATED file declares, a
 * line typed `Same<Generated.X, HandWritten.X>` compiles only when each is
 * assignable to the other. A renamed field, a field made optional, a
 * missing type — each is a `tsc` error, reported here by the TYPE'S NAME.
 */
object TsCheck {

  /** the generated declarations of `schemas`, against the file `handwritten` */
  def sameAs(handwritten: Path, schemas: Schema[_]*): Either[Vector[String], Unit] =
    same(Stubs.typescript(schemas: _*), Files.readString(handwritten))

  /** two declaration files' texts: every type the first declares, compared */
  def same(generated: String, handwritten: String, tsc: String = "tsc"): Either[Vector[String], Unit] =
    TsTypes.parse(generated) match {
      case Left(why) => Left(Vector(s"the generated declarations do not parse: $why"))
      case Right(decls) =>
        // the leaf aliases (`Int`, `Long`…) are the generated file's helpers:
        // a hand-written copy says `number`, and `Same` compares through them
        val names = decls.map {
          case TsTypes.Decl.Interface(n, _) => n
          case TsTypes.Decl.Alias(n, _) => n
        }.filterNot(TsTypes.leafAliases)
        val dir = Files.createTempDirectory("okay-ts-check")
        val _ = Files.writeString(dir.resolve("generated.ts"), generated)
        val _ = Files.writeString(dir.resolve("handwritten.ts"), handwritten)
        val header = Vector(
          """import type * as G from "./generated.ts";""",
          """import type * as H from "./handwritten.ts";""",
          "type Same<A, B> = [A] extends [B] ? ([B] extends [A] ? true : false) : false;")
        val lines = header ++ names.map(n => s"export const same_$n: Same<G.$n, H.$n> = true;")
        val _ = Files.writeString(dir.resolve("check.ts"), lines.mkString("\n") + "\n")
        val p = new ProcessBuilder(tsc, "--noEmit", "--strict", "--allowImportingTsExtensions",
          "--target", "es2022", "--module", "nodenext", "check.ts")
          .directory(dir.toFile).redirectErrorStream(true).start()
        val out = new String(p.getInputStream.readAllBytes(), UTF_8)
        if (p.waitFor() == 0) Right(())
        else {
          // each error names its line in check.ts: a line is a type
          val At = """check\.ts\((\d+),\d+\): error (.*)""".r
          val found = out.linesIterator.collect { case At(line, msg) =>
            names.lift(line.toInt - header.size - 1).fold(s"check: $msg")(n => s"$n: $msg")
          }.toVector.distinct
          Left(if (found.nonEmpty) found else Vector(out.trim))
        }
    }
}
