/** The row alias (specs/scala2-facade.md, stage 16). okay writes a row
 * `State % Int + Writer % String`; the facade's row is an intersection
 * of capabilities, which Scala 2 spells `with`. This alias lets Scala 2
 * write it the way okay does, `State[Int] + Writer[String]`. It is
 * declared HERE, in Scala 2 source — a user declares it once in their
 * own package object — because a Scala 3 top-level alias is invisible
 * to scalac 2.13. */
package object scala2probe {
  type +[R, S] = R with S
}
