package okay.foreign

import okay.codec.Schema

/**
 * TypeScript's side of okay-py's API (typescript-types, T1): `Py.*`, with
 * every value in the shape okay's JSON codec writes (`Shape.json`).
 *
 * That shape is what an HTTP client of an okay server sees (Scala backend,
 * TypeScript frontend), what okay-ts hands a TypeScript program in the
 * browser (both in the browser), and — through this API — what a
 * TypeScript worker receives (both on the backend). ONE declaration file,
 * `Stubs.typescript(schemas*)`, therefore types a TypeScript module in all
 * three, and a type is written once, in Scala.
 *
 * {{{
 * val w = TsWorker.start(dir, modules = Seq("shop"))
 * Ts.fn[Totals]("shop:total").calling(Ts.callbacks(priceOf))(order)
 * }}}
 */
object Ts:
  private given Shape = Shape.json

  def fn[Out: Schema](address: String): Foreign.Fn[Out] = Foreign.Fn(address)
  def hold(address: String): Foreign.Hold = Foreign.Hold(address)
  def program[Out: Schema](address: String): Foreign.ProgramOf[Out] = Foreign.ProgramOf(address)
  def callback[Arg: Schema, Res: Schema](name: String): Foreign.CallbackOf[Arg, Res] = Foreign.CallbackOf(name)
  def callbacks[F[+_]](cbs: Foreign.Callback[F]*): Foreign.Callbacks[F] = Foreign.callbacks(cbs*)

  /** the TypeScript type of these callbacks as a program's operations
   * (typescript-types T12), for `effects<Name>()` in a worker module */
  def ops[F[+_]](name: String, cbs: Foreign.Callbacks[F]): String =
    okay.codec.Stubs.typescriptOps(name, cbs.all.map(c => (c.name, c.types)))
