package okay.agent

import okay.codec.{Json, Schema}

/**
 * The tools an agent or an MCP server is given — ONE declaration each,
 * three interpretations (specs/optics-outside.md, stage 2).
 *
 * What stood here was three declarations per tool. The JSON Schema was
 * written by hand; a separate `Map[String, ToolCall => String]` re-read
 * the same field names as string LITERALS (`str(c, "text")`), so a
 * rename broke the tool silently; and the tool's name was written
 * twice, in two structures keyed by it, so a tool could be declared and
 * undispatched or dispatched and undeclared.
 *
 * None of that needed inventing away, only using. `Schema[A]` is the
 * optic algebra `specs/optics.md` names in its own Overview, and
 * `okay.codec.JsonSchema` is documented as the FOURTH algebra over it
 * — "so a tool's signature cannot drift from its parser". The three
 * interpretations of one `Schema[A]`:
 *
 *   - DECLARE — `spec`, which needs no call at all and is what the
 *     model is told;
 *   - DECODE — the arguments of an actual call, by the same schema
 *     that declared them;
 *   - DISPATCH — the handler, over the value the decode produced.
 *
 * `specs` and `table` are drawn from ONE vector, so their name sets are
 * equal by construction — the property `Router.describe` gives a route
 * table in stage 1, and the reason the two stages are written the same
 * way. The seam is unchanged: `table` is still the
 * `Map[String, ToolCall => String]` that `Mcp.Server`, `Handlers.tools`
 * and `Stepper` already take. This is a way to WRITE a tool, not a new
 * protocol.
 *
 * One inconsistency is left and cannot be designed away, because the
 * two shapes disagree about it: a `Map` keeps the last of a duplicate
 * name and a `Seq` keeps both. `duplicates` names them rather than
 * pretending.
 */
final class Toolbox private (val entries: Vector[Toolbox.Entry]) {

  /**
   * The usual kind: the argument type IS the declaration. The
   * properties, their types and which of them are required all come
   * from `Schema[A]`, and the same schema decodes the call.
   */
  def on[A](name: String, description: String)(run: A => String)
           (using s: Schema[A]): Toolbox =
    add(new Toolbox.Entry(ToolSpec[A](name, description), c =>
      ToolSpec.args[A](c) match
        case Right(a) => run(a)
        case Left(e) => Toolbox.failed(name, e)))

  /**
   * A tool whose argument is arbitrary JSON.
   *
   * Not a wart: `StateMcp`'s tools take a JSON Merge Patch (RFC 7396),
   * which is arbitrary by definition — there is no case class to derive
   * from, and inventing one would be a lie about the protocol. What
   * `raw` still buys is the PAIRING: the name is written once, and the
   * declaration and the handler cannot come apart. Half the win, and
   * saying which half is the point of having the constructor.
   */
  def raw(name: String, description: String, schema: Json)(run: Json => String): Toolbox =
    add(new Toolbox.Entry(ToolSpec(name, description, schema), c => run(c.args)))

  def add(e: Toolbox.Entry): Toolbox = new Toolbox(entries :+ e)

  def ++(that: Toolbox): Toolbox = new Toolbox(entries ++ that.entries)

  /** DECLARE: what the model is told */
  def specs: Seq[ToolSpec] = entries.map(_.spec)

  /** the existing seam, from the same values */
  def table: Map[String, ToolCall => String] =
    entries.map(e => e.name -> (e.handle(_))).toMap

  /** dispatch one call; `None` is "no such tool", which is the caller's
   * error to phrase */
  def call(c: ToolCall): Option[String] = entries.find(_.name == c.name).map(_.handle(c))

  def names: Vector[String] = entries.map(_.name)

  /** names declared more than once, in declaration order */
  def duplicates: Vector[String] =
    names.groupBy(identity).collect { case (n, xs) if xs.length > 1 => n }
      .toVector.sortBy(names.indexOf)
}

object Toolbox {

  /** the zero: what a fold over several boxes starts from, and what a
   * module answers when it contributes no tools */
  val empty: Toolbox = new Toolbox(Vector.empty)

  /** start a box from the companion, so a declaration never opens with
   * `.empty.` — the same shape `Router.on` has in okay-http */
  def on[A](name: String, description: String)(run: A => String)(using Schema[A]): Toolbox =
    empty.on(name, description)(run)

  def raw(name: String, description: String, schema: Json)(run: Json => String): Toolbox =
    empty.raw(name, description, schema)(run)

  /**
   * One tool: its declaration and its answer, together.
   *
   * No type parameter — the argument type is consumed by `on`, where
   * the `Schema[A]` is still in scope, so a box of tools taking
   * different argument types is an ordinary `Vector` and needs neither
   * an existential wrapper nor a cast.
   */
  final class Entry private[agent] (val spec: ToolSpec,
                                    private[agent] val run: ToolCall => String):
    def name: String = spec.name
    /** DECODE, then DISPATCH — with the schema that declared it */
    def handle(c: ToolCall): String = run(c)

  /**
   * A tool that cannot do the thing answers with DATA.
   *
   * A model handed an exception learns nothing; one handed an error it
   * can read may explain itself or try again. That was already
   * `BoardTools`' rule, written per handler; here it is the decode
   * failure's answer for every tool.
   */
  def failed(tool: String, message: String): String =
    Json.print(Json.JObj(Vector("error" -> Json.JStr(s"$tool: $message"))))
}
