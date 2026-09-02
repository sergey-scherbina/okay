package okay.agent

import okay.codec.{Json, Schema}

/**
 * SKILL.state (arxiv 2608.26263) end to end through the actual
 * context handler: a task's bounded state survives many steps at a
 * constant view size, an invalid patch never reaches Σ, and the
 * accepted patches are exactly what a real run would journal.
 */
class TestSkillState extends munit.FunSuite {

  final case class Progress(step: Int, done: Boolean = false, note: Option[String] = None)
  given Schema[Progress] = Schema.derived

  def patch(fields: (String, Json)*): Json = Json.JObj(fields.toVector)

  test("valid patches accumulate; the view stays O(1) across many steps") {
    val (ctx, _) = Handlers.context(Compact.skillState(Json.print))
    ctx.remember(Turn.System("you are a warehouse-picking agent"))

    for i <- 1 to 500 do
      val current = ctx.state.sigma
      val candidate = patch("step" -> Json.JNum(i))
      Compact.validatePatch[Progress](current, candidate) match
        case Right(_) => ctx.remember(Turn.StatePatch(candidate))
        case Left(e) => fail(s"a plain integer step should always validate: $e")
      ctx.remember(Turn.Result("pick", s"picked item $i"))

    // the view is the pin plus exactly one rendered turn — the SAME
    // size at step 500 as at step 1, the paper's headline made literal
    val view = ctx.recall
    assertEquals(view.size, 2)
    assert(view.head.isInstanceOf[Turn.System])
    val rendered = view.last match
      case Turn.User(s) => s
      case other => fail(s"expected the rendered state, got $other")
    assert(rendered.contains("\"step\":500"), rendered)
    assert(rendered.contains("picked item 500"), rendered)
    assert(!rendered.contains("picked item 1\""), "an old observation leaked into the O(1) view")

    // and the typed state decodes cleanly at any point
    assertEquals(Json.decode(summon[Schema[Progress]])(ctx.state.sigma), Right(Progress(500)))
  }

  test("an invalid patch is refused before it ever reaches Sigma — the rollback door") {
    val (ctx, _) = Handlers.context(Compact.skillState(Json.print))
    ctx.remember(Turn.StatePatch(patch("step" -> Json.JNum(1))))
    val before = ctx.state.sigma

    // "step" is an Int in the schema; a string can never decode as one
    val bad = patch("step" -> Json.JStr("not a number"))
    val result = Compact.validatePatch[Progress](before, bad)
    assert(result.isLeft, result)

    // the caller's contract: on a Left, do not remember the patch —
    // Sigma is exactly what it was, byte for byte
    assertEquals(ctx.state.sigma, before)
    assertEquals(Json.decode(summon[Schema[Progress]])(ctx.state.sigma), Right(Progress(1)))
  }

  test("a null field in an accepted patch deletes it — RFC 7396, through the real handler") {
    val (ctx, _) = Handlers.context(Compact.skillState(Json.print))
    ctx.remember(Turn.StatePatch(patch("step" -> Json.JNum(1), "note" -> Json.JStr("careful"))))
    assertEquals(Json.decode(summon[Schema[Progress]])(ctx.state.sigma), Right(Progress(1, note = Some("careful"))))

    val delete = patch("note" -> Json.JNull)
    assert(Compact.validatePatch[Progress](ctx.state.sigma, delete).isRight)
    ctx.remember(Turn.StatePatch(delete))
    assertEquals(Json.decode(summon[Schema[Progress]])(ctx.state.sigma), Right(Progress(1, note = None)))
  }

  test("the reasoning that produced a patch is never in the rendered view") {
    val (ctx, _) = Handlers.context(Compact.skillState(Json.print))
    ctx.remember(Turn.Assistant("Let me think step by step about what to do next..."))
    ctx.remember(Turn.StatePatch(patch("step" -> Json.JNum(1))))
    val view = ctx.recall
    assert(!view.exists {
      case Turn.User(s) => s.contains("think step by step")
      case _: Turn.Assistant => true
      case _ => false
    }, view.toString)
  }
}
