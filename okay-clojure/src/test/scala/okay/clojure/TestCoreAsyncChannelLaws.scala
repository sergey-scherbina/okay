package okay.clojure

import okay.ChannelLawsSuite

/**
 * A core.async channel, seen through `CoreAsyncChannel`, answers for the
 * SAME `Channel` laws every okay channel does — order, no duplication,
 * a closed channel accepting nothing, and the DRAIN tier: acceptance is
 * final and close ends the stream only after the buffer is spent
 * (specs/clojure.md, stage 3). okay-stream's battery, over this list.
 */
class TestCoreAsyncChannelLaws extends ChannelLawsSuite(List(
  ("CoreAsyncChannel", true, cap => CoreAsync.channel[Int](math.max(1, cap))),
)) {
  // Live since flaky-suites-live (2026-09-28): this one law timed out
  // (65 s) twice in ci-runner whole builds under load and passed alone,
  // holding the push. The same law's single-consumer mode is tracked as
  // backlog sentinel-single-consumer-lost-end; the other laws stay in.
  override def munitTests(): Seq[Test] =
    super.munitTests().map(t =>
      if t.name.startsWith("law: the end is delivered when close races offers on six channels at once")
      then t.tag(new munit.Tag("Live")) else t)

  override protected def describe(c: okay.Channel[?]): String = c match
    case cc: CoreAsyncChannel[?] => cc.debugState
    case other => super.describe(other)
}
