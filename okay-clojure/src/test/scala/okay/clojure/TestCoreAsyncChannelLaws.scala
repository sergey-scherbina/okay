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
))
