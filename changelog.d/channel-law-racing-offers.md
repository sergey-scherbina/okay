## channel-law-racing-offers - a channel law for the window claim-then-decide exists to close

`TestChannelLaws` (okay-stream, and every suite extending
`ChannelLawsSuite`, okay-clojure's core.async view included) gains law
1b: an accepted element is delivered when `close` races four producers
OFFERING in a tight loop, 300 rounds at a random instant. Law 1 — one
parking producer — meets close mostly through the channel's own open
check, and so it let through a `Ring.pushDeciding` that reads the
closing flag BEFORE winning its position: measured here on that mutant,
law 1 green on all seven implementations, law 1b red on SentinelChannel,
relaxed and single-consumer (an accepted element lost at rounds 24, 179,
3). Found porting the suite to okay2 (spec stage 29), where the same
mutant passed the same law. Green on the real code, including
CoreAsyncChannel; AbruptChannel records it as not claimed.
