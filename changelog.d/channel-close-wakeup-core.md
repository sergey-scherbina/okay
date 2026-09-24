## channel-close-wakeup-core - the core's "hang" was the law's own cost; laws 1b/1c bounded at 8192

TestChannelLaws law 1b on SentinelChannel/unbounded "timed out (30 s)"
in two full gates under load, once after adaptive-seal-race. No receiver
was ever found parked. Measured instead:
- The law closes after a random PAUSE, and an unbounded channel accepts
  every offer until then. Under 24 burners every round reached the
  50 000-per-producer bound (200 000 elements), and 300 rounds took
  27.9 s against munit's 30 s.
- Without load it took 0.15 s. On the bounded ring under load it took
  0.7 s.
- 6000 rounds under burners never hung.

Laws 1b and 1c now cap each producer at 8192 offers, in both okay-stream
and okay2-stream. Without load a round accepts at most ~3 700, so the
race is unchanged. Under load the law takes 5.1 s.

Also found: the decide-before-claim `Ring` mutant that law 1b was written
to catch PASSED a full run here, both before this change and after. This
is filed as `channel-law-1b-mutant-sensitivity`.
