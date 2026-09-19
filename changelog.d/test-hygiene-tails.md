## test-hygiene-tails - the two things yesterday's lanes filed and did not fix

Both were found while doing something else, filed the moment they were
found, and closed here rather than left to age.

NINE FEATURE WARNINGS THAT NO ORDINARY COMPILE SHOWS.
`TestDirectApplicative.scala` and `TestDirectSelective.scala` use the
colourless-val spelling, which goes through `selfColor` - an implicit
CONVERSION - and neither file had
`import scala.language.implicitConversions`. They are invisible
without `-feature`, which is why AGENTS.md says to hunt with it and
why they had ridden through every green gate. The core's test scope
now compiles clean UNDER the flag, on all three platforms.

THE PORT SURVEY, RE-RUN. `okay.jetty.TestPeerAddress` failed in a full
matrix ("the server saw no peer address") and passed alone on the same
tree; it binds a real port and runs a loopback request, which is the
definition AGENTS.md gives for `integrationTest`. Tagged `Live`, with
the reason where the tag is - the policy is explicit that a flake in
an untagged suite gets the TAG, not a retry loop and not a widened
assertion. Verified it MOVED rather than vanished: 0 tests in the
default run, 1 passing under `--include-tags=Live`.

AND THE ASSERTION WAS SHARPENED INSTEAD. Two different failures wore
one message, which is why the flake could not be told apart: the
handler may never have run, or it ran and the transport reported no
peer. A `served` flag distinguishes them for whoever reads the next
one.

THE SURVEY'S FALSE POSITIVES ARE NOW WRITTEN DOWN. 36 hits, 32 tagged,
one real; the other three are okay-ui's `Wire.serve(0)`, whose `0` is
an initial STATE and not a port. The grep is still the right first
pass - check what the `0` is before tagging. Recorded in AGENTS.md so
the next survey does not re-investigate them.
