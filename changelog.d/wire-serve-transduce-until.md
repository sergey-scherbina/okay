## wire-serve-transduce-until - the wire server as one `Stage.transduceUntil`

`Wire.serveClosing` (okay-ui) — the transport-agnostic server-driven
UI session, lines in, lines out — was a hand-written `loop`/`step`
pair over `Stage.await`, found by loop-audit counting the `!.loop`
doors and left there because it is a `Stage`. It is now stage 3's
first production consumer: `Stage.transduceUntil((init, first))(step(v),
_._1)`, the state the server's own plus the tree it showed, `step`
answering `Left` for a dropped line (damage, a forged key, a stray
hello) and for an event's patches, `Right(s)` on a `Close` from either
side (after the patches and the one `Msg.Close` line the closing road
sends), and `end = _._1` when the line stream ends. The line that
arrives before any hello is stepped once before the session starts,
as before. No behaviour change: `TestWire`, `TestWireClosing`,
`TestProtocol`, `TestHybrid`, `TestBrowserVocab`, `TestVocab`
unchanged, 29 green, no warnings. specs/fold-until.md Results, guide
§5. Landed as 004eda1e.
