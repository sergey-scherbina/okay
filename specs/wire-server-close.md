# wire-server-close — the server may end a session too

## Overview

`Wire.serve` (specs/ui.md phase 3) already ends a live session when
the CLIENT sends `Closed` or `Close`. Nothing lets the SERVER end one:
`update: (S, Event) => S` only ever answers a new state, and the
loop's own decision to stop — `case Some(Msg.Close) | Some(Msg.Event
(Event.Closed)) => pure(s)` — only ever fires on an inbound line. A
consumer whose application state says a session is over (a signed-out
analyst, a revoked key, an expired trial) has no way to say so; the
socket keeps running under whatever capability it opened with until
the client disconnects on its own.

Found from a consumer (okay-watch, specs/face.md stage 3, BACKLOG
`face-shell-live-logout`): a sign-out press drops the session from
that product's own store, but the socket that already opened under it
keeps drawing and keeps able to act, because dropping a row in a
store the socket never re-reads changes nothing the socket can see.

## Interface

```scala
object Wire:
  /** `serve`, but `update` may also decide this event is the LAST
   * one: `(S, Boolean)`, true meaning "end the session now." True
   * sends the patches for what changed on THIS event same as always,
   * then one `Msg.Close` line, then answers the final state — the
   * server's own version of what a client's `Closed` already meant,
   * so a client that reacts to one reacts to the other the same way.
   * `serve` becomes the case that is never true. */
  def serveClosing[S](init: S)(view: S => Ui)
                      (update: (S, Event) => (S, Boolean)): Stage[String, String, S]
  def serveClosing[S](init: S, vocab: Set[String])(view: S => Ui)
                      (update: (S, Event) => (S, Boolean)): Stage[String, String, S]

  def serve[S](init: S)(view: S => Ui)(update: (S, Event) => S): Stage[String, String, S]
  def serve[S](init: S, vocab: Set[String])(view: S => Ui)(update: (S, Event) => S): Stage[String, String, S]
```

`serve` is now defined as `serveClosing` with the second half of the
pair always `false` — one body, not two copies of the loop.

The client side of the protocol already allows this: `Msg.Close` is
documented as `either` direction in `Protocol`'s own conversation
diagram, and `Protocol.closes` already reads a `Close` line the same
way regardless of who sent it. What was missing was ever WRITING one
from the server, and a consumer's `update` having a way to ask for it.

`LiveJs`'s generated client gains the other half: today it dispatches
only on an incoming `"Tree"` or `"Patch"` top-level key and silently
ignores anything else, so a `Close` line from the server would arrive
and do nothing — the socket itself only ends because the SERVER SIDE
of the transport tore it down (the Stage ending closes the
connection from the server's end regardless of what the client does
with the message), which is sufficient for the capability to be
revoked but leaves the tab showing a socket that looks alive. The
client now closes its own end on the same line, so `ws.readyState`
agrees with reality on both sides.

## Behavior

- [x] `serveClosing` sends the SAME patches for a closing event as a
      non-closing one would, then one `Msg.Close` line, then ends —
      no patch is skipped and no patch follows the close
- [x] `serve(init)(view)(update)` behaves EXACTLY as before: the
      existing `TestWire` suite passes unchanged, proving the
      `(S, Boolean)` refactor changed nothing observable when the
      boolean is always false
- [x] a `Msg.Close` line round-trips through `Protocol` the same way
      `Msg.Hello`/`Tree`/`Patch` already do (JSON and CBOR)
- [x] the pure stage test: driving `serveClosing` with events where
      one decides `true` produces the tree, the patches up to and
      including that event, a `Close` line, and nothing after — even
      when more lines follow it in the input
- [x] end to end over channels (Live-tagged, mirrors `TestWire`'s own
      channel test): a client that keeps sending events after the
      server closes gets no further patches, and the value host's
      last frame is the one from the closing event
- [ ] `live.js` closes its own WebSocket on receiving a `Close` line,
      proven the way `ui-table-browser` proves DOM behavior: a real
      browser, `TestBrowserClose` or equivalent, Live-tagged. NOT
      DONE: the JS change is one line, read as correct, but nothing
      drives a real browser against it yet — okay-watch's own gate
      (BUGS.md's TestKafkaFeed lesson applies equally here) is the
      first real-world exercise of it so far

## Out of scope

- Telling the client WHY it was closed (a reason string, a redirect
  URL). `Msg.Close` carries no payload today and this lane does not
  give it one; a consumer that wants to say more renders it in the
  tree BEFORE deciding to close, and the last frame the client drew
  is that explanation.
- A reconnect policy. `live.js` has never reconnected on its own; a
  server-initiated close does not change that, and a product wanting
  one designs it having decided WHY a socket ended (a revoked session
  should not reconnect; a network blip should).
- Closing from ACROSS sockets (session X's other tab). `update` only
  ever runs inside the one Stage it is called from; ending a
  DIFFERENT open socket for the same session is a registry problem a
  consumer would need to build itself (okay-watch's `UiSessions`
  holds no reference to a live socket to close).

## Decisions

- **One shape, not a side predicate.** A separate `closing: (S,
  Event) => Boolean` parameter beside the existing `update` was
  considered and rejected: `update` already decides everything else
  about an event, and a second function evaluated over the same
  `(S, Event)` risks disagreeing with what `update` itself decided
  (e.g. a predicate reading old `s` where `update` already moved past
  it). Folding the decision into `update`'s own answer keeps it one
  fact, decided once, mirroring `Ui.runCmd`'s existing precedent of a
  richer `update` shape (`(S, Event) => (S, Vector[Event ! Async])`)
  for a capability `run`'s plain `update` does not need.
- **`serve` is `serveClosing` with the boolean pinned false**, not two
  parallel implementations. The two roads disagreeing was already a
  hard lesson here (react-host-vocab): a second definition of the same
  loop is a second place for it to drift.
- **The server tears down the transport regardless of the client.**
  The Stage ending IS what closes a WebSocket from the server side
  today (a client-sent `Closed` already works this way); a
  server-sent `Close` line is additive politeness for a client that
  listens, not the mechanism that revokes capability. That mechanism
  — the Stage returning `pure` instead of looping — is unchanged.

## Results

**LANDED 2026-09-19** (`2e7b7e52`): `Wire.serveClosing` and `serve`
refactored onto it, `LiveJs`'s generated client closing its own socket
on a `Close` line, `TestWireClosing` (five pure tests) and one new
end-to-end case in `TestWire`. Every Behavior box above is checked.

**A follow-up lane (`wire-close-test-hang`, `6b805fc3`) found and
fixed a real problem in the original landing itself:** the new
end-to-end test was never actually executed before it landed. `sbt
test` — the gate `wire-server-close` ran — excludes `Live`-tagged
suites by default, and `TestWire` is tagged at the class level, so
the test compiled and was described in the lane's own changelog entry
as passing, but nothing had run it. Run for real with `sbt
integrationTest`, it hung forever: it ended its scripted client's own
event feed with `feed.close()`, which does not stop `Wire.client`'s
`forward` loop (that loop only ever ends on its OWN `Event.Closed`,
per the hybrid rule `Wire.client` already documents) — `fiber.join()`
is a raw thread park with no timeout, and parked for over two hours
before being killed by hand. `okay-ui/BUGS.md` has the full account.

The fix sends `Event.Closed` as the client's own last event, matching
every other test in the file, and drops a trailing press the original
test used to race against the server's own close — `up` and `down`
are two independently scheduled fibers, so an event sent after the
closing one has no ordering guarantee against when the server acts on
it, and the first version of the test was asserting on that race
rather than on `serveClosing` itself. Verified: 5 repeats of the
fixed suite clean (`okayUiJVM/testOnly okay.ui.TestWire` with the
class's own Live tag disabled locally to drive it directly, since
that is what actually executes a `Live`-tagged suite's tests without
sbt's `set`-based `testOptions` override — which, separately, wedged
for 30 minutes with zero CPU on first attempt and had to be killed;
the working substitute for a one-off check is disabling the tag
locally rather than fighting `set`). The full monorepo `sbt test`
afterward: 0 failures, 736+ per-module suites green.

**The lesson this leaves behind, beyond the one bug:** a `Live`-tagged
test is invisible to the default gate in BOTH directions. A broken
one and a hung one look identical to `sbt test` — silence — so
landing anything that touches a `Live` suite needs that suite
actually run once, with `sbt integrationTest` or an equivalent, before
the claim is released. A changelog line describing what a test does
is not evidence that it ran, and this lane is the proof: it said so,
correctly, about a test that had never once executed.
