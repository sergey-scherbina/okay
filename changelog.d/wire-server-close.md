## wire-server-close - the server may end a live session too

`Wire.serve` only ever ended a session on the CLIENT'S `Closed`/`Close`
line; a consumer whose own state says a session is over (sign-out, a
revoked key) had no way to say so, and the socket kept running under
whatever capability it opened with. `Wire.serveClosing` gives `update`
a second answer, `(S, Boolean)`: true sends the event's own patches
first — the last thing the client sees before the door shuts is what
actually happened — then one `Msg.Close` line, then the session ends
the same way a client-sent `Close` already did. `serve` is now
`serveClosing` with the boolean pinned `false`, one loop instead of
two copies of it.

`LiveJs`'s generated client gained the other half: it dispatched only
on `"Tree"` or `"Patch"` and silently ignored anything else, so a
server-sent `Close` line arrived and did nothing — the socket closed
from the server's end regardless (sufficient to revoke the
capability), but the tab kept showing one that looked alive. It now
closes its own end on the same line, so `ws.readyState` agrees with
reality on both sides.

`TestWireClosing` covers the pure loop (patches-then-Close, a closing
event that also changes the view, a forged key still refused, `serve`
proven byte-identical to `serveClosing` pinned false, `Msg.Close`
round-tripping JSON/CBOR); `TestWire` gains one end-to-end case
against a real transport, server-initiated.
