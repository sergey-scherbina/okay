## stack-bytes-warm-flake - TestStackBytes warms a door until two readings agree

okay-codec's "every door is flat past the threshold" failed in a whole
`affected` gate on 2026-09-29: Cbor.read[Tree] read 256 KB at 100 levels
(its cold figure) and 16 KB at 400. Two thousand warm-up rounds had not
compiled the door on a loaded box, so the law measured the JIT. `needsWarm`
now warms again until two readings in a row agree, at most five times,
and keeps the least, since a compiled frame only makes the answer
smaller. Alone it reads 16 KB at 8, 100 and 400 levels for both doors.
