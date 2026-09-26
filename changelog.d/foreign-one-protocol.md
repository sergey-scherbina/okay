## foreign-one-protocol — five operations on the wire, and the protocol as a transcript (2026-09-26)

Stage 2d of specs/foreign-one.md. `frame` folds into `call`: a table is the
call's first argument (the Arrow stream itself, on the Arrow road) and
`"table": true` asks for a table back. Every far side now serves five
operations — `call` (a name, or a held object's method or attribute; `held`;
`table`), `program`, `continue`, `forget`, `release` — besides the
handshake's `configure`/`auth` and `verify`. Python shim 9, TypeScript 9
(which gains tables: it never served `frame`, though the facade's spec said
it did), R 12, Go/Rust/Haskell libraries 9.

The protocol is written down once, as `okay/py/wire.txt` in okay-py's jar:
fourteen request/answer steps and the module that answers them.
`TestWireTranscript` (default gate) holds the host to the requests,
`TestWireTranscriptPython` (Live) holds the reference shim to the answers;
a new language's library is written against it. Mutant: the host spelling
the flag `tabl` fails the transcript at step 5.

Narrowed, and why (Decisions 13, 14): `Frame` stays a TYPED case on the host
(every caller has one table, first; folding it would cost ~30 sites and the
result type for no new call), and a framing of several Arrow tables waits for
a call that has two. Live: okay-py 227, okay-r 98, okay-rust 35, cluster 30,
workflow 12.
