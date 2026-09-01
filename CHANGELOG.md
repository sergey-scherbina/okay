# Changelog

## ui-dom-patch — the raw-DOM Backend; the patch consumer arrives
Completed: 2026-09-01 (landed as 1624244)
React-less DOM over js.Dynamic, zero dependencies: React.elem is the
build plan, Ui.patch keeps the mirror events interpret against
(React.event, one delegated listener per kind), paths walk
childNodes. Proven against a fake document under Node: the law
(patching frames equals building the last), a shuffle creates zero
nodes, narrow patches mutate in place, events round-trip. okayUi JS
tests exist again (js test dir replaces sources := Seq()). Matrix
1106.

## persist-stage1 — the consumers prove the seam
Completed: 2026-09-01 (landed as 8501246; spec first af18ad6)
Compaction (keep-latest-per-key, offsets preserved as holes,
exclusive with retention) forced disk format v2 — frames carry
their offset — and bought the evolution test both ways (a forged
v1 segment reads; a v1 active segment is closed and a v2 rolled).
Typed view: four-byte version envelope over CBOR, byte-level
upcasts via Typed.step, every failure Decoded.Bad(offset, error).
Offsets (commit-as-record, refold-on-restart, lag), Snapshots
(put/latest, the ui lane's ask), Streams (stream/tail as
Chunk ! Produce + Async; dropped history stops by declared
OnTooEarly). okay-agent: TopicJournal = Durable.Journal over a
keyed topic, intent and completion separate records, the whole
crash-window battery green against it. okay-persist now depends on
the core; okay-agent on okay-persist. Merge read alone: exit 0.
Full matrix green (persist 38 JVM / 13 JS / 13 Native; agent +5).

## security-argon2 — the satellite that buys a dependency
Completed: 2026-09-01 (landed as 16b8d58)
New module okay-security-argon2 (jvm, Bouncy Castle) — a memory-hard
KDF cannot be had from the JDK, so this module pays while the core
keeps its zero. PHC stored form (portable, parameters ride the row),
RFC 9106 vector pins the provider, absurd parameters refuse before
allocating, verifyAny reads a mixed pbkdf2/argon2id store. Matrix
1072. okay-security's staged spec is now fully shipped, 0 through 5.

## security-es256 — the raw-vs-DER dance, danced
Completed: 2026-09-01 (landed as 9995966)
Es256 is the conversion alone — pure, total both ways, shared, its
battery on JS too (the build's := became += for that). EcPublic/
EcPair keys, the key still decides the algorithm with three kinds in
the ring, Jwks learns kty:EC, Oidc gains ES256 IdPs for free.
Merge read alone: exit 0. Matrix green.

## security-oidc — user login from parts on the shelf
Completed: 2026-09-01 (landed as 4951cd1; the release entry went out
one commit early again — a refused fast-forward followed by a `;`
chain. The rule hardens: merge FIRST, boards after the merge exit is
read, nothing between them but the check)
Discovery, login url (nonce), callback validating the id_token into
a Principal; at_hash keeps spliced access tokens out; the forgery
battery refuses each attack by name. Matrix 1049.

## spec-audit-fixes — the audit's seven gaps closed in the specs
Completed: 2026-09-01
Three specs born: tls.md (one transport seam, sslmode vocabulary,
verify-full default), obs.md (spans as values on a trace topic,
W3C traceparent, tracing handlers), blob.md (object-store seam, fs
+ own-SigV4 S3 subset). Six updated: persist.md (the sync-SPI
asymmetry recorded, backup/PITR stated, wire auth via
okay-security), jdbc.md (sketch retyped against Sql, own-DB
migrations à la Flyway), data.md (queues bridged not mirrored —
no Queue seam, two table rows), sql/cache/r TLS links. BACKLOG
slugs turned into implementation entries.

## security-node — the JS leg verifies
Completed: 2026-09-01
node:crypto behind the same seam; the JS suite runs the SAME shared
code (HS256 JWT, passwords, API keys, PKCE pinned to RFC 7636). The
linker forced the right design: platform keys are an opaque
Crypto.Handle; JWKS parses everywhere, verifies where keys exist.
Matrix 1047.

## codec-vector — Schema learns Vector and Char; recursion is a test
Completed: 2026-09-01
SVector + SChar, every algebra swept; recursion proven at depth on a
product and a sum; Schema[Ui]/Event/Patch derive and round-trip both
wires — the hand mapping is a choice now. The sweep's warnings caught
WireJson missing the keyed-diff trio (a real MatchError-in-waiting).
codec-defaults filed with its reason. Matrix 1042.

## py-spec — specs/py.md: Python as a handler
Completed: 2026-09-01
The REval twin (PyEval), with the r.md model adopted by reference
rather than copied. Python-specific: module:name addressing, an
own stdlib-only stdio shim (Py4J and jupyter-kernel rejected),
persistent worker as the served engine (resident imports; N
workers instead of threads-under-GIL), verify against the
configured interpreter (wrong-venv refuses loudly). The
JVM-python question answered once: Jython dead, JEP/ScalaPy
shared-fate, GraalPy a watched future engine behind the unchanged
seam. Spec only; py-subprocess, py-worker, py-arrow filed.
## nio-close-race (partial) — narrowed to a serve-fiber stall, not fixed
Completed: 2026-09-01 (investigation landed; fix still open in BACKLOG)

Three harness generations: racy counters, per-round futures, and a
leak-free sequential trace. Established: the serve fiber STALLS (a
write completion that never fires — not an exception, onComplete never
runs) after 0–4 writes, at ~1.3/1000 rounds; the client usually sees
premature EOF, sometimes a pure hang. The okay Async driver was read
and cleared — the Await cell CAS protocol is sound. Prime suspect is
the default AsynchronousChannelGroup under rapid channel churn.
Also landed per user request: law comments moved above each law in all
theory code blocks (phone readability), and chapter 7 now names the
origami tradition — Gibbons 2003, catamorphisms/anamorphisms, and the
Chunks pipeline as a hylomorphism with the optimizer as fusion laws.

## ui-keyed-diff — a moved child is a move, not a Replace
Completed: 2026-09-01
Keyed matching when every child has a distinct key: one Reorder for
a shuffle, narrow patches ride along, Remove/Insert for churn;
positional fallback otherwise. The law extended: 200 seeded rounds
plus a quality assertion (shuffles never Replace). Matrix 1027.

## r-spec — specs/r.md: R as a handler
Completed: 2026-09-01
R joins the landscape as call-shaped foreign compute: an R call is
an OPERATION (journalable by Durable, mockable by handler swap,
supervised like a cluster worker), never an embedding (JRI/Renjin/
FastR rejected with reasons). Named functions only — no string
eval, structurally; neutral RValue/RFrame with Schema at the edge
(the SqlValue move); verify(packages) catches environment drift
loudly; subprocess engine first, Rserve behind the same handler.
Spec only; r-subprocess, r-rserve, r-arrow filed.
## quiet-measurements — the two waiting questions, answered on a quiet machine
Completed: 2026-09-01

symbol-fold-cost is CLOSED: indexFoldNoRefs 189.6 ±5.4 against
indexFoldOnly 235.0 ±14.9 — the identifier branch is 19% of the walk
and 81% is the traversal machinery, which prices future optimization
honestly and agrees with the refuted mutable-bucket rewrite. The
cluster flush question closed directionally (blockingBytesFlushed 50.8
±9.5, slower than shipped 38.2 ±0.5) and yielded a NEW correctness
lead: the NIO lane sporadically fails its sum assertion — possible
data loss around close — filed as nio-close-race. Chapter 1's monad
laws expanded per user request: each law spelled, read operationally,
and tied to why generic code depends on it.

## mcp-templates — one declaration, unbounded uris; MCP's list closes
Completed: 2026-09-01
resources/templates/list both ends; expand (RFC 6570 L1) and its
never-guessing reverse `matches` — a server's read extracts the
variables, one line serves a tree; completion tied in; the reference
server's own templates probed live. The 2025-06-18 protocol list is
COMPLETE. Matrix 1022.

## mcp-completion — the completer is a function the Serving carries
Completed: 2026-09-01
completion/complete both ends: Complete => Vector[String] as an
Option (capability follows the function), 100-cap with hasMore/total,
context narrowing, resource uris passed through; the live probe got
an answer from the reference server. 4 tests + probe; matrix 1016.
## theory-textbook — where okay comes from, with the papers
Completed: 2026-09-01

docs/theory/: seven chapters and the map, ~800 lines, okay as the
single running example. Moggi and Wadler through Free's normalizing
fold; Felleisen, Danvy–Filinski and Filinski's representation theorem
as the sentence justifying Cont at the bottom of the tower; Atkey with
both instances; Swierstra and Kiselyov–Ishii with the left-nested-bind
literature; Plotkin–Power/Pretnar and the three handler shapes on one
line; Carette–Kiselyov–Shan and Taha–Sheard as the two answers to
interpretive overhead, both present and each placed where its theory
says; LogicT, codata and the sketch papers to close. 30 works cited;
every okay claim grep-verified at file:line during writing.

## ws-close-halfduplex — the last unchecked item in specs/http.md
Completed: 2026-09-01

The strong form of the half-duplex claim: `WsEcho(partingWords = 3)`
answers a Close with three more texts before echoing it, and the
session sees all three, in order, then the Close. specs/http.md now
has no unchecked behaviour item.

## security-mcp — MCP authorization: the challenge that teaches
Completed: 2026-09-01
McpAuth: RFC 9728 metadata (public — it is how strangers learn), the
protected route (401 with resource_metadata), discovery with named
Lefts, connect onto a bearer-carrying link. The loop test: the same
agent call works protected and open. MCP's last parked item closed.
4 tests; matrix 1011.

## sql-seam-spec — specs/sql.md: SQL without a mandatory JDBC
Completed: 2026-09-01
The typed relational contract (rows/verify/transact, jdbc.md)
re-cut against a driver-agnostic Sql seam (neutral SqlValue/Col,
Async): okay-jdbc becomes its first driver (and stays the honest
default on the JVM), okay-pg (Postgres wire over the Async
transport, cross-platform, unlocks the whole pg family incl.
pgvector) the direct road, R2DBC a stated low-priority hatch.
persist gains the openness commitment: persist-wire (remote Topic
client), format and wire as documented surfaces. Spec only;
sql-seam, sql-pg-wire, sql-r2dbc, persist-wire filed.

## security-core — okay-security stage 0
Completed: 2026-09-01 (landed as 3a36930)
The model as values, JWT (HS256/RS256, kid, skew; alg confusion
defused by the key deciding), JWKS, PBKDF2 with parameters in the
stored form, API keys as digests, the policy algebra, Secure.bearer
holding the door by type, OAuth2 client flows with S256 PKCE checked
by the stub AS. 11 tests, hostile side throughout. Matrix 1007.

## data-spec — specs/data.md: the data landscape, two postures, few seams
Completed: 2026-09-01
NoSQL, OLAP/warehouses, lakes, vector, Kafka, Spark — classified by
access shape, not vendor; a vendor enters only as an implementation
of an existing seam. One new trait for the one uncovered shape
(Docs: CAS conditional writes, declared consistency; multi-doc
transactions refused in favor of journaled sagas). Foreign posture
(no DDL, their constraints as the idempotency far end) and own
posture (the log + materializations, refold as universal rebuild)
defined once, applied per class. Five implementation slugs filed.

## mcp-resumable — Last-Event-ID is read(from)
Completed: 2026-09-01
Pushes journaled per session key before fan-out; SSE frames carry
id: offsets; a dropped stream reopens with the token and replays
exactly what it missed, then goes live; fresh GETs start at the live
end; v6 without a journal is untouched. 4 tests over real Jetty;
matrix 996.

## cache-spec — specs/cache.md: caching with named invalidation
Completed: 2026-09-01
Every cache names where its truth lives: a log-fed view (never
invalid, only behind — lag IS consumer lag), write-through with an
invalidation topic (the stale window stated, not denied), or a
declared staleness budget — no default TTL anywhere. Single-flight
in getOrLoad, bounded always, Redis via a minimal own RESP behind
the same trait; distributed locks refused out loud. Spec only;
cache-memory, cache-view, cache-redis filed.

## jdbc-typed-spec — specs/jdbc.md: the unmodifiable foreign database
Completed: 2026-09-01
The posture: their schema is authoritative — bind, don't model. SQL
stays; Schema becomes the row and param codec (total decode, damage
names the column), verify catches drift at startup (the fingerprint
lesson at the DB seam), transact is a Resource-shaped region with
declared isolation, and writes bridge to okay-persist through their
own unique constraints (WithKey/Reconcile in SQL). Spec only;
jdbc-typed, jdbc-write-bridge, jdbc-poll-source filed.

## conf-spec — specs/conf.md: config as data, secrets as references
Completed: 2026-09-01
Names the rule already in force (modules take credentials as
constructor values; the edge resolves them) and adds the missing
seam: Secret = a reference (env:/file:), Secrets = the resolver
trait, four invariants keeping passwords out of journals, logs,
URLs and stored configs by construction. Spec only; conf-impl and
conf-topic filed.

## ui-durable — the journal is the line stream, verbatim
Completed: 2026-09-01 (landed as 9068fe7 — an earlier entry said
277ecce, the pre-rebase hash of the same work: a `| tail -1` hid a
refused fast-forward and the release commit went out before the
merge; recovered from the reflog, rebased, landed for real)
Event-sourced sessions on persist-core stage 0: journal inbound
lines (hostile ones included — the stage's determinism is the
argument), segmented refold (a journaled Closed ended a connection,
not the session), snapshots bound the refold (counted). Six
equalities between live runs and recoveries. Matrix 983.


## cluster-nio — measured, the answer was neither guess, and the code stays
Completed: 2026-09-01

Four lanes, then a fifth when the first attribution turned out to be
confounded. The shipped transport is 37.9ms per 100 chunks; bytes with
a single flush 24.4; NIO 24.7; the codec alone 25.9. A byte rewrite
with the flush-per-send streaming requires measured 38.4 — equal to
what shipped — so the 1.55x was the flush policy, not the text
machinery, and the rewrite was REVERTED: equal performance, more code.
What stands: Loom parking is free (NIO vs a parked read is a wash), the
codec IS the transport (CBOR is the lever), and a totality hole found
by a torn frame — the "total" JSON parser threw on "-", "1e" and three
more — is fixed and pinned. `Lines.stage` (bytes→UTF-8 lines) moved to
the core; okay-http delegates to it.

## ui-screens — screens are codata, a wizard is a screen you push
Completed: 2026-09-01
Screen (view + step), Nav stack (Stay/Push/Pop/To; empty = end),
Nav.scenario fusing Dialog into a pushable screen with the answer
through the parent's continuation. 5 tests; matrix 977 (with
persist-core landed beside).

## persist-core — okay-persist: the durable log, stage 0
Completed: 2026-09-01
Spec-first (specs/persist.md — the partitioned log designed to its
distributed extent: replication with epochs, delivery semantics,
evolution, ops as values; built in stages). Stage 0: Record/Ack/
Policy/Topic/Store, FNV-1a routing, MemoryStore cross-platform,
FileStore on the JVM (versioned segment headers, CRC32C frames,
torn-tail truncation on recovery, retention by whole segments);
poll-on-end is a tested claim (the ui/mcp tailing contract). 23 JVM
+ 8 JS + 8 Native tests; full matrix green (one unrelated TestMcpHttp
flake, filed). Commits 5ffce3c..ca24db3.

## ui-wire — server-driven UI; the tree is the capability list
Completed: 2026-09-01
WireJson (hand-mapped; codec-vector filed), Wire.serve as a pure
stage (full tree, then narrow patches), Wire.client to any Host; the
forged-key test argues from the hostile side (its update THROWS on
the forged key — the wire never lets it through). 6 tests; matrix 933.

## codec-native — the P5 chain on Scala Native
Completed: 2026-09-01
okay-lex/parse/codec gain Native legs (an omission from P5, never a
decision); 57 tests pass as native binaries first try; okay-ui's Form
rides to Native. Full matrix: 927 tests.

## ui-scenarios — Dialog: a wizard is a program
Completed: 2026-09-01
Show answers an Event (a GADT); scenarios run standalone over any
Host or AS a screen inside the loop (the continuation is the state);
Form.ask/askSchema with retry-by-recursion; the demo's elicitation
loop collapsed to one line. 4 tests. Landed with spec check-off.

## ui — okay-ui v1: the toolkit that is not a toolkit
Completed: 2026-09-01
Spec-first (specs/ui.md, incl. the architecture above v1). The view
as a value (keys, not closures), diff+patch with the agreement law,
the loop over merged sources, terminal host (pure frames + stty),
React-shaped host (pure Ui=>Elem, five-line glue), Form as the fifth
Schema algebra (typed + dynamic), and MCP elicitation closed end to
end. 20 new tests. Landed: e5e19db.

## mcp — the Model Context Protocol, complete
Completed: 2026-09-01
Six tasks, spec-first (specs/mcp.md): tools/resources/prompts both
ends; duplex (subscriptions, roots, sampling as the Model effect);
transports stdio + streamable HTTP with server push over the GET
stream; acceptance against the reference server (passed first try).
Landed: 998bbc5, 955a99e, 46723fe, dd4599f, 080894e, 4a86daf.

## docs-sweep — what drifted, and what was never written down
Completed: 2026-09-01
README/ROADMAP/typepedia/tutorial corrections (Writer encoding,
groupId, counts), the upper-layers section, the MCP chapter, the
fourth kind of test, AGENTS.md. Landed: 7285974.

## stream-exercise + primitives
Completed: 2026-08-31/09-01
The fs2 exercise in okay-demo; Writer.of/map, Source + merge (bounded
by default, measured), Stage.transduce/mapAccumulate; inference fix
(one parameter list). Landed: a1f62b8..d059a9d.
