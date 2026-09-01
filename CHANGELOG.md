# Changelog

## sim-typestate-specs — the user's question becomes two specs
Completed: 2026-09-01 (landed as 3bb0bd3; spec only)
Born of "where are PState and Delim useful?" and hashed out in the
room while OTHER lanes landed three consumers the same afternoon.
specs/sim.md: deterministic concurrency simulation on Delim —
every fiber under its own Prompt, a seeded scheduler capturing at
the Async waist (Run/Await, the narrowest point everything already
passes), virtual clock, fault plans from the seed, interleavings
replayable byte for byte; the argument is the day's own ledger
(three real races, all found by flakes). specs/typestate.md: the
criteria doc — typestate pays for ONE-WAY phases through ABSTRACT
boundaries, and the cheapest adequate mechanism wins (phantom
types for two states = the landed sql-typestate; phase objects for
short handshakes = pg-scram-typestate, the room's counter-proposal
accepted; PState proper for type-changing accumulation = the
landed stage-phased). sim-harness and wire-typestate filed against
the specs; scram may be claimed by any lane.

## agent-stepper — pause, inspect, resume, fork: Delim's second consumer
Completed: 2026-09-01

Stepper.stepped translates every Tool.Call into a pause (a shift to a
typed prompt; the captured continuation IS the rest of the run, as a
value), drive loops the operator's decision, transparent proves the
observer away (stepping with nobody watching equals not stepping —
tested against the direct run). The Delim-specific dividend is
multi-shot: one pause resumed twice yields two futures from one past
("what if the tool had said X"), tested. With dialog-delim and
ui-pwizard landed the same day, PState and Delim both now hold
production consumers. 4 tests.

## persist-election — the operator removed from the loop
Completed: 2026-09-01 (landed as 096fc9c)
specs/consensus.md implemented: Election consumes total order and
a clock, nothing else — the fold is first-Take-wins per epoch,
Operator overrides even landing second, a deposed leader's lease
is noise; tryTakeover answers from the FOLD (the claim lands, the
node reads back whether it was first) and the winner leases
immediately so a racing claimant sees no vacancy. All six spec
boxes checked: 5 suite tests x THREE control-log engines (memory,
the FileStore arbiter, live Kafka — unchanged, which was the
claim) + 3 integration tests driving stage 2's promote (loss-free
takeover, epoch fencing, arbiter-down degrading only failover).
One truth taught back: a winner that never leases loses the seat.
The spec also gained the user's PState/Delim notes (typestate for
RaftStore roles; deterministic simulation for testing consensus).
En route, fixed forward for the ui lane: runCmd's close raced the
loop's launch and LOST command answers (flaky TestCmd) — a third
counter (handed-over-but-unfolded events) ends it, 5x green.
Merge read alone after one refused ff (dialog-delim divergence;
targeted retest): exit 0. Full matrix green on a quiet machine
(two environmental signal-9 kills on a load-42 machine before it,
both green alone — the multi-agent matrix stampede is real).

## dialog-delim — cancellable scopes: Delim gets its consumer
Completed: 2026-09-01

Scope: scenarios in the Delim + Dialog row, typed prompts as
cancellable sub-flow boundaries — cancel(p)(value) exits the named
scope from any depth with no Option threading between steps, and the
multi-prompt point is proven: an inner scope aborts ACROSS its own
boundary to the outer prompt (the capability nested handlers cannot
express; textbook ch. 2). One run erases the row; Dialog untouched,
plain scenarios run beside scoped ones by test. Delim's first
production consumer, PWizard being PState's second — both theory
exhibits now earn their keep. 3 tests.

## stage-phased — typestate on the stream; PState gains its consumer
Completed: 2026-09-01 (landed as 3aba599)
Stage.phased: the accumulator CHANGES TYPE at the switch (header ->
rows, the CSV shape) — the body cannot mention the head's phase by
TYPE, the suite gains its first does-not-COMPILE proof
(compileErrors), and the per-input transition is EXECUTED through
PState: the theory exhibit of docs/theory/03 doing streaming work.
Ends honest both ways (the answer names the phase the stream died
in). Core suites 208/14/14 green on all three platforms; the full
matrix carries the PRE-EXISTING ui-cmd flake, probe-proven on
pristine master and filed as ui-cmd-flaky. Second Atkey consumer
(sql-typestate) is the sibling's parallel lane; pg-scram-typestate
filed at the operator's ask.

## ui-pwizard — the typed wizard: PState's style as a Dialog alternative
Completed: 2026-09-01

PWizard, additive only: steps are Cont values whose answer type
threads a suspend/resume machine and whose state type GROWS — a step
names its state requirement, the compiler enforces the order (age
before name does not compile; compileErrors proves it), views read
the typed state-so-far, and `step` carries a built-in validation
retry. toDialog bridges any machine into an ordinary Dialog program,
so the typed wizard runs over any Host or as a Screen with Dialog
untouched. PState's second consumer after sql-typestate. 3 tests.

## sql-typestate — the transaction protocol in the types; PState gets its consumer
Completed: 2026-09-01

Typed.Db[S] carries the transaction state as a phantom; Typed.region
demands Db[Tx.No], hands the body Db[Tx.Yes], and owns begin/commit
itself — the nested-begin failure specs/jdbc.md documents as a
runtime refusal is now unrepresentable, proven by compileErrors (the
error names Tx.Yes). Runtime is exactly transact. This is PState's
typestate (Atkey, textbook ch. 3) in two-state form — the chapter now
points at the shipped consumer; the full answer-type embedding was
declined with its price stated. 14 tests on H2.

## cache-redis — four commands do not justify a dependency
Completed: 2026-09-01 (landed after c316c30)
The RESP client is four commands over a blocking socket; Budget =
SET PX (the SERVER expires, this process never filters), values
ride CBOR, connect PINGs and fails fast. Invalidations are EVENTS
on a persist topic: the cross-node honest window shown before the
drain, and a down node replays and CONVERGES — the trade justifying
the topic over pub/sub, asserted. Live vs docker redis, skip where
absent. Matrix ~1440.

## persist-consensus (spec) — who may advance an epoch, decided
Completed: 2026-09-01 (landed as fbf2e2e; spec only)
specs/consensus.md: election REDUCED to a fold of a totally-ordered
control log — the first Take at an epoch wins on every node's fold,
no votes or wire protocol of our own; leases (plus a declared skew
allowance) decide LIVENESS only, epochs keep deciding safety, and
the operator record outranks automation on every fold. The log is
sourced from engines this stack already has: KafkaStore first (its
KRaft did the twenty years), a FileStore arbiter for dev (honest
trade: failover availability, never correctness), own RaftStore
later as one more engine under unchanged machinery. Rejected with
reasons: Raft-first, per-partition election groups, ZK/etcd client
deps, clock-trusting correctness. persist-election filed with its
six-box battery. The persist staging now has every stage designed
and stages 0-3 shipped.

## docs-seam — the one new seam, proven on both postures
Completed: 2026-09-01 (landed as df9a119)
okay-docs cross-built: Docs[A] with Cond as the load-bearing part
(Always/IfAbsent/IfVersion — CAS as data, Stale carries what holds
NOW), declared-index queries (a scan wearing a query's hat refuses
by name), grants as the engine's honest consistency mapping, and
NO multi-document transactions deliberately (a multi-item change is
a journaled sequence of CAS — the saga with existing machinery).
TopicDocs = the own posture made code: a compacted-topic fold where
the version IS the record offset, deletes are tombstones, a cold
node refolds the same store. okay-docs-mongo (satellite pays the
driver, the argon2 precedent): every conditional write is ONE
server-side operation, declared indexes become real ones. ONE
DocsSuite over both engines (8+8+8 cross-platform + 7 live Mongo),
including the lost-ack CAS retry landing once. En route: the wire
server now binds LOOPBACK by default (plaintext until wire-tls
does not volunteer itself to the network), and the port-roulette
flake family got one BACKLOG ledger (TestWire read literal "HTTP"
at its handshake once under parallel suites). Merge read alone
after one refused ff (jdbc-bulk-load divergence; targeted retest):
exit 0. Full matrix green (Postgres, Kafka AND Mongo live).

## jdbc-bulk-load — WithKey at batch granularity
Completed: 2026-09-01 (landed as 6f7e8d8)
The OLAP write posture held by discipline: history row + the
caller's COPY in ONE transaction — the unique key IS the dedup, a
crash-retry lands once, a refused claim is VERIFIED against the key
(a dead wire must not impersonate a dedup), a failing COPY rolls
its claim back. The olap wrapper refuses row DML by name and points
at the right door. DuckDB as the double. Matrix 1433.

## obs-otlp — export is a consumer, and no SDK came
Completed: 2026-09-01 (landed as bc14801)
The pure half maps spans to OTLP/HTTP JSON (nanos as strings,
status 2 carries the message, roots omit parentSpanId); the jvm
glue is one more topic consumer — offset = resume token, a refusing
collector leaves the batch unconsumed so retry re-ships
(at-least-once, as ingestion expects). Proven against a recording
fake collector. Matrix 1430.

## cache-write-through — the window is stated, not denied
Completed: 2026-09-01 (landed as 61c7501)
Regime 2's write path held by construction (WriteThrough.write:
commit THEN invalidate — one helper, not an audit of call sites).
Argued three ways over H2 through the Sql seam: ordering asserted
on a probing cache; the WRONG ordering's resurrection bug
demonstrated (a reader between invalidate and commit re-caches the
old truth indefinitely — why the rule exists); the honest
commit-to-invalidate window shown. The last open cache.md box
closes. Matrix 1410.

## persist-interop — the engines that already did the twenty years
Completed: 2026-09-01 (landed as d101128; spec first)
Stage 3: SqlStore (okay-jdbc, via the Sql seam — any driver serves
it) passes the FULL 13-test persist StoreSuite on H2, the
cross-engine acceptance; begin proved to be state of its own (it
moves only under retention — the contract caught min(off) drifting
under compaction), plus two SQL truths (aggregates over nothing
answer a NULL row; H2 types SUM(expr) as NUMERIC). KafkaStore
(okay-kafka) inherits partitions/replication/election behind the
same trait: the sync SPI blocks honestly, compact() refuses by
name (the engine keeps its own ops), Received maps to
fire-and-forget with the log's end as the honest answer, and the
persist Typed view decodes unchanged over a real broker — four
live tests on dockerized Kafka 3.9 (skip when absent).
persist-offload refiled to pair blob-seam. Merge read alone:
exit 0. Full matrix green (live Postgres AND Kafka in the run).

## py-worker — N processes is the parallelism; the GIL is then irrelevant
Completed: 2026-09-01 (landed as b8c76ab)
PyWorkers: N resident processes behind the same handler shape a
single worker has — programs cannot tell. Dispatch proven by pid
distinctness (determinism over stopwatch); module state lives WITH
its worker (seed-and-draw); the supervisor replaces a corpse COLD
before rethrowing, the retry lands live. One program, both engines,
unchanged. okay-py stages 0+1 shipped. Matrix 1406.
## ui-cmd — the effect slot: commands are data, the loop runs them
Completed: 2026-09-01

"Press the button, fetch, fold the result back in" now has a direct
spelling: Ui.runCmd's update answers (state, commands), each command
an `Event ! Async` the LOOP spawns, its answer re-entering the same
fold; Nav.Run(prog, s) is the stack's version — go there AND launch.
Ui.run is the commandless special case. The first cut merged a
never-closing answers channel and broke v1's quiet ending (host ends,
loop ends) — caught by the old tests hanging, redesigned to one
channel with an honest close: upstream done AND nothing in flight.
A command encodes its own failure as an event or forfeits (stated);
a command may answer Closed — an app can end itself. 3 new tests,
52 in okay-ui, JS and Native compile.

## py-subprocess — the other half of the world's numerics, as a handler
Completed: 2026-09-01 (landed as ff683b6)
okay-py stage 0: PyEval operations (named functions only — no
eval-a-string case, structurally), conditions as Either with the
worker surviving them, the stdlib-only shim as a versioned resource
behind a loud handshake, a CLEAN child env, verify turning the
wrong venv into a startup refusal. The json wire tags what it would
merge (NaN, bytes, integral floats). Live vs python3, skip where
absent. First implementation of the r.md shape. Matrix 1395.

## persist-wire — the log reaches past the process; auth rides along
Completed: 2026-09-01 (landed as 73ff276; spec's wire section first)
Covers persist-wire AND persist-wire-auth. The documented surface
made real: [len][CBOR] frames with Wire.Req/Resp as the one source
for both ends (the cluster precedent), Hello/Granted where the
capability list IS the offer (the ui rule retold for logs), auth
as a function (token => Option[Set[topic]]) that okay-security
plugs into with no crypto dependency here, refusals by name with
the connection surviving them, TooEarly crossing unchanged, the
tail shape working remotely, a forged future-version Hello refused
in the handshake. The client speaks Async (a blocking socket
behind Async.Run, the okay-pg pattern); the Node leg arrives with
a consumer. Plaintext v1, stated — TLS rides wire-tls.
Replication's calls join the message enum under the handshake
version when replicas go remote. 7 loopback tests. Merge read
alone: exit 0. Full matrix green.
## ui-toolkit — Form v2 is total over the algebra; the composed dialogs arrive
Completed: 2026-09-01

Per the user's call, derivation and cross-field validation together,
not staged. Form.of[A] now renders every Schema node: a nested
product is a titled section with dotted-path keys (addr.city), a sum
is a Select of its cases plus the chosen case's subform (choosing
swaps it; the value keeps the codec's {"Case": {...}} shape), lists
edit in place with add/remove routed by index. Errors are data —
Form.errors gives (path, message) pairs and each renders under its
field — and cross-field checks read the DECODED value, holding
submit until both layers are clean. Toolkit.confirm/alert/prompt/
choice close the hand-rolling. The drift law extended and tested:
a nesting+sum+list form's submission round-trips the codec decoder.
7 new tests (49 total in okay-ui); JS and Native compile.

## wire-tls — verify-full or it is a named decision
Completed: 2026-09-01 (landed as 8e795cf)
okay-tls: the one transport seam, sslmode vocabulary stack-wide,
SSLSocket over the blocking sockets our wires actually use (the
SSLEngine machine waits for an NIO consumer — recorded in
Decisions). The whole ladder proven against live handshakes:
verify-full refuses wrong hostname/unknown CA by name, verify-ca
accepts the wrong hostname and the test SAYS SO, require tunnels
and refuses plaintext, disable is the named decision. Keys are
Secret refs; inline PEM refuses at the seam. pg/persist-wire
integration boxes stay with their lanes. Matrix 1380.

## persist-backup — backup is boring, and the doctor certifies it first
Completed: 2026-09-01 (landed as 1624f26)
Doctor (okay-persist): an INDEPENDENT reader of the documented
segment format — a second implementation double-checks the writer.
Torn tail on the LAST segment: normal, named, restorable; damage in
a CLOSED one condemns the copy; refusals never mistaken for tails.
Backup (okay.blob — persist->blob would cycle through http):
incremental closed-segment copies to any Blob engine; restore =
place files back for the ordinary startup path. End to end: copy,
wipe, restore, doctor certifies, recovery serves. Matrix ~1360.

## persist-replication — stage 2's core, transport-agnostic
Completed: 2026-09-01 (landed as a9e4bb5; spec first)
Replicated: a coordinator over N replica Stores behind the SAME
Topic trait (stage-0 consumers never rebind). The follower
push/pull IS the read path (replication is a consumer that writes
what it reads; divergence throws by name). The high-water mark =
the quorum-th largest replica end — reads and end() stop there, so
nothing a failover could unwrite is observable; Ack.Replicated
short of quorum throws NoQuorum rather than acking a promise it
cannot keep. The Leader handle carries its epoch: promote catches
the successor up FIRST, then fences the deposed handle, and both
promotion and fencing land on the ops topic (the log audits
itself). produce(producerId, seq, ...) is the idempotent window —
the retry answers the ORIGINAL offset. Six tests on all three
platforms with a Pausable store standing in for the down replica.
En route: TestRepoAgent's budget grows with the repo (120s over
munit's 30 at 419+ sources). persist-wire will carry these same
calls between nodes without changing the machinery. Merge read
alone after one refused ff (match-finish divergence; targeted
retest of persist/demo/match): exit 0. Full matrix green.

## match-finish — the entropy seam, the module page, board hygiene
Completed: 2026-09-01

The sibling's honest flag on 4b7dc0b (util.Random for the link token)
closed properly: `fresh` is a constructor seam on both stores — the
cross default is `Entropy.weak` (unique, linkable everywhere, stated
NOT guess-resistant), and `SqlMatch` defaults to `SecureEntropy.strong`
(SecureRandom is legal in a scala-jvm source; both the profile id and
the link token are credentials). docs/modules/okay-match.md joins the
satellite pages — the wiring table names every seam and its production
filling (Password, Crypto.randomBytes, a rag embedder, an okay-llm
reranker), and the docs index lists the module. The emptied okay-match
BACKLOG section is gone.

## blob-s3 — the lingua franca, spoken ourselves
Completed: 2026-09-01 (landed as 10783c4)
Own SigV4 pinned by the AWS doc vectors (GET/PUT verbatim; the list
example settled by cross-implementation agreement — the diagnostic
recorded in Results). PUT/GET/HEAD/DELETE/ListObjectsV2 path-style
over the one http client; puts buffer while http's Body stays
unstreamed (stated), gets stream. The SAME BlobContract passes green
against LIVE MinIO (docker), and a recording transport proves the
secret reaches the HMAC chain and nothing else. specs/blob.md fully
shipped, both stages. Matrix 1353.

## rag-pgvector — the vector store behind the same interface
Completed: 2026-09-01 (landed as 4b7dc0b)
PgVector in okay-rag's JVM leg: VectorStore[Async] over the Sql
seam via the okay-pg WIRE (the consumer that road was cut for) —
own posture (ensure() creates extension+table), upsert ON CONFLICT
on the segment identity (re-index replaces), search pushed to the
engine with declared Metric whose scores return on the Vectors
scale. THE assertion: order AND scores agree with the reference
MemoryStore within 1e-4 on the hashing fixture; the segment
round-trips whole. Exact scan v1 — an approximate index is a later
measured choice BECAUSE agreement is only testable while exact.
Fixed forward en route: the repo outgrew RepoAgent's 400-file
limit (now 1200); okay-match's second UUID.randomUUID site
(requestLink) broke the JS linker again — freshId, with a note
that a guess-resistant token wants okay-security's seam. Merge
read alone: exit 0. Matrix green.

## match-identity-x — cross-channel identity, without building the hijack
Completed: 2026-09-01

The registry marks attributes identifying (a phone is, a skill is
not); only those generate link candidates, and a candidate answer is
an attribute name plus a masked email — never the value, never the
other profile's facts, never a link. The link itself is proven by the
token: minted for the old profile, delivered through the OLD channel
(the site's job), typed in the new chat — single-use, expiring, right
holder only; the stage-2 recovery secret is the fallback for a dead
channel. A confirmed link is an equivalence, not a merge: both
profiles stay, identityOf answers the class, search folds it into one
candidate carrying facts from both, profileOf aggregates — log-first
holds, nothing rewritten. Tools ident_candidates/request/confirm let
the LLM drive the whole dialogue. 18 tests; the class survives a
restart on the durable store. specs/match.md is now fully landed.

## sql-pg-wire — the direct road: Postgres v3 behind the Sql seam
Completed: 2026-09-01 (landed as 2b03cb3)
okay-pg, ~400 lines for the whole road and zero dependencies:
startup + SCRAM-SHA-256 with the halves most clients skip (server
nonce must extend ours; server SIGNATURE verified — mutual auth;
md5/cleartext deliberately not spoken), the extended protocol with
portals AS the chunk mechanism (Execute maxRows + Flush,
PortalSuspended = next chunk — fetch-size with no driver in
between), text format v1, errors drained to ReadyForQuery before
the throw so the session survives, describe consulting
pg_attribute so verify keeps full strictness. Live suite on the
dockerized Postgres 17.11 (skips where absent): 8 tests including
the TWO-DRIVER ACCEPTANCE — one typed program over PgSql and
JdbcSql/H2, one equal answer, only the SQL strings differ ($n vs
?). The pg family (Cockroach, Timescale, Materialize, Neon,
pgvector) is now a connect call away. Merge read alone: exit 0.
Full matrix green.

## blob-fs — the seam three specs assumed; stage 0, the fs engine
Completed: 2026-09-01 (landed as 2338af1)
Trait Blob cross-built; the Fs engine holds the floor: strict root
containment, atomic tmp-then-move puts, crash leftovers invisible,
engine-defined etags. get answers Either — the chunks are the body,
the answer is the outcome, an absent key is a value naming itself
(sketch adjusted, recorded in Decisions). BlobContract written once;
blob-s3 re-runs it against MinIO. Matrix 1317.
## match-stage2 — the rerank, the gate engine, decay, and the recovery seam
Completed: 2026-09-01

Rerank is an effect (the rag/Embed precedent): `top` runs Find, then
the reranker over the top slice; tests use the lexical handler, the
production one is five lines over okay-llm at the site. PlatformPolicy
replaces the predicate — Allow / AfterMatch / Withhold per attribute —
and Ranked.withheld NAMES the AfterMatch facts that matched: the
seeker learns that the phone exists, not what it is. Volatile
attributes decay the rank on an exp2 half-life. Email recovery is a
hashed-secret rebind behind a hash/verify seam (okay-security plugs
in; no dependency): with the secret the new email finds the old
profile, without it a stranger gets a fresh one — never a hijack.
Memory and Sql handlers carry all four; 14 tests green. Cross-channel
identity stays open as match-identity-x.

## sql-sqlite — the embedded engine proves the seam (user ask)
Completed: 2026-09-01 (landed as 5b17922)
The whole typed battery over xerial sqlite-jdbc (test-scope)
against a FILE database: metadata honest enough for a clean verify,
both isolation levels granted, the Writes bridge in its
spec-preferred ON CONFLICT DO NOTHING spelling, and READ-ONLY open
mode standing in for the no-DDL posture (an embedded db has no
users — "their database" is a file you were handed). En route,
found and fixed for everyone: okay-match + okay-jdbc both carrying
H2 in one sbt JVM raced DriverManager's per-classloader driver
registration ("No suitable driver" for whoever ran second) — both
suites now fork, the core-fork precedent. Merge read alone after
one refused ff (claim-only divergence): exit 0. Matrix green.

## own-db-migrations — the settled discipline, adopted not reinvented
Completed: 2026-09-01 (landed as e5eff69)
Migrate against the Sql trait: versioned authored scripts, sha-256
checksums, the version table in the SAME database, script + row in
one transaction as far as the engine's DDL allows. The fingerprint
rule again: changed or vanished applied scripts refuse naming the
version; duplicates/disorder refuse before touching the db; a failed
script leaves no row and the fix applies next run. record = the ops
hook. RODE ALONG: okay-match hotfix (UUID.randomUUID's SecureRandom
broke the JS linker — every matrix run was red; util.Random hex ids
now) and the obs Never test proven by a counting clock instead of a
wall clock. Matrix 1301.
## match-stage1 — okay-match is durable: the Sql seam, the log, the migration
Completed: 2026-09-01

SqlMatch: the same three handlers over ANY `Sql` driver (H2 in the
tests; sqlite or Postgres is the connection string — the seam is the
point), values flattened into typed columns, restart-proof (a second
handler over the same database continues where the first stopped,
ids included). ChatLog: chat turns on a persist topic keyed by
profile, offsets as provenance, and `replay` — the log-first test
rebuilds a FRESH store from the topic to the live store's exact
state, and replaying over the live store changes nothing (the
idempotence key doing its job). mergeAttr: the registry migration —
the drifted attribute's facts move to the winner, the loser answers
no more. 10 tests green (6 stage-0 + 4 stage-1).

## match-stage0 — okay-match exists: the model, the effects, the reference store
Completed: 2026-09-01

New module okay-match (package okay.matching — `match` is a keyword).
Model.scala: profiles (email + owner-secret UUID), append-only facts
with chat-span provenance and supersede-with-reason, two-gate
visibility, the small value core, typed predicates. Ops.scala: the
Registry / Facts / Find effects — the whole backend contract, open by
construction. Memory.scala: the reference handler — hashing
embeddings, search-before-create on propose (slug/synonym exact OR
description cosine), replay-idempotent asserts keyed by (profile,
attr, provenance), hybrid candidates (hard predicate filter, then
similarity over per-side profile summaries), both gates honored at
disclosure. Tools.scala: the operations as LLM tools 1:1 — the
(specs, table) pair mcp.Server.serve takes. Six tests, one per
spec behavior checkbox, including the scripted two-side scenario:
provider chat in, seeker chat out, matched end to end.

## obs-tracing — the missing third of the doctrine, without a framework
Completed: 2026-09-01 (landed as 2ad52a4)
okay-obs cross-built: spans as values on a trace topic, W3C
traceparent parsed totally (damage = a NAMED fresh root), tracestate
opaque, the traced(Handler) combinator wrapping any handler blind.
The crossing test follows one traceId from an inbound header through
okay-http into H2 through the Sql seam. Never is a short-circuit by
construction. obs-otlp and obs-durable-overlay filed; the journal
join box stays open until a Durable consumer. Matrix 1282.

## match-spec — okay-match designed: structure the unstructured, then find it
Completed: 2026-09-01

specs/match.md, designed in conversation with the user. Log-first
(chats in persist topics are the only truth; facts, profiles and
indexes are rebuildable projections), an attribute registry with a
search-before-create contract as the mechanism that bounds LLM
vocabulary drift, append-only facts with provenance to a chat span and
supersede-with-reason (freshest wins, but ask first), two-gate
visibility (owner intent AND platform policy) from day 0, identity as
email + owner-secret profile UUID with the recovery/hijack question
honestly deferred to stage 2 alongside okay-security. Effects first,
handlers second: memory + rag embeddings at stage 0, sqlite and
Postgres+pgvector through the Sql seam at stage 1.

## conf-topic — the config becomes one more consumer of the one primitive
Completed: 2026-09-01 (landed as 87f53b4)
Configs in okay-persist (beside Snapshots/Offsets): put/latest/at/
history over a compacted keyed topic, values as the Schema's JSON.
The audit IS the log, rollback IS a read, and history is honest
about compaction (asserted). okay-conf keeps codec-only deps;
reference-only safety is invariant 3 by construction. specs/conf.md
fully shipped, both stages. Matrix 1254.

## codec-iso — to every algebra the wrapper does not exist
Completed: 2026-09-01 (landed as 46c6bcd)
SIso in the Schema enum (wrap/refine): a newtype travels as what it
wraps, a refining Left is a decode error like any other. All six
algebras swept — Json, Cbor, tool schema, form, and okay-sql's row
bridge (Field gained into/outof; a wrapped column is its underlying
kind both directions). First consumer: Schema[Secret] is the bare
reference string. Composes with codec-defaults. Matrix 1242.

## cache-view — the consumer that is never invalid, only behind
Completed: 2026-09-01 (landed as 6ab7e63)
Regime 1 shipped: View(topic)(key)(fold) in okay-cache (now on
okay-persist) — latest serves the consumed fold, lag IS consumer
lag, refresh is the whole of invalidation, a fold answering None is
the tombstone. Cold refold agrees with the warm view before AND
after compaction — the snapshot story, told as a cache. All three
platforms. Merge read alone: exit 0. Matrix green.

## cache-memory — named invalidation, no default TTL, stage 0
Completed: 2026-09-01 (landed as f6219f9)
okay-cache cross-built (core-only dep): Regime (Budget/Invalidated,
no default, no unbounded constructor), bounded LRU memory engine
(expiry on read, re-insertion recency), single-flight getOrLoad
whose loader runs under its OWN drive — a failure anywhere in it
reaches every waiter instead of stranding them, and the key
recovers. Negative caching is a type (V = Option[A]), stats a plain
value. 9 JVM / 7 JS / 7 Native (the shared suite drives Run-only
programs inline — no CanBlock, so it runs on JS). Filed
cache-write-through for the orphaned write-through box. En route:
one full-matrix environmental kill (okay-conf Native, signal 9,
green twice alone — OOM under parallel Native runners). Merge read
alone: exit 0. Matrix green on rerun.

## agent-langchain4j — their ChatModel behind our Model effect
Completed: 2026-09-01 (landed as b8b4d75)
okay-langchain4j (jvm, langchain4j-core only): message/declaration/
reply as pure mappings — the fourth algebra's schema walks into
their JsonSchemaElement tree with required intact, so a defaulted
field stays omittable across the interop; the handler is comonadic
like Provider.openAi (Loom parks in their client); count stays
local. Proven against a scripted ChatModel recording what it saw.
The P9 interop sentence's Model half; rag-langchain4j filed for the
EmbeddingStore half. Matrix 1197.

## lake-read-duckdb — the lake read road, proven with zero new code
Completed: 2026-09-01 (landed as be03533)
The point of the seam, demonstrated: a Parquet file is queried
through the SAME typed layer as every relational source — DuckDB
embedded (test-scope only), read_parquet the table, verify passing,
100k rows streaming at fetch-size chunks, an aggregation pushed to
the engine. Finding recorded in the spec: Parquet marks fields
OPTIONAL by default, so verify demands Option fields — the
fingerprint lesson working, not a nuisance. One full-matrix flake
observed en route (okay-jetty TestResumable, green twice alone) —
noted on the http flake entry. Merge read alone: exit 0. Matrix
green on rerun.

## theory-nav — previous/next at the foot of every chapter
Completed: 2026-09-01

Each theory page (the map and chapters 1-7) ends with
prev · Contents · next navigation, so the book reads front to back
without returning to the index by hand.

## theory-cite-links — in-text citations are footnotes now
Completed: 2026-09-01

Every \[Author Year\] in the theory chapters links to its entry in
that chapter's References (HTML anchors on the entries), where the
paper link from the previous pass awaits — 31 in-text citations wired,
zero unmatched, existing links and code blocks untouched.

## jdbc-poll-source — the watermark poll, honestly not CDC
Completed: 2026-09-01 (landed as 07a57d0; spec first)
Poll(db, offsets, group, source): the watermark IS a persist
consumer offset (commit-as-record, refold-on-restart), one poll =
the decoded prefix up to the first damaged row — damage STOPS the
watermark, so nothing is silently skipped, and the fixed row is
re-served next poll. The late-commit caveat is a TEST, not a
footnote: the miss asserted as behavior, then the lag window (in
the caller's SQL, the DBA's language) holding the watermark back
so the late row arrives. With this the jdbc.md behavior list is
fully checked. Merge read alone: exit 0. Full matrix green.

## jdbc-write-bridge — the Durable policies over their constraints
Completed: 2026-09-01 (landed as b1903cd; spec first)
Writes(db, topic, run) in okay-jdbc, written only against the Sql
seam and a persist Topic (movable to any driver): write() journals
Intent(seq, sql, params, key) durably BEFORE the statement, Done
after; recover() refolds and resolves each open intent by declared
Policy — WithKey re-executes the same statement/key and the far
end's constraint dedups (H2 MERGE, landed once), Reconcile(select)
settles the journal without re-executing (proven with a PLAIN
insert that would have thrown on re-run), Fail/empty-Reconcile
answer Unresolved as data with the world untouched. Both crash
windows tested; seq continues over restart. Schema[SqlValue]
derives for the journal records. Merge read alone: exit 0. Full
matrix green.

## conf-impl — configuration as data, secrets as references
Completed: 2026-09-01 (landed as 443c8a2 — the release rode a pull --rebase over the README push, so the changelog names the post-rebase hash)
okay-conf cross-built (depends on okay-codec only): Secret whose
toString IS the reference, Secrets env/file/memory/chain (the chain's
one error is the specific one), Conf.read/load. The shared suite
proves env: on JVM, Node and Native; file: trims exactly one
trailing newline. No plain: scheme, deliberately. codec-iso filed
for the bare-string Secret form. Matrix 1179.

## sql-seam — the relational seam cut at the driver, first road open
Completed: 2026-09-01 (landed as 90c97bf; spec first 0c560db)
New module okay-sql, cross-built JVM/JS/Native — the no-java.sql
claim IS the JS/Native compile: SqlValue/SqlType/Col/Isolation/
Granted(requested, granted), trait Sql (Async everywhere, plus the
one sync cancel() brake for the region finalizer), the typed layer
written once (rows by label camel→snake with row-position Bad,
verify naming dropped/renamed/retyped/nullability drifts, params
positional-prepared-only, transact generic over the rest of the
row so aborts cross the scope and still roll back). okay-jdbc is
the first driver: 13-test battery on H2 run AS a no-DDL user.
The rollback-on-exception test caught a core finalizer leak —
Resource.run applied k(y) outside its try after a forwarded
effect — fixed in core, pinned in TestResource. Write-bridge and
poll-source stay their own slugs. Merge read alone: exit 0. Full
matrix green.

## history-tsv-tabs — the flagged rows had changed shape, not just tabs
Completed: 2026-09-01 (landed as 5774436)
The six rows the room flagged already had real tabs; the live defect
was 51 five-column rows (theirs included) against the eight-column
header. Normalized mechanically — unit into the note's prefix,
unknown sha/load/ref/ratio honestly empty. NF==8 for every data row.

## codec-defaults — the one macro this library allows itself
Completed: 2026-09-01 (landed as 3ebee10)
Mirrors do not carry defaults; the companion's <init>$default$N do.
The macro reads them into SProduct.defaults as ordinary values;
Json/Cbor fall back in order (declared default, None-if-optional,
refusal); an uncallable default is honestly None. ToolSpec stops
requiring defaulted fields and advertises `default` — the omission
an LLM will make is now one decode survives. Proven JVM+JS+Native
by the shared suite. Matrix 1126.

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
## nio-close-fix — the flake was the OS; Nio rewritten on blocking channels
Completed: 2026-09-01

The chase ended two suspects deep: the dedicated-channel-group
experiment cleared the default group, the blocking rewrite reproducing
the loss cleared the whole JDK async layer, and stage counters plus a
parked-accept-never-woke trace pinned it on macOS itself — under
listener churn the kernel completes a handshake into the backlog,
never delivers it to accept, and closes it with a clean FIN, at
~1.2/1000 rounds on either channel API. One stable listener: 8000/8000
clean. Nio stays rewritten on blocking channels over virtual threads
(simpler, measured equal, no userland dispatch to lose); specs/nio.md
carries the argument, okay-http/BUGS.md the full forensics, TestNio a
stable-listener churn gate — deliberately NOT a listener-churn gate,
which would flake red on ~45% of runs by the OS's hand. Also per user
request: the library is Okay, capitalized, across README and docs
prose (127 mentions; code, packages and module names untouched).

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
