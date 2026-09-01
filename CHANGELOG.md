# Changelog

## pg-sslmode — the pg driver speaks TLS through the one seam
Completed: 2026-09-01
specs/tls.md's pg box: Postgres over TLS, the seam's second consumer
and the one with a protocol preamble. `PgSql.connect` was factored
into `connectOver(conn: NetConn, …)` — the startup + SCRAM half over
any established connection. On the JVM, `PgTls.connect` does pg's
STARTTLS-style SSLRequest dance on the raw socket (Int32(8) + code
80877103, then the server's single 'S'/'N' byte), wraps the socket via
`Tls.client` on 'S', and hands the encrypted NetConn to `connectOver`,
which runs the same SCRAM and never learns it is on TLS. A server that
answers 'N' when encryption was asked for is refused by name. okay-pg's
JVM leg compile-depends on okay-tls (the box's own shape — the dance is
the driver's, the session is the seam's); the JS leg has no okay-tls,
so `PgTls` is scala-jvm only. Verified LIVE against the dockerized
Postgres with ssl reloaded on: sslmode=require completes end to end,
verify-full with the server CA passes chain AND hostname, verify-full
with an unknown CA is refused (TestPgTls, skips where TLS is not
offered). Plaintext connections keep working — ssl=on accepts both, so
the existing pg suite is untouched.

## ctx-reader-elim — Reader elimination: the gate lifted, direct blocks the consumer
Completed: 2026-09-01
Landed as e66ac87. Int ! (Reader % E + Row) rewrites to
E ?=> Int ! Row = direct { ... wire[E] ... } — the environment out
of the row, the elaborator running the Reader half at compile time.
TestCtxReaderElim: equivalence at both spellings; provide nesting
overrides THROUGH the effectful block (nearest-wins survives the
macro); lift/unlift one-liners at the call site (functions, never
Conversions — E10 stands). specs/context-functions.md's
ctx-reader-bridge gate lifted; the capabilities.md recipe records
when to KEEP the Reader row (local-style rescoping, handler-visible
asks). GATE CAVEAT: OOM-kill late, 0 failures, full reference
coverage — the standing precedent.

## condition-direct — resumable exceptions in direct style, the frame door, chapter 09
Completed: 2026-09-01
Landed as 167c02e. signal.? resumes at the mark (a call that may
return — the Common Lisp reading, asserted by TestConditionDirect's
before/after trace); within/frame unwind to their frames from direct
blocks; a for-do loop repairs malformed elements mid-stream (the
operator's story, per-element Resume). The frame door takes the
restart body as a direct block (two lines over within + direct).
docs/direct-style.md gains the section; docs/theory/09-conditions.md
opens with Goldberg-Robson/Steele/Pitman, argues via Plotkin-Pretnar
that handlers ARE resumable exceptions, and names both recorded
roads (typed signal, restart capabilities) through Zhang-Myers'
bidirectional effects. GATE CAVEAT: the full matrix was OOM-killed
late with 0 failures and every reference suite covered — the
direct-macro/direct-loops precedent.

## llm-cut-conditions — the repair road between passing and cutting
Completed: 2026-09-01
Cut gains `screened`, the first module consumer of the condition
system: a violating token signals the Violation while the stream's
continuation is live, and the policy chooses per incident —
Resume(t) emits a replacement in the token's place and the stream
continues; Invoke("drop") makes the token vanish; Invoke("cut", v)
falls back to the old hard cut (pull stops, the guard answers
Left(v)). The menu is ["drop", "cut"]; mechanism in the stream,
policy at Condition.run. Additive: checked/watched untouched, a
clean stream never signals (the policy-never-consulted test).
TestCutRepair (4); okay-llm 18/18 JVM, JS compiles.
## ui-direct — the three roads reach the toolkit, and the demo
Completed: 2026-09-01

Direct wizards: a Dialog scenario is straight-line code under
direct[[A] =>> A ! Dialog] with .reflect marks (the ? spelling
collides with Effects' row-?; named mark, tested). Form.askWith
lifts ask's retry policy into conditions — InvalidSubmit signaled,
forgiving≡ask, patience(n) gives up, a repairing policy Resumes a
forced value, and a valid submit never consults the policy (the
machine runs per submit over a pure program). Dialog.hosted /
Nav.hosted are the ambient-Host doors. And the demo's agentTurn is a
direct block now — remember, seed, converse as three plain lines,
the seeding loop staying a named helper exactly as the macro's
no-marks-under-lambda rule prescribes. 67 ui tests, 15 demo tests.
## tutorial-new-arcs — the day reaches the tutorial
Completed: 2026-09-01
Three chapters join the worked tour: 19 "Needs are types:
capabilities" (the TestShowcase shape — one route, production edge /
provide unit / providing override, missing capability = compile
error), 20 "Monads as plain code: the direct block" (bare statements
as do-notation, multi-shot preserved, the door-outside-block-inside
composition), 21 "Errors you can repair: conditions" (signal keeps
the continuation alive, the repair story: one loop, three policies).
The closer renumbered to 22 and now points at capabilities.md and
direct-style.md; the docs index count updated. Every snippet's shape
runs in the repo's tests (TestShowcase, TestDirectDoors,
TestCondition).

## direct-loops — effectful iteration in direct blocks
Completed: 2026-09-01
Landed as a2ba997. for-do/foreach, for-yield/map (the traverse
shape) and while are rewritten into recursive Cont loops over an
immutable materialized List (multi-shot re-entry sound — 2x2
continuations tested; .iterator built by name so Array receivers
serve), while's cond/body splice inside the recursive def and
re-evaluate per iteration, and Assign with a marked rhs binds then
assigns (surfaced by the first loop test). Mid-loop None stops the
loop (2 of 3 hits observed). Non-whitelisted HOFs keep the v1
refusal. 35 tests across TestDirect+TestDirectAuto. GATE CAVEAT:
two full-matrix runs were OOM-killed near the end (the machine at
5.9G/6G swap) with 0 failures each; their suite UNION covers every
suite of the last complete green run — landed on that evidence, the
same precedent as direct-macro's landing.
## match-conditions — malformed tool values meet the condition system
Completed: 2026-09-01

The v1 silent coercions (a "num" tag with no number became 0.0) are
a named policy now: valueOr signals MalformedValue with the legacy
restart on the menu. The default table invokes legacy — nothing
changes for anyone — while table(store, policy) lets a deployment
REPAIR (Resume at the live signal point with a corrected Value) or
REFUSE (Fail becomes a {"refused": ...} answer the model reads and
retries; no fact stored). A well-formed value never consults the
policy. The condition system's second applied consumer. 28 match
tests.
## demo-conditions — the intake's silent default becomes a decision
Completed: 2026-09-01

The condition system's first applied consumer outside its own tests:
the marketplace intake signals BadEmail instead of silently minting
guest@demo. The guest restart is on the menu; the lenient demo
policy invokes it (yesterday's behavior, now chosen on the record),
a repairing policy resumes AT the signal point with a corrected
address, OKAY_CHAT_STRICT=1 escalates as Unhandled naming the menu —
one intake, three outcomes, and a present email never consults the
policy. 15 demo tests.

## persist-wire-tls — the wire runs encrypted, the first consumer of the TLS seam
Completed: 2026-09-01
specs/tls.md's persist-wire box: persist-wire over TLS passes the same
acceptance suite as plaintext. The move keeps okay-persist dependency-
free by making the wire's transport INJECTABLE rather than TLS-aware:
`Wire.Server` gained a `socket: Option[ServerSocket]` (pass the
SSLServerSocket from `Tls.serverSocket`) and `Wire.Remote.connect`
gained a `wrap: Socket => Socket` (pass the `Tls.client` wrap, whose
contract is exactly "wrap the connected socket before any protocol
byte"). Encryption wraps the TRANSPORT, so the handshake, capability
grant, frames and refusals are byte-for-byte the plaintext behaviour —
the acceptance is what does NOT change. okay-tls joins okay-persist in
TEST scope only; the SSLSocket is built in the test, so the core-only
compile graph (okay + codec) is untouched. TestWireTls (live over an
openssl localhost identity, skips where openssl is absent): the
encrypted grant, an append/read round-trip, refuse-by-name, and — the
proof it is REQUIRED not optional — a plaintext client refused by the
TLS server. The plaintext TestWire/TestWireRepl (12) unchanged and
green. Still open for the pg lane: the sslmode SSLRequest dance.
## demo-polish — the demo teaches itself and fails visibly
Completed: 2026-09-01

The page states its mode and links /market — the marketplace made
visible as lists of DISCLOSED facts only (a Matched phone stays off
the page; the gates hold there too, by test). Example chips fill the
input; "помощь" reaches the phrasebook. Failure is visible: a model
dying on the agent path answers an error frame the page renders
(⚠), and a dropped stream on the plain path is detected client-side.
14 demo tests.

## ctx-e20-pattern — the door outside, the direct block inside
Completed: 2026-09-01
The two arcs of the day meet: TestDirectDoors (core, 3 tests) lands
the E20 pattern as executable documentation — `def told: Env ?=> Int
! (Writer % String) = direct { Writer(s"hello ${wire[Env].user}");
wire[Env].uid }` with provide/providing at the edge. A direct block
is itself a context function (DirectCtx[F] ?=> A), so it nests under
the environment layer by nearest-wins and wire resolves inside;
three layers peeled by three machines (compiler / macro / handlers),
none knowing of the others. Sections added to capabilities.md and
direct-style.md, cross-linked.

## docs-direct-style — the direct-style documentation, user page and theory chapter
Completed: 2026-09-01
Landed as ea0df1b. docs/direct-style.md: the four layers (reflection,
the direct block, auto-coloring, do-statements) with the rationale
for every boundary, worked examples, the choosing-a-layer table and
the nine-entry refuted-alternatives graveyard. docs/theory/08:
Part IV of the textbook — the same story argued from the literature
(Filinski 1994/1999, Kameyama-Hasegawa 2003, Flanagan et al. 1993,
Brachthauser et al. 2020, Lindley-McBride-McLaughlin 2017,
Sivaramakrishnan et al. 2021), every Okay claim with file:line.
Linked from docs/README.md and the theory index (eight chapters now).
## demo-ctx — the demo adopts the capability style
Completed: 2026-09-01

The user asked where the new context-function DI pays; the demo was
the textbook site. MatchStore is ambient now: seven signatures drop
the threaded `store = market` default (the hidden-global-with-
override idiom); main provides the durable store, each test provides
its own — the forgot-to-thread bug class (one test once hit the
global sqlite) is structurally gone. Cut.checked gained the
ambient-prompt door (additive), so the demo's guard reads
Cut.guard { Cut.checked(tokens)(rule) }. 13 demo tests green.

## direct-do-statements — bare statements run: do-notation for direct blocks
Completed: 2026-09-01
Landed as c0facb3. A bare statement whose type is the block's F or a
row operation is bound as an implicit .? with the value dropped (the
_ <- reading) — Writer("a") on its own line tells, None
short-circuits, a bare List statement re-runs the rest per element.
The discard guard narrows to foreign marked types; val keeps a
program un-run (binding is consent to hold the value). The None.type
wrinkle: singletons carry no type arguments, so runnableElem also
consults the base type at the block's monad, every guess verified by
<:<. 27 tests, full matrix green.
## match-docs — the day, documented
Completed: 2026-09-01

docs/modules/okay-match.md rewritten to the module's full present:
the two founding decisions, the model layer by layer, hybrid search
with withheld, the reverse chain, scenarios-as-data with the typed
pen, identity without the hijack, open stores, the complete tool
table. docs/modules/okay-demo.md is new — the chat as the stack's
tour: running it, the env table, who does what, how the model runs
the marketplace, the offline phrases, the tests as the tour. Both
linked from the docs index.

## ctx-capabilities-doc — the whole story, told in one place
Completed: 2026-09-01
docs/capabilities.md: what a context function is (three mechanical
facts, E8/E10), the four-word vocabulary (doors, provide,
providing/and, wire) with the rules learned the hard way, the
zero-framework DI story (type as contract, compile-time resolution,
given-scopes as the object graph, environment-vs-resource), the
theory the compiler runs (Reader monad/applicative as verified
identities, the graded <*> chain, ctxMonad for the generic
combinators, the given-import gotcha), the boundaries as kept
refutations (bare-receiver method syntax, forbidden boxes,
Conversions, linear rebinding, the blocked tuple provide), and the
three-worlds payoff page. Linked from README, docs index, guide §9
and typepedia. Every claim traces to E1-E19 or a running test.

## queue-shape — the two queue bridges, no new seam
Completed: 2026-09-01
Per-message-ack brokers (RabbitMQ/AMQP, SQS, NATS, Pulsar, MQTT) are
DELIVERY machinery — per-message ack, redelivery, no offsets — a shape
the log deliberately is not, so a native Queue seam is rejected
(specs/data.md). Instead `okay.persist.Queues`: two bridges over
`Source`/`Sink` SPIs. INGRESS drains a broker into a topic keyed by the
broker's message id, acking AFTER the append — at-least-once: a lost
ack redelivers and re-appends, never drops, and `Queues.dedup`
collapses the duplicate one hop downstream by id (WithKey's shape).
EGRESS reads a topic from an offset and publishes outward, resumable by
the returned offset (a lost offset re-publishes; a sink that dedups on
the id gives exactly-once OUTCOME, the rest at-least-once said out
loud). The SPIs are the whole coupling to a real broker — an engine
adapter is a named deployment, not a core seam. Proven against an
in-memory fake broker (TestQueues, 4 tests: happy drain, lost-ack
redelivery + dedup, resumable egress, lost-offset replay + id-dedup).
Also checked the now-true kafka-eos box in specs/data.md.

## direct-auto-coloring — v2: no marks, and one mark where marks remain
Completed: 2026-09-01
Landed as 838eb2f. The block is DirectCtx[F] ?=> A; phantom
Conversions gated on the capability (selfColor) and additionally on
the Effect[G] marker (opColor) let F[A]-as-A typecheck ONLY inside
direct blocks and ONLY for registered types; the macro rewrites the
conversion calls with the v1 machinery whole. Same landing unifies
the marks: .? now serves monadic values AND raw operations (markTerm
dispatches by type), .!? refuted as redundant. The discard guard
makes a silently dropped monadic statement a compile error — found
and kept as tests: statements never see conversions (no expected
type) and Unit ascription is value discard, so tell-like ops keep
the explicit .?; auto-coloring resolves at DECLARED types (smart
constructors color, raw case constructors do not). 24 tests
(TestDirect + TestDirectAuto), full matrix green.

## ctx-showcase — the payoff on one page, executable
Completed: 2026-09-01
TestShowcase (okay-obs, 3 tests) distills the context-function arc
into one witness: `api: (Principal, Tracer) ?=> Traced.Route` — its
needs ARE its type — runs (1) behind the production doors (a
verified JWT becomes the Principal, tracing wraps it), (2) under
`provide(ada, tracer)` as a unit test with no token and no HTTP
machinery, and (3) under `providing`-composed environments with one
layer overridden (`base and providing[Principal](bob)` answers
for:Bob). One value, three worlds, zero changed letters; a missing
capability is a compile error in all three. Guide §9 gains the same
page as prose.
## demo-flow-cmds — the offline driver speaks scenarios
Completed: 2026-09-01

Three phrases complete the offline mode: "сценарий <имя> роль=email
…" starts any registered flow (and lists its transitions with their
roles), "шаг <N> <переход>" fires the writer's transition, "флоу <N>"
shows state and history. The escrow walk runs through real routes by
phrases alone — the wrong role refused with the reason, the buyer's
page ringing on the seller's sign. The no-model mode now covers
everything the model can drive. 13 demo tests.

## applicative-op — `<*>`, the idiom bracket's own spelling
Completed: 2026-09-01
`trait Applicative` gains the symbolic alias: `f <*> a` is `f.app(a)`
(inline, Monad.scala) — `pure(f) <*> fa <*> fb` now reads as written
in the papers. Works over any carrier through the generic door:
TestApOp runs one generic idiom over `[X] =>> Env ?=> X` (context
functions, via ctxMonad) and over `X ! Pure` (the effect row). Bare
ctx-fn receivers still hit E10 outside generic code — the known
boundary. Matrix 275/14/14.

## direct-macro — the flat block v1: direct style with no for-comprehension
Completed: 2026-09-01
Landed as 96a46e8 (nearly lost once: a no-op self-merge in the
worktree read as landed and the branch deleted — recovered from the
dangling hash; the merge-alone rule's "from the main checkout" half
is now twice-paid). direct[F] { val x = m.?; ... } rewrites at
compile time into Monadic's Cont binds (~300-line Quotes macro):
statement folding, ANF hoisting of value slots in application spines
(order asserted), if/match with effectful scrutinee/branches, &&/||
desugared to their If keeping the short-circuit, op.!? lifting a raw
effect operation into the block's row (Free.Inject emitted at the
Row the macro extracts from F = A ! Row). Marks under
lambda/while/try/by-name: positioned compile errors naming the
workaround. Effects first-class; F infers from the expected type.
16 tests. REFUTED and recorded: .! for the op mark (an imported
extension named ! shadows object ! — !.run breaks); isInstanceOf on
quotes-reflect types (erases to always-true, TypeTest patterns are
the way).

## obs-durable-overlay — the journal/trace identity join
Completed: 2026-09-01
specs/obs.md's last open box: a journaled operation's span and its
journal entry now carry the same operation identity, so an incident
replayed offline lays its spans over the originals. The identity that
survives a replay already existed — the Durable `Entry.key` (`keyFor`:
the step's position and what it asked for, nothing per-process). A
journaled operation opens a span carrying that key (`durable.key`, plus
`durable.op`/`durable.seq`); `Durable.replaying` stamps the SAME key
with `durable.replay=true`, so filtering by `durable.key` overlays the
replay on the original. The coupling stays OFF the main graph: a
neutral `OpTrace` seam lives in okay-agent (which does not depend on
okay-obs), `Durable.tools`/`replaying` take an optional `OpTrace`
(default None = no span, no cost), and `okay.obs.Tracer` adapts to it
in one line. Journal and trace stay two things — the span carries the
identity, it does not merge them. Proven twice: a fake sink in
okay-agent (TestDurable, the stamping) and a real Tracer over a trace
topic in okay-obs (TestOverlay, which Test-depends on okay-agent —
okay-obs is a leaf, so no cycle). Build: okayObs gains okayAgent.jvm in
TEST scope only.

## demo-flows — generic scenarios ring the chat
Completed: 2026-09-01

flow_advance joins the demo's wrapped tool table: a fired
transition's notifications are delivered to the role-holders'
inboxes with templates filled — any registered scenario's steps ring
the right pages with no per-scenario code (the deal's hand-written
onResponded now has a generic sibling). The prompt teaches the model
the flow tools. 12 demo tests.

## ctx-monad-instance — okay's Monad over context functions, for the generic combinators
Completed: 2026-09-01
The E13/E15 "not adopted" verdict was incomplete: direct style needs
no instance, but traverse/sequence/replicateA are written ONCE over
any F and need exactly an instance — juxtaposition cannot replace
them. Core now carries `given ctxMonad[E]: Monad[[X] =>> E ?=> X]`
(Providing.scala): pure is the value, flatMap is literally f(fa) —
the compiler's own auto-application is the Reader diagonal, so the
instance certifies semantics the elaborator already runs.
`sequence(Seq[Env ?=> Int]): Env ?=> Seq[Int]` works with F
inferred. Method syntax on bare ctx functions stays out (E10:
receiver applies before extension lookup). Tests: TestCtxMonad (4).
Matrix 257/14/14 — the global given collides with nothing.

## ctx-wire — the consumer one-liner: wire[A] is Reader's ask
Completed: 2026-09-01
The other half of the vocabulary (E17 in specs/context-functions.md):
`inline def wire[A]: A ?=> A = summon[A]` pulls the ambient
capability by naming its type. The naive `def wire[T] = summon[T]`
does not compile — no given at the definition site; the `A ?=> A`
result type is the fix, and the E10 eagerness finally works FOR us:
`wire[Db].q` applies in receiver position, `val d = wire[Db]` lands
as a plain Db, and doors write point-free (`val getQ: Db ?=> String
= wire[Db].q`) — no summon, no parameter. A missing given stays a
COMPILE error. Composes with providing/and (nearest wins). Core:
Providing.scala; tests: TestWire (4). Matrix 253/14/14.
## match-scenarios — scenarios as data; the deal becomes a definition
Completed: 2026-09-01

The review question ("can we add new scenarios? how?") gets the
registry answer a second time. ScenarioDef/Transition/Flow: roles,
states, terminals; a transition BELONGS to a role (the generalization
of "respond is the asked provider's alone"), carries the visibility
unlocks it grants (generalizing contacts()) and notification
templates. validate answers malformations as data (unknown
role/state, terminal with exits, unreachable terminal) and an invalid
definition is not registered. advance is the ONE engine method; the
deal machine is now the built-in ScenarioDef.deal running on it. The
universality proof: a three-role escrow housing sale runs with zero
engine changes, unlocking the address only at release. Flows and
unlocks are durable (sqlite restart test); definitions are
configuration. Stage 1 landed with it: the phantom-indexed
ScenarioBuilder — a route naming an undeclared state does not
compile (match-type membership, no macros) — the safe pen for
definitions written in code, the data form staying primary. Tools:
flow_start/flow_advance/flow_state/scenario_get. 27 match tests.

## security-crypto-split — the SCRAM primitives on a shared, dependency-free seam
Completed: 2026-09-01
okay-pg's SCRAM used a local `PgCrypto` given because okay-security's
fuller Crypto seam drags okayHttp (the JWKS road) and cycles the build
through this project's test edge. That local copy retires: a new
crypto-only module `okay-crypto` (specs/sql.md) holds the four
primitives SCRAM and password hashing need — hmacSha256, sha256,
pbkdf2, randomBytes — as a per-platform given (JCA on the JVM,
node:crypto on JS), resting on NOTHING, so any module can depend on it
without the http drag. okay-pg now depends on okay-crypto: PgCrypto*
deleted, `Scram` and `PgSql.connect` take `okay.crypto.Crypto`, the
test given imports move to `okay.crypto.given`. The four primitives are
pinned to published vectors (TestCrypto: NIST sha256("abc"), the RFC
fox HMAC, the PBKDF2-HMAC-SHA256 password/salt/1 vector), and the live
SCRAM battery (15 pg tests over the dockerized Postgres) proves the
seam end to end. The signing surface (RSA/ECDSA, JWT key handles) stays
in okay-security, which owns those heavier concerns — the split is by
dependency weight, not a move of everything. Build: new lazy val
okayCrypto (JVM+JS), okayPg depends on it, root aggregate updated.

## monadic-reflection — Filinski's reflect/reify over Cont: direct style for any Monad[F], no macros
Completed: 2026-09-01
Landed as 84d955f (spec d35c6a2, demo hotfix b725d19). object Monadic:
reflect is ONE extension serving three spellings (m.reflect,
reflect(m), and the symbolic m.? — Rust's postfix question
generalized), reify is the delimiter back into F; answer-type
modification types it precisely (Cont[A, F[B], F[B]]). Multi-shot
PRESERVED (a reflected List runs the continuation per element — the
"direct style forfeits multi-shot" note in specs/context-functions.md
corrected: that is Loom's cost, not this road's). FINDING: stack
discipline is the reflected monad's own — strict flatMap (Option)
costs a frame per reflect, trampolined A ! F runs 100k reflected binds
flat. 10 tests (TestMonadic); full matrix green. Rode along: master's
okayDemo/Test compile fix (two munitTimeout overrides from ebd344a +
99364a6 — kept 180s).

## ctx-provide-and — provide composes applicatively, the 22 cap falls
Completed: 2026-09-01
The missing combinator of the provide family (E16 in
specs/context-functions.md): `providing[A](a)` builds an installer
as a VALUE carrying `F[X] = A ?=> X`, and `and` composes installers
by composing the type constructors — `F[G[X]]` IS the curried chain
`A ?=> G[X]`, so `(providing[Db](db) and providing[Log](log)) {
app }` installs both without nesting and without the tuple. Type
lambdas reduce where the E11/E12 match-type route stalled, so the
using-method body eta-expands into the chain at the call site. The
right operand of `and` is the inner layer — override under
nearest-wins as plain data (`base and providing[Log](testLog)`).
Compositions are values: build a base environment once, reuse and
override per test. No arity cap — 25 layers tested past
ContextFunction22 (composition is heterogeneous, the type grows, so
no homogeneous fold — chains are written explicitly). Core:
Providing.scala; tests: TestProviding (flat composition, value
reuse, right-wins override, 25 layers, missing-dependency
compile-error claim). Core matrix 239/14/14 on JVM/JS/Native.

## kafka-eos — exactly-once on the Kafka interop, inherited from the engine
Completed: 2026-09-01
The stage-3 persist-interop rule "an engine keeps its own ops" cuts
both ways: Kafka HAS exactly-once, so the interop now exposes it
(specs/persist.md). The producer is idempotent by default
(`enable.idempotence`) — a retry after a lost ack cannot duplicate,
effectively-once to Kafka; the consumer reads `read_committed`, so a
reader never observes an aborted or in-flight transaction and `end` is
the last stable offset. New `KafkaStore.transaction(transactionalId) {
tx => tx.append(topic, partition, k, v) }` runs appends across
partitions AND topics atomically — commit on a normal return, abort
and re-raise on a throw — over a transactional producer cached per id
(initTransactions once, fenced by the id), closed with the store. The
own-engine file store gains nothing: this is Kafka's feature exposed,
not reimplemented, and the out-of-scope note stands for the own
engine. 3 live tests (TestKafkaEos, skip when the broker is absent):
a committed transaction's records appear together and in order; an
aborted one is invisible to a read-committed reader; one transaction
spans two topics atomically. The existing kafka suite (13) unchanged
and green — read_committed leaves non-transactional offsets identical.
One live wart stated in the spec: a read immediately after commit must
tolerate the last-stable-offset propagating (the test retries briefly).

## ctx-provide-n — the Cats mapN answer applied: 22 generated arities
Completed: 2026-09-01 (landed as 8265a2e)
Their "unbounded" is 22 generated overloads; so is ours — each a
one-line delegation, tools/gen_provide.py regenerates, capped where
the platform caps (ContextFunctionN ends at 22). Tested at 8 and at
the cap; core green on all three platforms (234/14/14). The
single-definition tuple route stays recorded as blocked (E11/E12)
with the missing compiler piece named.

## persist-wire-repl — replication crosses the wire, machinery unchanged
Completed: 2026-09-01
The stage-2 replication surface joined the documented wire (specs/
persist.md), version bumped to 2 with the new message cases APPENDED
so no v1 CBOR ordinal moved. Three frames added: produce (idempotent —
the retry across the wire answers the ORIGINAL offset; a stale seq
refuses by name), promote (the operator's failover, driven remotely;
the epoch advances) and compact (the Topic surface, completed). The
JVM `Wire.Server` gained a `repl: String => Option[Replicated]`
resolver: a replicated name serves through its coordinator (reads
truncate to the hwm, appends fence by epoch), every other name stays a
plain engine topic, and produce/promote on a name with no coordinator
refuse by name while the connection survives. The other direction —
replicas go remote — is a new `RemoteStore` that presents a
`Wire.Remote` as an ordinary synchronous `Store`, so the SAME
`Replicated` (not a variant) holds a remote replica: the eager push is
the remote's Append, the replicate-pull is the remote's Read, driven
on the coordinator's own thread (the okay-pg blocking waist under the
async client, JVM-only by design). Proven live over loopback: the far
node ends up holding the very bytes no in-process replica wrote, and a
lagging remote is caught up by replicate() over the wire. 5 new tests
(TestWireRepl), the existing 7 (TestWire) unchanged and green.

## ctx-everywhere — doors wherever the environment is a type; provide
Completed: 2026-09-01 (landed as 02098bf)
The operator's "everywhere, OPTIONALLY" executed with the operator's
own framing adopted: this IS the DI story — provide (core:
expression-scoped, nearest-wins, 1-3 arities) plus doors =
compile-time dependency injection, a missing dependency a QUOTED
compile error, zero framework. Doors: McpAuth.granted closes the
route family (protect refactored through one shared ladder);
OAuth2/Jwks/McpAuth gain ambient-Http forms; Tls.served
(Secrets ?=>, reshaped after an erasure clash — recorded);
Langchain4j.wired and S3.wired open the wiring family;
Configs.ambient. The environment-vs-resource line drawn; the
two-line recipe in typepedia; guide and five module pages updated.
Verification note: three full-matrix runs were SIGTERM-killed
externally (a sibling pkill, admitted in the room) at 1082/1082/
1089 tests with ZERO failures; all ten touched module suites green
directly (480 tests).
## match-deals — the negotiation: several candidates, the confirmed match
Completed: 2026-09-01

Deals complete what Vis.Matched promised: inquire/respond/withdraw
with Asked -> Accepted | Declined | Withdrawn, respond the asked
provider's alone, and contacts(viewer, other) unlocking Matched
facts (and platform AfterMatch gates) ONLY under an accepted deal —
both engines, sqlite parity, restart survival. The demo runs the
round: numbered candidates, the client chooses whom to ask (several
is wise — someone agrees), providers answer in their own chats, an
acceptance hands the seeker the unlocked contact and stands the rest
down, a full-decline round says the request still stands. The round
policy is store-driven and restart-surviving — PState/Delim were
CONSIDERED for it and declined with the reason written down: this
protocol spans processes and days, and state that must survive a
boundary belongs in data, not in a continuation (the same criterion
that placed them in transact/wizard/stepper, where the whole
protocol lives inside one program). Domains are anybody's: the jobs
round is the demo test, housing the engine test, repairs the live
one. 21 match tests, 11 demo tests.

## sql-pg-node — the pg driver reaches Node; sql.md's last box
Completed: 2026-09-01 (landed as a4e491e)
okay-pg cross-built JVM+JS. The message pump was restructured onto
the Net seam: it now PULLS bytes as a sequential Async program
(receive = readFully(5) then the body; collectReady folds to
ReadyForQuery, an error drained to quiet so the session survives)
instead of blocking-read calls — so the SAME driver runs over a
blocking socket on the JVM and over Node's buffered net. SCRAM
kept the room's phase-object shape but its three primitives + nonce
now come from a per-platform PgCrypto given (JCA / node:crypto) —
okay-security's fuller seam drags okayHttp and would cycle the
build, so security-crypto-split is filed. cancel() became a marked
rollback settled before the next use (no sync I/O on the async
leg). The acceptance: TestPgNode — a NODE process speaks SCRAM and
portals to the dockerized Postgres and gets 42 back, a wrong
password refused by SCRAM itself, no JVM/JDBC in the process; the
whole JVM live battery green THROUGH the new pump proves nothing
regressed. Every behavior box of sql.md is now checked. Merge read
alone after one refused ff (claim-only divergence): exit 0. Full
matrix green on a quiet machine.

## docs-sweep — the landings reach the docs
Completed: 2026-09-01 (landed as a963374; markdown only)
Ten module pages born (blob conf demo docs-mongo java langchain4j
obs py security-argon2 tls), six updated (security ES256/OIDC/
granted; jdbc Migrate/BulkLoad/Poll; persist Doctor/Configs/stages;
cache stage 2; ui Scope-capability/Nav-boundaries; llm Cut). The
guide gains phased stages, ambient prompts, Blocking and a
Capabilities section; typepedia records PState's consumers (no
longer an exhibit), Blocking and ambient Prompt; theory ch.2 names
its shipped consumers; ROADMAP P9 closes two of three opens; the
module index catches up by fourteen rows.

## ctx-adopt — the third capability route, the Blocking value, the documented edge
Completed: 2026-09-01 (landed as f11ec6c)
Secure.granted: the principal ambient by pure delegation (the
401/403 ladder byte-identical to bearer). The composition crown
holds: ONE stored (Principal, Tracer) ?=> Route serves protected
AND traced under stacked installers — deferred requirements compose
as arrows. Blocking[A] names core practice as a type (stored,
forced only where CanBlock is given). The edge patterns (given-chain
+ import-thread with the footgun) moved into typepedia with their
E-numbers; conf.md points at them. ctx-wiring's gate noted possibly
open, offered to the demo lane. Matrix 1599.

## wire-node — one socket leg for every wire; the log reaches Node
Completed: 2026-09-01 (landed as 594faf1; spec first)
Net in the core (specs/net.md): the byte-stream seam as a given per
platform — ONE blocking file in scala-jvm-native serves JVM and
Native (both ship java.net.Socket), the Node leg buffers `data`
events behind Async.await pulls so every protocol pump stays a
sequential program. persist's wire protocol moved to SHARED
WireProtocol (Version, the enums, frame helpers over NetConn, the
cross-platform Client); `export` kept every Wire.* path compiling
and TestWire untouched. THE headline, the openness acceptance made
literal: the SAME client code talks to a scripted Node net server
answering frames encoded with the SAME shared enums — with no JVM
in the process. En route, two forward-fixes for the fresh chat
demo under the day's house rules (a JDBC-carrying module forks its
tests; live calls get 120s) plus the third rule those exposed: a
FORKED test JVM keeps the repo root as cwd when the suite indexes
File("."). sql-pg-node now has its transport; the PgSql pump
restructure stays its own claim, stated. Merge read alone after
two refused ffs (demo landings; targeted retests): exit 0. Full
matrix green.
## demo-chat-async — the reverse chain: events in either order
Completed: 2026-09-01

"Мне нужно починить велосипед" today, nobody fits; "я умею чинить
велосипеды" tomorrow — and the seeker's page rings. The chain is
STRUCTURAL, not the model's: the tool table is wrapped, every
facts_assert of an offer runs the reverse search over stored needs
(and vice versa, floored by similarity — the embedder seam's
business), and a hit lands in the matched profile's inbox — an SSE
stream (/events/<email>) both pages hold open from the first email
they see, rendering 🔔 bubbles. Needs are stored before searching
(driver and prompt both). The two-window story is a deterministic
test through real routes: need waits, offer arrives, the open stream
receives the match — plus the hello frame that flushes SSE headers
(client.send blocks without it) and the email-in-the-PATH lesson
(requestOf keeps the path; a query string never reaches a route).
10/10 including the three live legs.

## ctx-functions — what the capability arrows buy us, verified first
Completed: 2026-09-01 (landed as 67e11ad)
specs/context-functions.md: the FULL map on its experimental base
(E1-E8 — same-type rebinding impossible; type-changing given-chains
linear; the import-thread works via NAME shadowing incl. LTS;
nested using-params resolve NEAREST; stored ctx-fns self-apply;
macros cannot rescope). Shipped: implicit prompts for Scope
(mark/exit/bounded) and Cut (guard/violation/watched) — exit to the
nearest scope by nesting, bound prompts still cross; Obs
Traced.route (Tracer-capability routes, per-request roots, stored
route values self-wiring). Filed: ctx-blocking, ctx-edge-docs,
ctx-wiring, ctx-reader-bridge (gates named). Rejected with reasons:
ui builder DSL, macro direct-style. Rode along: the sqlite
DriverManager race named (third telling), demo-chat-live-budget
filed. Matrix 1588.
## demo-chat-seek — the seeker's question, answered live
Completed: 2026-09-01

The user asked "а найдёт?" and the answer is a test now: with a bike
repairman in the store, "мне нужно починить велосипед, найди мне
кого-нибудь" (no prefix, no hints) runs the intake across two turns —
the model asks for the seeker's email, receives it, registers, calls
find_candidates and reports the master with his skills. Asserted
against the local model.

## demo-chat-ungated — the model decides when to match
Completed: 2026-09-01

The /match gate is gone when a model is configured: every turn is an
agent turn, okay-match's tools are always on the table, and the
system prompt hands the DECISION to the model — offer or need means
work the marketplace, anything else means just answer. The live test
asserts both halves against the local model: a bicycle-repair offer
with no prefix anywhere reaches the tools (stored, or the email asked
for), and "какая столица Франции" leaves the marketplace untouched.
/match survives as the no-model driver's prefix and a forcing hint.

## sql-pg-copy — the bulk-load posture on the free engine
Completed: 2026-09-01 (landed as merge; box in specs/sql.md)
copyIn speaks the simple-protocol COPY dance (CopyInResponse /
CopyData / CopyDone) with the text format's escapes proven
round-trip (tab, newline, backslash, NULL); a thousand rows land
in one command. The load-id posture where plain Postgres has no
per-file load history: a loads REGISTRY whose claim row commits IN
ONE TRANSACTION with the data — the retry answers AlreadyLoaded,
and a crash between COPY and commit (tested by killing the
connection mid-load) rolls back claim AND data together, so the
retry lands exactly once overall: WithKey at batch granularity,
made physical. sql.md now has one open box: the non-JVM consumer
(sql-pg-node). Merge read alone: exit 0. Full matrix green.
## demo-chat-match — the marketplace joins the chat
Completed: 2026-09-01

/match turns are matchmaking turns over one shared MemoryMatch per
server. With a model configured the turn is an AGENT conversation —
Provider.openAi/anthropic as Handler[Model], okay-match's Tools.table
as Handler[Tool], a system prompt teaching the intake — and the LIVE
test proved the local rozum model driving the real tools end to end
(it stored the welder's offer, or asked for the missing email — both
honest outcomes asserted). With no model, a deterministic driver
speaks THE SAME tool table, and the offline test walks the two-sided
story through the real route: "умею класть плитку" chats in, "нужен
плиточник" finds it, the marketplace remembers across turns and
sessions. And the marketplace is DURABLE by default: sqlite
(OKAY_CHAT_DB, ":memory:" opts out) — which made the store interface
nominal (MatchStore: both engines spoke it structurally, the trait
writes it down, Tools.table takes any store), caught sqlite's
booleans-as-integers dialect trap with a parity suite, and proved
restart survival over the same file. 7 + 1 tests.

## typed-bad-repair — damaged records ask
Completed: 2026-09-01 (landed as 0a321aa)
Conditions' first consumer outside the core: Repair in okay-persist
— decode/read over the Typed view where each damaged record
SIGNALS Damaged(offset, error, raw bytes) under a per-element
"skip" frame. One log, three answers under three policies: patched
IN PLACE (the corrected value sits exactly where the damage sat),
skipped with order intact, aborted naming the offset and the
declined menu. And the additive rule's other half, tested: a clean
slice never consults the policy — who never signals never pays.
Merge read alone: exit 0. Matrix green on rerun (one TestHttp
port-roulette flake, 11/11 alone — the ledger entry stands).

## demo-chat-move — the frontend moves in with its demo
Completed: 2026-09-01

okay-chat-web relocated to okay-demo/web (user ask): the chat's
frontend lives inside okay-demo now; the separate sbt module remains
only because a JS cross-build cannot ride a plain JVM project. Paths
rewired (appJs discovery, build.sbt), all 8 tests green after the
move including the live local-model leg.

## conditions — resumable exceptions: the road between throwing and damage-as-data
Completed: 2026-09-01 (landed as 99d44dc; spec first; user ask)
Condition in the core: signal raises WITHOUT unwinding — the
policy runs while the signal point's continuation is live, so
Resume(v) continues FROM THERE with the value (the effect system
was a resumable-exception system waiting to be named); within
establishes named restart frames the policy can unwind TO
(Invoke — the Delim shape, one machine owning frames and menu per
Delim's own payload-erasure discipline); Fail escalates naming the
condition AND the declined menu; invoking off the menu is the
POLICY's named bug (NoSuchRestart). The repair story proven: a
decode loop with a skip frame per element — ONE program answers
patched/skipped/failed under three policies. Additive by the
operator's rule: Throws, runEither, damage-as-data untouched; a
program that never signals never pays. Eight tests. Filed for
later: Typed.Bad interactive repair, r.md's native restarts over
the now-shared vocabulary. Merge read alone after one refused ff
(nav-pop divergence, ui-only; core retested 227): exit 0. Full
matrix green.
## demo-chat-react — the React frontend, and the live leg proven locally
Completed: 2026-09-01

okay-chat-web (cross): the chat's brain is pure view/update over the
Ui tree, JVM-tested with scripted events (send flow, token folding
into the open bubble, the cut line) — the React frontend's logic
never sees a browser in its tests. The JS leg is glue only: okay-ui's
ReactJs against CDN React UMD (the first time the mapping meets a
REAL React), the Elm fold on runAsync (no CanBlock on JS — the event
loop is the runner), and a fetch reader feeding $token/$done/$cut
into the same bus the clicks use. ChatDemo serves the React page and
/app.js when a link exists, the vanilla page otherwise — and gained
the third model filling: OKAY_CHAT_BASE speaks any OpenAI-compatible
endpoint, and the LIVE test streams through the local rozum model on
:8089 (green on this box) — the live box closes without an Anthropic
key. 5 + 3 tests.

## nav-pop-to-screen — the pattern held, the mechanism corrected
Completed: 2026-09-01 (landed as 84617de)
A typed Key names a boundary screen; PopTo drops every intervening
frame — none stepped, they are DATA — and the boundary routes the
typed answer; boundaries chain, the outer pop crosses the inner, an
absent key names nothing (total). The spec's mechanism claim
corrected in place: Dialog needs Delim's capture because its
continuations are implicit; Nav's stack is reified data, so the
boundary is a marker and the exit is a drop — the adoption
doctrine's own test applied to its own poster case. Matrix 1546.

## persist-offload — the cold tail becomes the lake
Completed: 2026-09-01 (landed as 7c19340)
Segments: the documented disk format gets a PUBLIC reader in
okay-persist (Doctor's certification knowledge as a library —
bytes parse into records wherever they live, torn tails end
soundly). Offload on the blob side: verified-then-evict under a
local byte budget (a segment leaves only when the blob's copy
matches its size; the active file never leaves; begin advances
exactly as under retention — proven across reopen), and the
Tiered Async read where TooEarly stops meaning gone and starts
meaning COLD: blob history strictly below the local begin plus the
local tail, byte-exact, bounded (the overlap bug — backup holds
copies of still-local segments — caught by the first run and
fenced by the strictly-below rule). Dependency direction blob →
persist compile, safe (persist rests on core+codec; reverse cycles
through http). Merge read alone: exit 0. Full matrix green.
## demo-chat — the chat with an LLM, as one JVM main and no build step
Completed: 2026-09-01

The user-requested showcase, out of what already ships: okay-jetty
streams the SSE reply body live (Source[Chunk[Byte]] chunk by chunk
on a virtual thread), okay-llm's Anthropic.stream speaks the real
API when ANTHROPIC_API_KEY is set, Cut guards the stream with a
token budget the page renders as a visible scissors line, and the
offline mode IS the demo — the scripted model streams the same
framing, so the acceptance proves the same path on a real socket:
first frame read incrementally before the end, done marker, and the
over-budget run cut at exactly the budget with the rule named and
nothing following. GET / carries the whole page inline (dark, small,
fetch-reader appending tokens as they arrive). `sbt okayDemo/run` →
http://127.0.0.1:8090. 3 tests.

## stage-phased3 — one more arity, because the consumer exists
Completed: 2026-09-01 (landed as 1444810)
The http message shape needs exactly three phases; chaining two
phased cannot express it (the middle's end is the third's TYPED
start). No phase enum, both switches through PState, the answer
names the dying phase three ways, and the does-not-compile proof
stands at BOTH seams. Driven by the consumer's shape: request-line
-> headers -> body. Matrix 1537 (the day's sibling landings ride
in the count). http-message-phases (the Nio refactor) is next.

## sim-harness — luck retires from concurrency testing
Completed: 2026-09-01 (landed with spec boxes checked)
Sim in the core: many fibers, one seeded scheduler, interleavings
as VALUES — a found bug is a seed, a fix is verified by replaying
it. Fibers are freer trees and their k at every operation IS the
captured delimited continuation (the Cont foundation as scheduler
food — the operator's primary-where-necessary rule, satisfied);
SimChannel makes blocking primitives operations; the virtual clock
moves only when nothing else can; deadlock is an OUTCOME, not a
hung test; fault plans ride the seed. The headline: the runCmd
close race, modeled, loses its answer under seeds a 200-sweep
finds, and the shipped rule survives all 200 — today's flake is
now a replayable regression test. One lesson: continuations apply
at SCHEDULING, not enqueueing (eager k ran side effects early);
tasks are thunks. Eight tests. Merge read alone: exit 0. Full
matrix green.
## llm-streaming-cut — the validator cuts the model off mid-sentence
Completed: 2026-09-01

Cut.guarded installs a typed prompt over a streaming generation;
Cut.checked stands in the token stream, emits what passes, and on a
violation ABORTS to the prompt — Left(Violation(rule, at, seen)),
the poisoned token never flows, and the source records NO further
pulls (the counter had to become Async data to observe that
honestly — uncons builds one node ahead). A passing stream is
identical to the unguarded run; nested guards prove multi-prompt:
the inner cut recovers, the outer stream continues. The open P9
roadmap item, closed with Delim as the PRIMARY mechanism per the
adoption doctrine; the unguarded path untouched. 3 tests; the live
probe stays open pending an API key, mechanism covered scripted.

## control-specs — the PState/Delim consumer map, written down
Completed: 2026-09-01 (landed as bff0581; markdown only)
The operator's adoption doctrine stated once (delimited-control.md):
ADDITIVE by default — a wrapper, an extra combinator, a typed
internal, never a rewrite; PRIMARY only where no equivalent exists,
which today is exactly cross-boundary abort/cancel. Six sections in
the owning specs: llm-streaming-cut (closes the open P9 item's
design), stage-phased3, http-message-phases (doctrine home deferred
to the wire lane's typestate.md), nav-pop-to-screen (ui lane's),
logic-named-cut and r-restarts (both GATED, gates named). Slugs
filed; pg-scram amended to defer its form. Design discussed in the
room: capture-at-Async, prompt-machine non-collision, and
internals-only transact all settled with the sibling lane.
## pg-scram-typestate — the handshake's order is the type's shape
Completed: 2026-09-01

Scram rebuilt as PHASE OBJECTS (the wire-typestate family; phase
objects where PState's Cont bridge buys nothing): ClientFirst's only
step is serverFirst, ClientFinal's only step is serverFinal — an
out-of-order step does not EXIST as a method (compileErrors-pinned
both directions). The one-object Scram class stays as the adapter
over the phases (same API, same bytes — usable without the types),
and even there the old silent NPE on a misordered server became a
named PgError; PgSql's driver loop now holds the phase and names
SASLContinue/SASLFinal arriving out of order. The whole dance is
pinned to the RFC 7677 test vector byte for byte, mutual
verification included. 4 tests.

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
