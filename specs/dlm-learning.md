# dlm-learning — the model's learning mode: governed, audited, explainable, correctable

Status: specification, 2026-09-28, written before the code at the
operator's word: *«в моей модели есть режим обучения. Но тут нужно
будет подумать о безопасности — чтобы процессом обучения можно было
управлять и его контролировать, чтобы был аудит и прозрачность самой
модели, чтобы её можно было изучать и исправлять изнутри — и снаружи
тоже, при наличии прав».* Owner lane family: `dlm-*`. The model is
specs/dlm.md; this is what changes it.

## 1. Overview

A deterministic dialogue model learns in one way only: a person pairs
a sentence with a class the model already has. «Надо было: мои
заявки» after a misroute is a lesson; a confirmed intake is a
harvested row; a reviewed row is a corpus row and a rebuilt table.
Nothing in it generates text, invents a class, edits a rule or moves
a threshold at request time. That is what makes learning here SAFE
BY CONSTRUCTION — and it has been an accident of implementation, not
a stated guarantee. This spec states it, and adds the four things the
operator named around it: a policy that says who may teach what, a
ledger that says what changed and why, an explanation of every
decision, and a door to correct the model from outside under rights.

The organising claim: **learning is a fold over a journal, and every
control on it is a control on what enters the journal or on how the
fold reads it.** Nothing is deleted; a correction is a record; the
model at any moment is the fold of the records up to it; an audit is
a read of the same records. There is no second system.

## 2. The three channels, and what each may change

| channel | who | what enters | what it may change | takes effect |
|---|---|---|---|---|
| **lesson** | the person, about their own sentence | `Memory.Event.Taught(who, earlier, intent, offset, at)` | the routing of THEIR OWN same words, exact band; shared to everyone at N people or by a teacher | next turn |
| **withdrawal** | the same person, or a right | `Memory.Event.Withdrawn(who, earlier)` | removes the lesson; a shared pair loses a holder | next turn |
| **corpus** | a person with the right, through review | a row in the authored corpus, then a rebuilt `Exemplars` | the vector layer's opinion of everyone's sentences | next build |

What NO channel may do, and a test holds each:

- create a class: a lesson to an intent the set does not have is
  refused at the fold (`Intents.byName` is the gate);
- change a rule, a slot, a `require`, a threshold, a margin;
- change the encoder or accept a table another encoder compiled;
- affect a person other than the teacher, below the sharing bar;
- write anything the fold cannot replay to the same model.

## 3. Interface

```scala
package okay.dlm

/** who may teach what — the policy seam, ours by default */
trait Teaching:
  /** may this person teach this pair for themselves? */
  def own(who: String, intent: String): Boolean
  /** may this person's lesson become everyone's on its own (a teacher)? */
  def teacher(who: String): Boolean
  /** may this person act on somebody else's lessons (withdraw, share, revoke)? */
  def steward(who: String): Boolean
  /** the kill switch: learning off entirely, or per channel */
  def enabled(channel: Teaching.Channel): Boolean

object Teaching:
  enum Channel: case Lesson, Withdrawal, Corpus
  /** OURS: everybody teaches themselves, nobody is a teacher or a
   * steward, every channel on — the model as it has always learned */
  given ours: Teaching

/** every change to what the model knows, as it was decided —
 * append-only, replayable, and the audit is a read of it */
enum Ledger.Entry:
  case Learned(who: String, earlier: String, intent: String, offset: Long, at: Long, by: String)
  case Forgotten(who: String, earlier: String, at: Long, by: String)
  case Shared(earlier: String, intent: String, holders: Int, at: Long, by: String)
  case Rebuilt(artifact: String, encoder: String, before: Option[String], after: String, corpus: String, at: Long, by: String)
  case Refused(who: String, what: String, why: String, at: Long)

/** why the model decided what it decided, as one value */
final case class Explanation(
  text: String,
  route: Route,                                  // what was decided
  layer: Layer,                                  // which layer spoke
  rule: Option[String],                          // the rule that matched, verbatim
  lesson: Option[Lesson],                        // the lesson that applied, and whose
  noticed: Vector[(String, Support)],            // everything every layer saw
  scores: Vector[(String, Float)],               // the judge's ranking
  judge: Option[String],                         // who ranked it
  encoder: String,                               // what embedded it
  tables: Map[String, String],                   // the artifacts by name → hash
  language: Option[String])

/** the model, read and corrected — from inside by the conversation,
 * from outside by whoever `Teaching` lets */
trait Governed:
  def explain(text: String, who: String): Explanation
  def lessons(who: String): Vector[Lesson]
  def shared: Vector[Lesson]
  def teach(by: String, who: String, earlier: String, intent: String): Either[String, Ledger.Entry]
  def forget(by: String, who: String, earlier: String): Either[String, Ledger.Entry]
  def share(by: String, earlier: String, intent: String): Either[String, Ledger.Entry]
  def ledger(since: Long = 0L): Vector[Ledger.Entry]
```

A `Governed` is built over a `Memory`, a `Router`, a `Teaching` and a
`Ledger` sink; the app hands it its journal's records and its rights.
Every mutating door returns the entry it wrote or the reason it
refused, and a refusal is ALSO an entry: an audit that cannot see
what was declined cannot see an attack.

## 4. Behavior

- [x] a lesson to a class the model does not have is refused, and the refusal is in the ledger
- [x] a lesson never changes a rule, a slot, a threshold or a table — asserted over the model's own data before and after any sequence of lessons
- [x] a person's lesson routes only that person's words until the sharing bar; a stranger's same words are unchanged
- [x] `Teaching.enabled(Lesson) == false` makes every `teach` a refusal and leaves the fold as it was — the kill switch works without a redeploy
- [x] `forget` by the person, or by a steward, removes the lesson and unshares a pair only they held
- [x] `share` by a teacher makes one person's pair everyone's; by anybody else it is refused
- [x] `explain` names the layer, the rule verbatim, the lesson and its owner, the judge and the encoder — for a turn a rule decided, a lesson decided, a judge decided, and one nobody could
- [x] `explain` of a route stopped by a MISSING SLOT still names the layer that decided the intent, the rule verbatim and the lesson — the route carries no support, `noticed` does (dlm-explain-missing, found by okay-watch)
- [x] the ledger replays: the fold over `Learned`/`Forgotten` entries is the same `Memory` the live process holds
- [x] a rebuilt table is an entry with the hash before and after and the corpus it came from, so a wrong build is named and reverted by hash, not by memory
- [x] with learning off, a replay of the journal reaches the same decisions as were recorded — the model does not drift while nobody is teaching it

## 5. Design

**Rights are a seam, and ours is the narrowest.** `Teaching.ours`
lets a person teach only themselves and nobody act on anybody else's
lessons; that is the model as it learns today. A service that has
roles — admins, teachers — supplies its own `given Teaching` over
its own lists or its own identity provider. The library never stores
a right.

**The ledger is the journal read through one lens.** A service that
already journals every turn does not need a second log; `Ledger.Entry`
is what its `taught` and `taught-withdrawn` records already say, plus
what a build step writes. A library-side `Ledger.Sink` is a function
of an entry, and ours appends to a `Vector` — the app's is its
journal topic.

**An explanation is data, not a sentence.** `Explanation` is a value
the app renders in the person's language (`/explain`, an admin
command, a line under a reply), and a value a test asserts on. The
vector layer's contribution names the judge, so «Jev said so» and
«our probe said so» are told apart in every audit.

**A table is identified by hash.** `Exemplars` gains a content hash
(the checkpoint's bytes are already a pure function of the numbers);
`Rebuilt` carries the hash before and after and the corpus commit or
file hash. Reverting is «serve the table with hash H», which the
image already supports by name.

**Nothing moves at request time but the fold.** Thresholds, margins,
rules and tables change in a build a person runs, gates and ships
(specs/dlm.md, "What it deliberately is not"). Learning at request
time is the memory fold and only it, bounded per person, windowed,
shared at a bar — and switchable off.

**The System One door is the outside.** The same wire that answers
questions (`okay-dlm-remote`, `Service`) is where an outside agent
with rights teaches and reads: `/v1/systemone` answers; a sibling
`/v1/explain` explains; `/v1/lessons` reads and, under a right,
writes. One door, one rights check, one ledger.

## 6. Decisions

**No new class by learning, ever.** A class is a decision a person
makes in the corpus; the model cannot be talked into one. This is the
line between a model that learns and one that can be steered.

**A refusal is recorded.** An audit that only shows what was accepted
is half an audit.

**The person's own words, first and by default.** The narrowest right
is the default because the widest incident this model has had was a
label with no lifetime attaching one sentence to twenty-five turns
(okay-chat specs/model.md §6).

## 7. Out of scope

- Fitting or calibrating at request time (specs/dlm.md, held).
- Federating lessons across deployments.
- Rendering explanations: the words are the caller's.

## 8. Staging

1. DONE 2026-09-28 (dlm-learning): `Teaching`, `Ledger`, `Governed` over the memory fold with ours by default; `Explanation`; `Exemplars.hash`; a test per behavior line, eleven of them.
2. DONE 2026-09-28 (okay-chat `learning-doors`): `Teaching` over its admin/teacher lists, `OKAY_CHAT_LEARNING` as the switch on every channel, the journal as the ledger (the door writes the chat's own record shapes, refusals as `refused` records), `POST /v1/explain`, `GET/POST /v1/lessons`, `POST /v1/lessons/forget` under a partner token or the admin token.
3. The table hash and `Rebuilt` in `Compile`; revert by hash — §9.

## 9. Stage 3 — the shelf: every table kept by hash, dropped only by a policy

The operator, 2026-09-28: *«старые чекпойнты храним, но можем их
потом удалять согласно политике».* A rebuild does not replace a table;
it puts a new one beside the old on a SHELF, both named by hash, and
says so in the ledger. A policy — a seam, ours keeps everything —
says which old tables may go; every one that goes is an entry too.

```scala
package okay.dlm

/** one table on the shelf */
final case class Kept(artifact: String, hash: String, encoder: String, at: Long, size: Long)

/** the tables a model has served, by hash — ours: a directory, or memory */
trait Shelf:
  def put(artifact: String, table: Exemplars, at: Long): Kept   // idempotent by hash
  def get(artifact: String, hash: String, expect: Option[(String, Int)] = None): Either[String, Exemplars]
  def kept(artifact: String): Vector[Kept]                      // oldest first
  def artifacts: Vector[String]
  def drop(artifact: String, hash: String): Boolean

/** which old tables may go — ours keeps everything */
trait Retention:
  def name: String
  def expired(kept: Vector[Kept], now: Long): Vector[Kept]

object Retention:
  given ours: Retention = all
  val all: Retention
  def latest(n: Int): Retention            // the newest n stay
  def within(ms: Long): Retention          // what is younger than ms stays
  def any(keep: Retention*): Retention     // a table any of them keeps, stays
  def of(spec: String): Either[String, Retention]   // "all", "last:5", "days:30", "last:5,days:30"

object Shelf:
  /** what a policy lets go — never the newest of an artifact, never one serving; an entry each */
  def prune(shelf: Shelf, keep: Retention, serving: Set[(String, String)], now: Long, by: String): Vector[Ledger.Entry]

enum Ledger.Entry:
  case Pruned(artifact: String, hash: String, policy: String, at: Long, by: String)   // new
```

**THE HASH IS OF THE TABLE AS SERVED.** A build writes F16; a boot
reads F16 back as floats. `Exemplars.stored(f16)` is the table a
checkpoint of it reads back, and the shelf keys by ITS hash, so the
hash an explanation names, the hash in `Rebuilt` and the hash of the
file on the shelf are one string.

**A REBUILD THAT CHANGED NOTHING WRITES NOTHING.** The ledger records
changes; a build of the same corpus under the same encoder hashes the
same, and an entry per run would make every build a diff.

**NOTHING ON THE SHELF GOES BUT BY A POLICY, AND THE POLICY IS NAMED.**
`Pruned` carries the policy's `name`, so an audit reads which rule
dropped a table as well as who ran it. The newest table of an artifact
and any table serving are never dropped, whatever a policy says.

**REVERT IS A REBUILD FROM THE SHELF.** «Serve the table with hash H»
is a `Rebuilt` whose corpus is `shelf:H` — the same entry, so a revert
is audited, and reverted, exactly like a build.

### Behavior (stage 3)

- [ ] a table put on the shelf and read back hashes the same, F16 included; putting it twice keeps one
- [ ] a shelf refuses a table of another encoder by name, like a checkpoint
- [ ] `Retention.all` drops nothing; `latest(n)` keeps the newest n; `within` keeps the young; `any` keeps what either keeps; `of` reads each spelling and refuses a wrong one by name
- [ ] `prune` never drops the newest table of an artifact nor a serving one, whatever the policy; each drop is a `Pruned` entry naming the policy
- [ ] a ledger written to a file reads back entry for entry, `Pruned` included
- [ ] (okay-chat) a build records `Rebuilt` with the hash before and after and the corpus by content hash, and keeps the old table; the same build twice records nothing
- [ ] (okay-chat) a revert serves the shelf's table and records a `Rebuilt` from `shelf:H`; a pin by hash at boot serves it, and a pin to a hash not on the shelf refuses to boot

## Results — dlm-learning (2026-09-28)

Stage 1, library only. `Teaching` (`roles`, `off`, `switched`; ours
the narrowest), `Ledger` (`Entry` with `Refused`, `Sink`, `Recorded`,
`replay`, the JSON wire), `Explanation` (`of` a router and a memory;
`encode`), `Governed` (`route`, `explain`, `lessons`, `shared`,
`ledger`; `teach`, `forget`, `share`, `rebuilt`), `Exemplars.hash`.
Eleven tests, one per behavior line; 100 in the module.

Two things the tests decided. A `Refused` names WHO was refused and BY
whom separately, because a steward refused for somebody else is not
that somebody's refusal. And `share` is `teach` in the teacher's own
name — the fold already shares a teacher's pair, so a second mechanism
would have been a second truth.

## Results — dlm-explain-missing (2026-09-28)

Found by okay-watch's check bot, whose `/why` explained a sentence a
LESSON had routed to `check` — its address slot missing — as «layer:
none, lesson: none». `Route.Missing` carries no `Support` by design, and
`Explanation.of` read support out of `Fires` alone; it now falls back to
`noticed`, which already holds every layer's reading of every intent it
saw. No type changed, so no consumer's pattern match moved. Two tests: a
Missing decided by a rule, and one decided by a lesson.
