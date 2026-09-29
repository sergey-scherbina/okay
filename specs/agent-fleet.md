# agent-fleet — agents as supervised actors, a hierarchy the parent grows

## Overview

okay-agent has the loop (`Agent.converse`), tools as an effect, the
conversation as a fold, durable tool journals and a paused intake. It has
**no notion of a second agent**: nothing spawns one, steers one, reports
one's status, or lets an agent hand part of its task to another. The first
consumer that needs exactly that is `../nadia`'s okay implementation
(nadia `docs/specs/app.md`, its `SPEC.md` §6): an operator drives a
hierarchy of coding agents from a chat and a console, and a parent agent
delegates the independent halves of its task to children whose steps come
out of its own budget.

The operator's rule for where this lives (2026-09-29): general-purpose
platform code is okay's, shown rather than hidden; the leaf keeps its
business logic. A supervisor has no nadia in it. This module is that
supervisor, built from what okay already has: an agent is an **okay-actor**
(typed mailbox, `spawnChild`, `Supervise`), its status is a **value**, its
transcript is a **topic** (okay-persist), and delegation is a **tool** in
the parent's toolbox — so a hierarchy costs the parent nothing it does not
already know how to do.

## Interface

```scala
package okay.agent

/** what an agent is asked to do, and within what */
final case class Spec(task: String, workspace: String, budget: Budget,
                      parent: Option[AgentId] = None, model: Option[String] = None)
final case class Budget(steps: Int, wallMs: Long)

opaque type AgentId = Long

enum Phase:
  case Running, Paused, Stopping, Done, Failed, Killed, Interrupted

/** one agent, as the operator and a screen see it — a value, so a chat
 * card, an HTTP body and a test read the same thing */
final case class Status(id: AgentId, parent: Option[AgentId], task: String,
                        workspace: String, phase: Phase, step: Int,
                        lastTool: Option[String], elapsedMs: Long,
                        children: Vector[AgentId], result: Option[String],
                        report: Option[Json])

/** what an agent can be told — the messages of nadia SPEC §6 */
enum Control:
  case Tell(message: String)     // delivered at the agent's next turn
  case Pause, Resume
  case Stop                      // finish the current tool, then halt
  case Kill                      // now; the workspace is released

final class Fleet(store: Store, run: Runner)(using Scheduler):
  def spawn(spec: Spec): AgentId ! Async
  def send(id: AgentId, c: Control): Boolean ! Async   // false: no such agent
  def status(id: AgentId): Option[Status]
  def all: Vector[Status]
  /** the log's projection, on start: running agents come back Interrupted
   * with their transcript; ids continue from the last */
  def restore(): Int
  /** an agent's transcript so far, as recorded */
  def transcript(id: AgentId): Seq[Turn]

/** what runs ONE agent: the consumer's loop, tools and gate — the
 * fleet supplies the mailbox, the budget and the record, nothing else */
trait Runner:
  def run(id: AgentId, spec: Spec, inbox: () => Vector[String],
          control: () => Option[Control]): Outcome ! Async
final case class Outcome(text: String, report: Option[Json], phase: Phase)

object Fleet:
  /** `delegate(task, subdir?, budget?)` for a parent's Toolbox: runs a
   * child to completion in the parent's workspace (or under it) and
   * returns its text and report; the child's steps are deducted from the
   * parent's remaining budget, and a child that crashes is a tool error */
  def delegate(fleet: Fleet, parent: AgentId): Toolbox.In[Async]
```

The events the fleet appends to its topic, one JSON record each, keyed by
agent id:

```
Spawned(id, spec, at)
Phased(id, phase, at)
Stepped(id, step, tool, at)
Turned(id, turn)            // one Turn of the transcript
Finished(id, text, report, at)
```

`Status` is the fold of those; `restore()` folds the topic on start.

## Events

What a screen folds instead of asking (nadia `BACKLOG.md` NAD-14, NAD-19):

```scala
object Fleet:
  enum Event:
    case Spawned(id: AgentId, spec: Spec, at: Long)
    case Phased(id: AgentId, phase: Phase, at: Long)
    case Stepped(id: AgentId, step: Int, tool: String, at: Long)
    case Turned(id: AgentId, turn: Turn)
    case Finished(id: AgentId, phase: Phase, text: String, report: Option[Json], at: Long)
  /** THE decoder of a record; restore folds through it */
  def event(j: Json): Option[Event]
  /** the log followed as events, from an offset, in any process that can read the topic */
  def events(topic: Topic, from: Long = 0, pollMillis: Long = 25)(using Timer): Source[Event]
final class Fleet:
  /** every record from now on, as it is written; a listener that falls
   * `capacity` behind is dropped, never the fleet held */
  def events(capacity: Int = 1024): Source[Event]
```

- [x] an in-process listener sees `Spawned`, `Turned`, `Stepped`, `Finished` in the order written
- [x] `Fleet.events(topic)` tails the log from the start and from an offset; a fleet folded from
      the same topic agrees with what the feed said
- [x] `event` decodes every kind; an unknown kind or a non-record is `None`, never a throw

## Commands

The control plane as data (nadia `BACKLOG.md` NAD-14, NAD-20): a screen in
another process appends to a `commands` topic through a `RemoteStore`, the
service folds it.

```scala
object Fleet:
  enum Command:
    case Spawn(spec: Spec, by: String)
    case Send(id: AgentId, control: Control, by: String)
    def principal: String
  def commandJson(c: Command): Json
  def command(j: Json): Option[Command]            // total
  enum Event: … case Refused(seq: Long, by: String, why: String, at: Long)   // on the agents record
final class Fleet:
  def spawn(spec: Spec, by: Option[String] = None): AgentId ! Async   // Spawned carries `by`
  /** follow the commands topic; apply after `allow(by, command)`, else Refused(seq = the offset) */
  def commands(topic: Topic, allow: (String, Command) => Either[String, Unit],
               from: Long = 0, pollMillis: Long = 25, applied: Long => Unit = _ => ())(using Timer): Unit ! Async
```

- [x] command JSON round-trips for every kind; what is not a command is `None`
- [x] an allowed spawn runs and its `Spawned` carries `by`; a denied command, a command to no
      live agent, and a non-command each put a `Refused(seq, by, why)` on the record, in order;
      `applied` hears every offset; a fresh fold of the record is not confused by refusals

## Approvals

The REPL's `y / n / a` (nadia `SPEC.md` §3.3) answered from any host (NAD-21):

```scala
final case class Ask(seq: Long, tool: String, args: Json)
enum Control: … case Approve(seq: Long, yes: Boolean)
enum Event: … case Asked(id, ask: Ask, at) · case Answered(id, seq, yes, at)
final case class Status(…, asking: Option[Ask] = None)
final class Ctx:
  /** on the record at once; parked until Approve(step, yes) — false on a stop or a kill */
  def ask(step: Int, call: ToolCall): Boolean ! Async
```

The policy — which calls to ask about, and whether "always" was said — is the
runner's; a runner that never asks is auto-approve. The `approve` command is
`Send(id, Approve(seq, yes), by)`.

- [x] an ask goes on the record and parks the runner; `Approve` with the wrong seq changes
      nothing; the right one lets it through and is recorded
- [x] a no is a no; a stop while asking answers no, and the runner is not left parked
- [x] the approve command reaches the ask through the commands topic; a fleet restored over
      an unanswered ask shows it `Interrupted`, the ask still visible

## Behavior

- [x] `spawn` returns at once with an id; `status(id)` is `Running` with step 0
- [x] `send(id, Pause)` holds the agent at its next tool call; `Resume` continues it;
      a paused agent's `elapsedMs` still grows (wall clock, not work)
- [x] `send(id, Stop)` lets the current tool finish and ends the run with phase `Done`
      when the model had a final answer, `Interrupted` otherwise; `Kill` ends it now with
      `Killed` and no further record from that agent reaches the topic
- [x] `Tell` is delivered as the next user turn, once, and appears in `transcript(id)`
- [x] a step past `budget.steps`, or a wall clock past `budget.wallMs`, ends the run with
      `Interrupted` and a partial result — never a hang
- [x] `delegate` from a parent with 10 steps left, whose child uses 4, leaves the parent 6;
      a child asked for more than the parent has is refused as a tool error naming both numbers
- [x] a child whose runner throws leaves the parent `Running`, and the parent's next turn
      carries the failure as a tool result — `Supervise.Stop` on the child, never on the parent
- [x] `all` lists children under their parent (`parent` set, and the parent's `children`
      contains the id), so a screen can draw the tree without a second query
- [x] `restore()` after two `Running` agents and a process exit: both are `Interrupted`, their
      transcripts are intact, and the next `spawn` gets an id greater than either
- [x] a `Turned` record is appended per turn as it happens, not at the end, so a kill loses
      at most the turn in flight
- [x] a scripted `Runner` drives the whole suite: no model, no gateway, no filesystem

## Out of scope

- The tools, the prompt, the gate, the sandbox: the consumer's `Runner`. The fleet
  never reads or writes a workspace.
- Cross-process fleets (an agent on another machine). An `ActorRef` over an
  okay-cluster channel is remote already; a fleet spanning stores is its own spec.
- Streaming tokens to a screen. `Stepped` is per tool call; token streaming stays at
  okay-llm.
- Who may spawn or steer. That is `okay-security`'s `Policy`, consulted by the caller
  (specs/identity-roster.md); the fleet trusts its caller.

## Design

- **Fleet is an actor; each agent is its child.** `Actor.spawn` for the fleet,
  `spawnChild` per agent with `Supervise.Stop`: a failed message is dropped and the
  child stops, which is exactly "a crashed child is a tool error, not a cascade".
  Delegation is `spawnChild` from the agent's own ref, so the tree is the actor tree.
- **The runner is a function, not a subclass.** `Runner.run` receives the inbox and
  the control poll as closures and calls them between tool calls; the fleet does not
  know what a step is. This is what keeps nadia's six tools out of okay and lets the
  test suite run on a scripted runner.
- **Budget deduction is arithmetic on `Spec`.** `delegate` reads the parent's
  remaining steps from its status, spawns the child with `min(asked, remaining)`, and
  on the child's finish debits the parent by the steps the child used. No shared
  counter, no lock beyond the fleet actor's own mailbox.
- **The topic is the truth.** `Status` is a fold; `Fleet` keeps the fold in memory
  and appends before it applies, so a crash between the two loses nothing that was
  acknowledged. This is the same shape as `TopicJournal` for tool calls; the two are
  separate topics so a run's tool journal (replayable) and its record (readable) do
  not share a key space.

## Decisions

- **`delegate` is a tool, not an operator command** — chosen because a hierarchy an
  operator assembles by hand is a list; and the consumer's spec (nadia `SPEC.md` §2)
  has a bar for a seventh tool that this is the one case to meet. Rejected: spawn
  only from the surface (the Rust nadia does this; fine for a batch runner, not a
  hierarchy).
- **Control is polled between tool calls, not preempted** — chosen because a tool
  call is the only point where the model's state is consistent; `Kill` is the one
  preemptive message and it says so in its name. Rejected: interrupting a
  completion mid-stream — the transcript would hold half a reply.
- **One topic per fleet, keyed by agent** — chosen so `restore()` is one fold.
  Rejected: a file per agent (the Rust nadia's `~/.nadia/.agents/<id>.json`) —
  restores, but cannot be replicated or shared without a second mechanism.

## Implementation lane

`agent-fleet` — okay-agent, additive (new file `Fleet.scala`, new suite
`TestFleet`); touches no existing signature. Consumer: `../nadia` `app/`.

## Results

Implemented 2026-09-29 (lane `agent-fleet`): `Fleet.scala`, `TestFleet` (7,
JVM), okay-agent now depends on okay-actor. Against the interface as first
written:

- **`Control` is `Fleet.Control`.** okay's core exports a `Control` (the
  final tagless interface of delimited control), and in a file that imports
  `okay.*` — every test does — that one outranks a package member defined in
  another file. Nesting it is the fix that needs no rename.
- **The runner sees a `Ctx`, not two closures.** `inbox()`, `checkpoint(step,
  tool)`, `turned(turn)`, `stepsLeft`: the checkpoint is where a pause waits
  (a channel the resume closes), where a stop or an exhausted budget is
  learned, and where the step is recorded — one call between tool calls.
- **`finish` runs inside the agent's fiber**, not in an `onComplete`. A parent
  awaiting a child joins the fiber, and a join can resolve before a
  completion callback runs; the first cut of the delegate test saw a child
  "running" after it had returned.
- **The step past the budget is the one refused** (`left < 0`), so a budget
  of two steps runs two.
- **Kill** ends the record at once (`Killed`, written) and cancels the fiber;
  a runner parked on its own channel is told `Kill` at its next checkpoint.

Records are plain JSON keyed by id (`spawned`, `phased`, `stepped`, `turned`,
`finished`); a `Turn` has its own small codec here, since `Turn` derives no
`Schema` and carries a raw `Json`.

**Events (lane `fleet-events`, 2026-09-29):** the record became a typed `Event`
with ONE decoder, `Fleet.event`, which `restore` now folds through as well —
so a feed and a restart cannot read the same bytes two ways. `Fleet.events(topic)`
is `Streams.tail` mapped through it; in-process `fleet.events()` is a channel per
listener, offered under the fleet's lock (never parked: a slow screen loses
events, the fleet loses nothing). `TestFleetEvents`, 3 tests.

**Commands (lane `fleet-commands`, 2026-09-29):** the control plane is a
topic, so any process that can append to the store can drive the fleet, and
the fleet's answer to a command it would not apply is a record on the feed the
sender already watches. The principal is a string the SERVICE checks (`allow`
over `okay.security.Roster`); the fleet trusts its caller, as before. A
command's `seq` is its offset: nothing to allocate, and the sender knows it
before the service does. `TestFleetCommands`, 2 tests.

**Approvals (lane `fleet-approvals`, 2026-09-29):** an ask is a record and a
parked channel; the answer is a control message like any other, so it comes
from the console, a chat or a browser alike. A stop or a kill declines an open
ask rather than leaving the runner parked forever — the case the first cut
missed until the test asked. `TestFleetApprovals`, 3 tests.
