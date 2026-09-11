# okay-leads

> What the chat learned from one request, as a countable thing
> (specs/leads.md): the fields a provider would pay for, none of the
> data that would make passing them on illegal, and the demand they add
> up to — every figure one `Aggregator`, one pass over the ledger.

Depends on: `okay` (JVM), `okay-intent`. JVM-only — it writes files.

## Guide

**Two lines in the chat.** `Leads.watch(ledger, salt)(sessionId, text)`
records a turn and returns the `Lead` it wrote;
`Leads.outcome(ledger, lead, Matched, contact)` appends what came of
it. Nothing else is needed, and no model is called: the category comes
from cues in Polish, Russian and English, the budget and the date from
`okay-intent`'s own parsers, and what none of them read stays
`Unknown`.

**What is deliberately absent.** The message. `session` is a salted
hash, so two requests from one conversation can be told apart from two
people's and nobody downstream can walk it back; `contact` exists only
when somebody asked to be contacted, and `Demand.deliverable` is the
only door that returns it.

**The ledger is a CSV.** It survives a restart, a spreadsheet opens it,
and `Bulk.csv` aggregates it on one JVM or on a cluster unchanged
(docs/modules/okay-spark.md). Append-only: an outcome changes later,
and `Demand.latest` takes the last row per request.

## Tutorial

```scala
val ledger = Ledger.at("leads.csv")
val salt = conf.secret("OKAY_LEADS_SALT")

// in the chat, per turn:
val lead = Leads.watch(ledger, salt)(sessionId, userMessage)
// later, when something came of it:
Leads.outcome(ledger, lead, Lead.Outcome.Matched, contact = Some("tg:@ann"))

// what to say to somebody who might pay for it:
println(Demand.report(ledger.read()).show)
```

```
sbt "okayLeads/runMain okay.leads.Report leads.csv"
```

## API reference

| member | signature | meaning |
|---|---|---|
| `Capture.lead` | `(message, sessionId, salt, at) => Lead` | cues + parsers, no model |
| `Capture.refine` | `(lead, category?, city?, budget?, urgency?) => Lead` | fill holes only |
| `Leads.watch` | `(ledger, salt)(sessionId, message) => Lead` | the whole integration |
| `Leads.outcome` | `(ledger, lead, outcome, contact?) => Lead` | appended, history kept |
| `Ledger.append/read/readEither` | | a row, all rows, all rows and every failure |
| `Demand.report` | `Iterable[Lead] => Report` | every figure, one pass |
| `Report.rolling` | `days => Vector[(date, Double)]` | the Group's window |
| `Demand.medianBudgets` | `Iterable[Lead] => Map[Category, (Long, Double)]` | t-digest, with the count |
| `Demand.deliverable` | `Iterable[Lead] => Vector[Lead]` | consent, and still live |

## Gotchas

- The salt is a deployment secret. Rotating it is the "forget
  everybody" button — old rows stop being joinable to new ones, which
  is a feature and a thing to do on purpose rather than by accident.
- `Demand.latest` keys on `(session, at)`, so a correction must carry
  the ORIGINAL `at` (which `Leads.outcome` does by copying the lead).
- The cue tables are the product's own vocabulary and will need
  extending per market; a cue is a word that means somebody wants that
  kind of thing, not a word that merely occurs in such requests.
