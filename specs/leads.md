# Leads — what the chat learned, as a countable thing

## Overview
The prototype chat finds people jobs, housing, services, goods and
things to do. Before any of that can be charged for, one question has
to be answerable: **what did people ask for, where, how much were they
willing to pay, and how soon.** A conversation that was not reduced to
fields is not evidence of anything, and a lead that cannot be counted
cannot be sold. This module is that reduction — deliberately the
cheapest possible one, because a measurement that costs a token per
turn is a measurement that stops when the bill arrives.

## Interface
```scala
final case class Lead(at: Instant, session: String,           // a SALTED HASH, not a person
                      category: Category, city: Option[String],
                      budget: Option[Amount], urgency: Urgency,
                      outcome: Outcome, contact: Option[String] = None)

Capture.lead(message, sessionId, salt): Lead     // cues + okay-intent's parsers, no model
Capture.refine(lead, category = …, city = …)      // a later tier fills HOLES, never overwrites
Leads.watch(ledger, salt)(sessionId, message)     // the whole chat integration
Leads.outcome(ledger, lead, Matched, contact)     // appended: the ledger keeps history
Demand.report(leads): Report                      // every figure in ONE pass
Demand.deliverable(leads): Vector[Lead]           // only what may be passed on
```

## Decisions
- **The message is not stored.** A ledger of what people typed is
  personal data the moment somebody writes a phone number into a chat,
  and it is worth nothing extra: the fields are what a buyer reads.
  `session` is `SHA-256(salt ++ sessionId)` truncated — enough to tell
  two conversations apart, not enough to identify one; rotating the
  salt forgets everybody.
- **Consent is a field, not a policy document.** `contact` exists only
  when a person asked to be contacted, and `Demand.deliverable` is the
  only door that returns rows with one. Without it a lead is a
  statistic, which is what the ledger is by default.
- **Nothing is invented.** A category is assigned when a cue fires, a
  city when a city is named, a budget when an amount is written down —
  otherwise `Unknown`/`None`. A guessed field is a number somebody
  will quote back at you. `refine` lets a later tier (the model that is
  already answering) fill a hole and forbids it from overwriting what a
  person wrote.
- **No model in the capture path.** Cues in three languages (Polish,
  Russian, English — the city this was written for) plus
  `okay-intent`'s `Amount` and `Temporal`, which already read "do 3 tys.
  zł" and "od 15 października" and refuse what they cannot. Urgency
  falls back to a DATE when no urgent word appears: "od 15
  października" is an urgency.
- **A CSV file, appended to.** It survives a restart, a spreadsheet
  opens it (which is how the first buyer will read it), and `Bulk.csv`
  aggregates it — on one JVM now, on a cluster the day the file becomes
  a folder, unchanged (specs/bulk.md). Append-only because an outcome
  changes later and the history of how long a match took is the second
  thing a buyer asks about; `Demand.latest` takes the last row per
  (session, at).
- **Every figure is one `Aggregator`, zipped.** The report reads the
  ledger ONCE whether it answers three questions or thirty
  (specs/aggregators.md), and the rolling window is the Group's —
  a day in, the day that aged out subtracted. Budget quantiles are a
  t-digest rather than a sorted list: holding every value to answer
  "who are these people at 1500?" is the one thing a ledger must not
  start doing.

## Behavior
- [x] a cue fires per category in each of the three languages; nothing
      else is classified, no city is guessed, no budget invented
- [x] urgency reads words first and a date second
- [x] `refine` fills holes and refuses to overwrite
- [x] the pseudonym is stable, salt-dependent, and does not contain the
      session id
- [x] a lead survives the CSV round trip including a comma and a quote
      inside a field; an unreadable row is reported, not dropped
- [x] the report: total, people (distinct sessions), by category, by
      city (unknown counted as unknown), by (category, city),
      deliverable count, conversion; a correction changes the outcome
      and not the count
- [x] median budgets per category from the digest, absent where nobody
      wrote one
- [x] rolling demand over days, by the Group's window

## Out of scope
- charging for anything: this is the ledger the pricing conversation
  starts from, not a billing system
- a UI: `okay.leads.Report` prints five lines, which is what the first
  customer conversation actually needs
- storing the message, ever
