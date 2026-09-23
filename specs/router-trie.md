# router-trie — dispatch by index, decision by entry

## Overview

`Router.routes` was first-match over a `Vector[Entry]`: for every
request, every entry's `matches` closure ran until one answered —
`entries.iterator.map(answer).collectFirst`. Each `matches` splits and
percent-decodes the request's path again (`Route.segmentsOf`), checks
the method, the segment count and the literals. Filed from the staging
survey (2026-09-22) as the fourth of four with the trigger "a router
with dozens of routes"; the operator waived the trigger (2026-09-23,
"потом тоже"), so this spec builds it and lets the number say at what
table size it pays. The design keeps the invariant every earlier
router lane rests on — **the entry decides**: headers (route-headers),
queries, security (`matches` vs `run`, fail-closed without a verifier),
first-match order — by making the index answer only WHO IS ASKED, in
declaration order, never what they answer.

## Interface

```scala
// okay-http, Route.scala — no public change
final class Router private (val entries: Vector[Router.Entry]):
  def routes: PartialFunction[Request, Response ! Async]     // same type, same answers
  private lazy val index: Router.Index                      // built once per table

object Router:
  private[http] final class Index(entries: Vector[Entry]):
    /** the entries this request can possibly match, in declaration order */
    def candidates(r: Request): Vector[Int]
```

`Index`: method → segment count → a trie over the template's literal
segments, a `{param}` a wildcard branch; a leaf holds the indices of
the entries declared at that shape, in order. `candidates` walks the
request's decoded segments down the literal and the wildcard branch at
each depth and answers the union of the leaves, sorted. An entry whose
template `segmentsOf` refuses is a candidate for every request.

## Behavior

- [x] AGREEMENT with the scan on generated tables: for random tables
      (1–40 entries, random methods, templates mixing literals and
      params, overlapping shapes, `Route.root`) and random requests
      (hits, near-misses on one literal, wrong method, wrong count, a
      malformed escape), `routes.isDefinedAt` and the entry that
      answers (`applyOrElse`, handlers answering distinct statuses)
      equal a reference first-match scan over `entries`.
- [x] `candidates(r)` ⊇ the entries whose `matches(r)` holds, and is
      sorted ascending (declaration order).
- [x] Headers, queries and security unchanged: `TestRouteHeaders`,
      `TestRouterOut`, the secured-route tests and every http suite
      pass as they are.
- [x] `++` of two routers dispatches in the concatenated order.
- [x] MEASURED (compare `RouterBenchmark`, quiet alternated pairs):
      tables of 3, 30 and 300 literal-discriminated routes, a request
      hitting the LAST route and a miss, `routes.applyOrElse` — the
      scan (master) against the index; the table size at which the
      index pays, and the price at 3. Rows `rt-*`.
- [x] Docs: the http guide's router paragraph names the index and its
      invariant; typepedia `Router` entry.

## Out of scope

- Indexing queries or headers: they are the entry's decision, and a
  request's query is not part of the path template.
- A regex or prefix route: the DSL has none; a segment is a literal
  or a param.

## Design

- **The index is a filter, never a decider.** Every entry it excludes
  would have failed its own `unapply` (method, count or a literal);
  every entry it keeps is asked exactly what the scan asked. So the
  proof of agreement is the proof that `unapply` needs the method, the
  count and the literals — which is its first three lines.
- **Templates are read back from `Entry.path`** (`/a/{id}/b`) with the
  same `segmentsOf` the request goes through, so literal and request
  compare decoded to decoded. A literal that happens to look like
  `{x}` becomes a wildcard — a superset, still correct.
- **Built once, lazily, per `Router`** — a server keeps one
  `PartialFunction`; a test calling `r.routes(req)` per request pays
  the build once per `Router` value, not per call.
- **A malformed request path answers `always`** (the unreadable
  templates), as the scan answered nothing for it: `unapply` refuses
  the same escape.

## Decisions

- **A trie on segments, not a hash on the whole path** — params make
  the whole path unknown; segments are the unit `unapply` compares.
- **Sorted union rather than a merged walk** — a request reaches at
  most 2^depth leaves and the indices are ints; sorting a handful
  beats a merge that must carry order through every branch.

## Results

**2026-09-23** (compare `RouterBenchmark`, `-prof gc`, one quiet pair
— the scan at 12:43 and the index at 14:39, both at load 3–4, on a box
that carried sibling gates at load 60–120 between; rows `rt-*`).
Literal-discriminated routes `/svc{i}/items/{id}`, a request hitting
the LAST route, `routes.applyOrElse`, the handler's program built and
not run:

| table | scan (master) | index | ratio |
|---|---|---|---|
| 3 routes, hit last | 356 ns / 2 400 B | 342 ns / 2 144 B | 0.96 |
| 30 routes, hit last | 3 798 ns / 21 408 B | 297 ns / 2 084 B | 12.8x |
| 300 routes, hit last | 37 091 ns / 233 076 B | 275 ns / 2 024 B | 135x |
| 300 routes, a miss | 40 745 ns / 240 208 B | 79 ns / 760 B | 518x |
| 300 routes, `isDefinedAt` | 28 767 ns / 230 544 B | 276 ns / 1 944 B | 104x |

The scan paid ~780 B and ~120 ns PER ENTRY TRIED — every `matches`
split and decoded the path again — so the index's price at three
routes is nothing (the path is split once) and its answer is a
constant, ~275 ns and 2 KB, whatever the table holds; a miss is
cheaper still, the trie walk with no candidate to ask. Laws:
`TestRouterIndex` 3/3 (300 tables × 50 requests against the scan),
`TestRouteHeaders`, `TestRouterOut` unchanged; the http family gate
green. The first `after` attempt died in JMH's bytecode generator
(`NoClassDefFoundError: okay/Free$Pure`) after the lane was rebased
over core changes — the worktree's incremental classes were
inconsistent; cold targets fixed it.
