# okay-match — two-sided matching over LLM-structured chats

One person tells an LLM what they can do; another tells it what they
need; the system stores both and finds the counterpart. The module is
the framework the LLM structures inside: effects to write against,
tools that mirror them, stores that are projections of a chat log.
The design is argued in [specs/match.md](../../specs/match.md); this
page is the how-to.

## The shape

- **Log-first.** Chats live on a persist topic (`ChatLog`); offsets
  are provenance; every store is a rebuildable projection. Extraction
  is idempotent by (profile, attribute, provenance), so replaying a
  log over a live store is a no-op and over a fresh one a rebuild.
- **Three effects are the whole backend contract** — `Registry`
  (attributes; search-before-create is its law), `Facts` (append-only
  with provenance; supersede with a reason), `Find` (hard filters
  exclude, similarity ranks). Two more compose above: `Rerank` (an
  LLM orders the top slice) and `Ident` (cross-channel identity).
- **Two stores today, any store tomorrow**: `MemoryMatch` (the cross
  reference) and `SqlMatch` (JVM, over the [Sql seam](okay-sql.md) —
  H2 in tests; Postgres via [okay-pg](okay-pg.md) is the same
  constructor with a different `Sql`).

## Wiring it

```scala
import okay.matching.*

// the durable store over any Sql driver
val m = SqlMatch(JdbcSql(conn))           // or PgSql — same line

// the LLM's tools: exactly what mcp.Server.serve takes
val serving = mcp.Server.serve(info, Tools.specs, Tools.table(m))
```

The seams a production site fills in, each a constructor parameter
with a working default:

| seam | default | production |
|---|---|---|
| `embed` | hashing (deterministic, offline) | a rag `Embed` provider |
| `policy` | `PlatformPolicy.open` | `afterMatch("phone", ...)` — the business gate |
| `hash`/`verifyHash` | identity | `okay.security.Password.hash`/`verify` |
| `fresh` | secure on JVM, `Entropy.weak` cross | `Crypto.randomBytes` via okay-security |
| `Handler[Rerank]` | `Rerank.lexical` | a few lines over okay-llm |

## The two gates, and what `withheld` means

A fact's owner sets `Vis` (`Public`/`Matched`/`Private`); the
platform sets a `Gate` per attribute (`Allow`/`AfterMatch`/
`Withhold`). Private facts do not even participate in matching.
An `AfterMatch` fact that matched is NAMED in `Ranked.withheld` —
the seeker learns THAT a phone exists, not what it is. Disclosure
of the value is the platform's transaction to grant.

## Cross-channel identity, safely

Only registry-flagged `identifying` attributes generate link
candidates, and a candidate is an attribute name plus a masked email
— never a value. The link is proven by a single-use expiring token
minted for the OLD profile and delivered through the OLD channel
(your transport's job); the stage-2 recovery secret is the fallback.
A confirmed link is an equivalence: both profiles stay, search folds
the class into one candidate, nothing is rewritten.

## Tests

`TestMatch` (cross logic, 12) and `TestSqlMatch` (durable parity,
log rebuild, registry migration, restart survival, 6) — one test per
spec behavior checkbox, including the scripted two-side scenario
driven entirely through the tools.
