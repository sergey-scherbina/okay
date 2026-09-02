# okay-subscription

> Gate a resource behind a paid period (specs/subscription.md): free
> for the subject's first calendar month, then only a period actually
> paid keeps it visible — unpaid is GATED, never deleted. Extracted
> 2026-09-02 from `okay-demo`, a pure move: the logic already took a
> bare `String` id and had no dependency on `MatchStore`/`ChatLog`.

Depends on: `okay-agent` (`ToolSpec`, for the tool contract only).
JVM-only.

## Guide

**The check.** `Subscription.subscribed(uuid, now = Period.now())` —
`true` for the subject's join period (anchored LAZILY: the first time
ANY function here sees a `uuid`, that becomes its join period, so a
subject this module has never touched is never surprise-gated) or any
period it paid for. `Period(y, m)` is a calendar month;
`Period.of(epochMillis)`/`Period.now()` build one.

**Paying.** `Subscription.pay(uuid, now)` takes effect immediately —
the very next `subscribed` check, same turn even, sees it.

**Telling the subject.** `Subscription.subscriptionNotice(uuid, now)`
is `None` while subscribed, else `Some(...)` with a message naming
the tool that pays.

**As an LLM tool.** `Subscription.paySpec: ToolSpec` is the static
`subscription_pay` contract — pure data, so every consumer wiring it
into an agent's tool table gets the identical schema instead of
re-deriving it. Wiring it in, and what side effects ride the actual
payment action, stay per-application.

**Testing "a month passed".** `subscribed` only anchors `joined` on a
subject's first-ever check, so production code never needs to touch
it — a test that wants a subject already past its free month calls
`Subscription.backdateJoin(uuid, period)` to move the anchor back in
time before asserting.

| | |
|---|---|
| `Period(y, m)`, `.key`; `Period.of`/`.now()` | the calendar-month unit |
| `Subscription.subscribed(uuid, now)` | free-period or paid-this-period |
| `Subscription.pay(uuid, now)` | immediate |
| `Subscription.subscriptionNotice(uuid, now)` | the user-facing nudge, or `None` |
| `Subscription.paySpec` | the `subscription_pay` tool contract |
| `Subscription.backdateJoin(uuid, period)` | test seam: force the join anchor |
