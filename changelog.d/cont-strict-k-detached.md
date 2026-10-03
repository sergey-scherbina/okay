## cont-strict-k-detached - a capture reuses a delimiter that is already over nothing

cont-strict-k, the first step on top of cont-run-prompt (operator:
"Продолжай 2", i.e. continue with option 2).

**What it was.** A capture's `k` is the live segment over a COPY of its
delimiter, `Dollar(p, ret, Done)`. In a strict `k`'s nested run, and at
a resumed `k`'s head, the delimiter the capture finds is already such a
copy. So every capture built an identical node.

**The fix.** `nearestAt` takes the delimiter itself, and `detached(d)`
answers `d` when its `below` is `Done`. Matching `Done` makes the
delimiter's indexes meet by the GADT, so there is no cast. The old
`nearest` is folded into it.

**Measured,** alternated against master, history.d
`cont-strict-k-detached`:

| lane | ratio | bytes |
|---|---|---|
| statePara | 0.95x / 0.94x | -48 KB (24 B a capture) |
| contAnswer | 0.93x / 0.93x | -24 KB |
| fib100 | 0.98x / 0.98x | -2.4 KB |

The first cut covered only the resumed-`k` arm. It moved contAnswer but
not statePara, whose captures meet the delimiter right under the live
segment; that is recorded too.

**Next:** the `Next` carrier, backlog cont-strict-k.

Tests: the machine's suites (TestDelimited*, TestHandleFrames*,
TestKont, TestShift, TestContMacro, TestPState*).
