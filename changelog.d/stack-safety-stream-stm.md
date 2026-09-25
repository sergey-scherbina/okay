## stack-safety-stream-stm - a poller that flushes after every empty poll no longer descends per poll

Stack-safety stage 3, streams and STM, both cores. One real hole:
`Channel.feedFlushing` — the chunked feed for a producer that marks its
own boundaries — continues directly into the next step whenever a step
sends nothing. A told element does that at most `Source.ChunkSize` times
before a send's `flatMap` breaks the descent; an EMPTY flush sends
nothing, so a producer that flushes after every empty poll descended
once per poll, and 200 000 of them were a StackOverflowError
(`TestFlushDepth`, red first in okay-stream and okay2-stream). The feed
counts its direct steps now, and every `FlushBudget` (256) of them goes
through a `pure(()).flatMap` node — the budget idiom `Pipe.PullBudget`
already is.

The other 36 rows of the two modules carry their bounds, mirrored across
the cores: the Pipe/Take loops have `PullBudget`; STM's `perform` and
`runWithLog` nest per `orElse` in the transaction's own text; its
`attempt`s retry through a thunk that `park` stores and a waker runs; the
Sim handler reaches a `Sim.yieldNow` bind at the first operation;
`Pipeline` and `Tables` walk a plan the program built by applying
operators. No row was paid: a budgeted descent is still a recursion, and
its row says what bounds it.
