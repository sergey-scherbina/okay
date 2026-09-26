## foreign-pipe-hello-timeout — a silent worker over pipes is refused, not waited for (2026-09-26)

A worker started over pipes that never said hello — a container that never
came up, seen twice today under load — left its opener in a read for ever;
only the gate's stall watchdog ended it. `WireLink.pipes` now reads the hello
on a thread of its own with a limit (5 minutes by default, `helloMillis`: a
container start with ~19 in flight was measured past a minute), and a
process that says nothing in time is refused by name and stopped — as TCP
already was. TestPipeHello; docs/one-language.md, "When the far side fails".
