## cont-stack-cold-bytes-per-level: a cold level is 2.5 KB on `Delimited`; both rooms fixed

- `TestColdRoom` (new; each case its own cold JVM, `ColdRoomMain`). An answer-using opaque body overflowed
  a 1 MB default stack at 367 levels, under a first room of 436, with no caller frames at all. The 1 GB
  room overflowed at ~426 000 cold levels under a count of 524 288. All three cases were red.
- `StackSwitch.coldBytesPerLevel` 1 200 → 2 600 B (measured ~2 520 B a level, interpreted and in a
  default JVM's first run), on JVM and Native. The 1 GB room is three quarters of the stack over it
  (309 000). Price: 1.09–1.14x at a million levels, from more switches (history.d cont-stack-cold-bytes).
- The operator's rule, the host stack only where nothing else can work, is written into
  specs/cont-stack.md. The next two lanes are backlog cont-program-leaf-always and cont-stack-exact-first.
