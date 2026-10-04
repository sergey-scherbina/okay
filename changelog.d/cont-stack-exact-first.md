## cont-stack-exact-first: the host stack READ where it can be, counted where it cannot

- At the end of every room, `StackSwitch.more` reads the stack pointer and the thread's bounds through
  `StackRoom` (JDK 22+ with native access) and grants half of what is left over a 64 KB margin. The first
  room there is 32 levels. Elsewhere the count stays (JDK 17–21, no native access, Native), and
  `-Dokay.cont.read=false` turns reading off. One stack holds at most `levelsPerStack` levels either way.
- TestColdRoom: a 256 KB thread with its caller 200 frames deep, which the count overflows and the reader
  holds; a 16 MB thread holds 2 000 cold levels with no second thread. TestContStack asserts the read road
  where it is in play.
- Cost against the count: 1.04x at 1 000 levels, 1.03x at 100 000, statePara 1.00x. At a million levels
  it is 2.31x, a GC imbalance that is still open: backlog cont-stack-exact-million (history.d
  cont-stack-exact-first).
