## cont-stack-8mb-flake — TestContStack asserts what the reader reports, not what the JIT decides

The flake ("8 MB switched 1 times, 2 MB 1", a full `affected` gate at
load 25–40, green in isolation) is the JIT's state, proven rather than
guessed: a Cont level takes 1 858 B under `-Xint`, 1 223 B under C1
only, ~288 B warm C2 (a probe reading the stack pointer at depth).
20 000 levels fit 8 MB only warm, so "8 MB switches fewer times than
2 MB" read 0 < 1 once C2 had compiled the path and 1 vs 1 while it had
not — and under load the compiler threads fall behind. `-Xint` and
`-XX:TieredStopAtLevel=1` make that assertion red every time.
cont-stack-test-8mb (2958d402d, the evening before) had already swapped
it for "4 000 levels fit 8 MB", which holds, but with 4% to spare
interpreted (7.4 of ~7.7 MB) and without proving the 8 MB was seen
(4 000 warm levels fit 2 MB too).

- The 8 MB test asserts the reader's bounds first: `top − floor` on an
  8 MB thread is over 7 MB, on a 2 MB thread over 1 MB and at most 2 —
  no JIT state moves them. Then 3 500 levels on 8 MB switch zero times
  (6.5 MB interpreted, a megabyte spare) and 20 000 on 2 MB switch
  (5.8 MB even warm).
- The same flake one test up: "a program the stack can hold switches
  ZERO times" ran 1 000 levels on 2 MB, 1.86 MB interpreted of ~1.66 MB
  usable — red under `-Xint` every time. Now 600 levels (1.1 MB).
- TestContStack 9/9 green under default, `-Xint` and C1-only.
- specs/cont-stack.md Results: bytes a level by JIT state. Found on the
  way and filed: the count road's first room divides by a cold
  constant (1 200 B) under the measured interpreted level (backlog
  cont-stack-cold-bytes-per-level).
