## condition-nested-frames - okay's Condition runs frames nested to any depth: no host frame each, not quadratic

Operator: "condition-nested-frames - исправь" (fix it). Found while
porting `Condition` to okay2 (okay2-condition-repair).

**Two costs in `Condition.run`** (okay-direct), both met by a recursive
program that opens a `within` per level:
- a nested frame's region was entered by a direct call of `loop`, one
  host frame each, so 100 000 nested frames overflowed (red first:
  StackOverflowError);
- each `loop` built the menu's names up front, which is O(depth) a frame
  and quadratic in all.

**Now** the region is deferred into the program the loop answers
(`Free.delay`), and the names are a `lazy val`, read only by a signal or
an invoke.

The comment that said frames "nest by lexical depth, which is bounded"
was the false assumption, and now says what is true. Its two
stack-safety inventory rows, which gave a reason that was not this
code's, are gone with the recursion (`recscan-check.sh --write`). okay2's
twin, which already did both, says so in its doc.

Tests: TestCondition gains the hundred-thousand-frame test, which okay2
has too. TestCondition, TestConditionTyped and TestConditionDirect pass
(22).
