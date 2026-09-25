## stack-safety-okay2-core - okay2's loops checked, and its core's stack recursions become loops (specs/stack-safety.md stages 0b, 1b)

- Stage 0b: 17 okay2 methods scalac already compiled to a loop carry
  `@tailrec` now. For 11 of them the call from inside a `flatMap`, a
  `Cont.defer`, or fs2's by-name `++` goes through a one-line `again`
  wrapper.
- Stage 1b: `Aggregator.topK`, `Delim.cut` and `Static.foldMap`
  overflowed a 128 KB stack at a few thousand levels, the same three
  as in okay. Each test ran RED first
  (src/test/scala-jvm/okay2/TestStackSafetyCore). Each is a loop now.
  `Delim.cut`'s type-aligned frame stack is written the Scala 2 way:
  each step is a method on its node, and the walk's changing type is
  an existential of `Walk`, with no cast.

Found on the way, in BOTH cores: `Cont` overflows at 20 000 shifts in a
row whose bodies call their continuation. The operator chose a
trampoline inside, measured first: stage 1c.
