## okay2-dollar - λ$'s dollar in the okay2 machine, and stacked shift0/dollar; two tests both cores lacked

- okay2's `Delim` has `dollar(p)(ret)(body)` as the Scala 3 core has it:
  - a `Ret` segment carries `ret`;
  - the cut has two shapes, `Plain | AtRet`, with `P0` a type member,
    so no new cast is needed;
  - a control-capture to a dollar is refused by name.
- TestDollar's twin holds every value of the Scala 3 suite, 100 000
  nested dollars included.
- Found: no test in either core told `ret $ E[v]` from
  `push(E[v] >>= ret)`, and a mutant `reify` passed all 13. Two tests
  now tell them apart in BOTH cores: `ret` runs outside the re-installed
  dollar, and a capture to an outer prompt keeps the dollar.
- Stacked: `Has.Below` by induction (`Aux`), which also removed `Has`'s
  two casts. `stack.shift0(p).apply { below => k => … }` types the body
  under the stack below `p`, and a shift to `p` from it is a compile
  error (TestStackedShift0). `stack.dollar(ret) { … }` is there too.
- Filed, not built: okay2-layered, okay2-lexical (the latter needs
  `dollarResumed` first).
