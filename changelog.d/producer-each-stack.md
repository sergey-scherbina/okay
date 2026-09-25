## producer-each-stack - okay's `Producer.each` is stack-safe, and now pinned

okay2's `Producer.each` threw StackOverflowError at 200 000 back-to-back
productions (okay2-split-at-rest), and okay's has the same text. okay
turned out to be SAFE, and the difference is `split`. In okay `split`
is `inline`, so the produced arm's `each(k(w))(f)` lands in tail
position and scalac turns it into a loop (`goto 0` in the bytecode of
`Producer$.each`). In okay2 `split` takes closures, and the call inside
one cannot become a jump.

No code change. A test in TestFoldUntil (200 000 productions, no G
operation between them to reset the stack) guards the property, and a
comment on `each` says why it holds.
