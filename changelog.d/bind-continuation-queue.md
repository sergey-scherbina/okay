## bind-continuation-queue — the continuation queue prototyped and dropped

ORDER 3 of the map-cost plan: `resume` built a queue node (`ThenK`,
"Reflection without remorse"'s type-aligned sequence) instead of a
closure when it rotated, and ran a `map`'s function in place. The first
cut overflowed on Delim's shift under 20 000 pending maps (it called the
next queue node instead of continuing the loop — map fusion's old
failure); fixed, the core suite green. Measured against master: the
map-heavy lanes unchanged (`rowFoldM`, `stateFoldM`: the library's own
builders had lost the shape to one-bind-hot-steps and op-map-constructors
that morning), `nestedSW` 0.83-0.85x and -13% bytes, the map-free
`handlePrebuilt`/`relayPrebuilt` 1.01-1.03x slower. The item's bar failed
on both sides; the code is not landed. specs/map-fusion.md, history
`…-bind-continuation-queue.tsv`.
