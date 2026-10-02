## refine-route - one stream of documents in, a stream per kind out

`Router(pattern)`: `.route[X](channel)` by type (exact for unions, via
`TypeTest`), `.route { case … }(channel)` by pattern matching,
`.byName(name)(channel)` by the pattern that took the document,
`.routeAs`, `.tap`, `.otherwise(Rejected)`, `.run(source)` answering
`Routed` counts and closing every channel once (failing them on an input
failure); `decide(a)` for one document; `Refine.routed(key)` as the
synchronous `Stage` twin. `Unclear` documents are rejected, never routed;
nothing is dropped silently. Found by the first run: a union's
`ClassTag` is its least upper bound, so `route[Swap | Cds]` on a
ClassTag took `Fx` as well — routes test with `TypeTest`. TestRouter (9),
docs/modules/okay-refine.md "Routing".
