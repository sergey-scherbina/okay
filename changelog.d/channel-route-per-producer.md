## channel-route-per-producer — feeds keep their order wherever they run; the foreign-resume handoff is back

A partitioned channel (`AdaptiveFifo`) kept each producer's order by
giving it a part, and knew a producer by its THREAD. On `own`/`adaptive`
a fiber is resumed wherever it was woken, so sending a woken producer
home (f50932cbe) broke `Channel.merge`'s per-side order and was withdrawn
(3f1321210). Now `Buffer.claimRoute` / `Channel.claimRoute`,
`offerFrom`, `sendFrom` let a producer name its part once, and the
library's feeds do through a send-only view (`Channel.routed`):
`Channel.merge`, the shared chunked merge (a side's feed and flusher on
one route) and a windowed chunked side. `Growing` has no routes (-1), so
a route never becomes its adopted part. `TestChannelRoute` (the control
reorders through `offer`, the route keeps order); TestMergeOrder green
400/400 even under the forkLong handoff that had turned it red at round
9. With the `fork` handoff back, every lane beats Loom on the default:
merge cap 7 / 64 91.1 / 62.9 us (Loom 310.8 / 80.8), zip cap 7 / 64
1400 / 370 (Loom 2343 / 753), okayChunked 180 (Loom 196).
specs/channel-route-per-producer.md.
