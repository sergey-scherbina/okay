## refine-dispatch - hierarchical routing written as a Scala match (stage 1)

okay-refine gains `Dispatch` (specs/refine-dispatch.md): the routing
table is the user's own `match` over what the pattern recognised — lanes
declared by path with a type (`lane[Swap]("rates/swaps/eur")`), a case
delivering with `lane(x)` (compiles only for the lane's type) or
`unrouted(why)`, sub-tables as methods, `table(b, by)` to route on the
verdict's path; `split` over every `Routable` carrier; `Routed.under` for
a subtree's count; a table that throws costs one document, named. Over a
sealed type the compiler's exhaustiveness check makes a forgotten case a
warning, so a forgetful table does not build (verified by a probe).
Kafka topics, Flink streaming side outputs and okay2 are queued as stages
2–4.
