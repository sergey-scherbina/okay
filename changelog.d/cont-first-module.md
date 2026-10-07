## the first module on the machine: okay-cache, with Async under it

Lane cont-first-module (specs/freer-min.md, stage 49). `AsyncCont` puts
Async on the machine's `A ! R`: `async`/`await` as row-polymorphic `Op`s,
an answering `blocking` handler, a callback drive `runAsync`, and
`toClassic`/`fromClassic` bridges to the classic tree. okay-cache is
written on it: single operations are `Op[Async, X]`, multi-step programs
(`getOrLoad`, `WriteThrough.write`, `Invalidations.drain`) take their row;
the Scala 2 facade crosses by the bridges. What it found is
backlog.d/okay-core/cont-first-module-findings.md. Also: okay-platform's
JMH sources compile again (`State` was ambiguous between `okay.freer.*`
and JMH's annotations).
