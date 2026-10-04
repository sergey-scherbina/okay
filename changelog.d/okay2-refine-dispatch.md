## okay2-refine-dispatch - Dispatch in okay2-refine: the routing table as a Scala 2 match

specs/refine-dispatch.md stage 4. `okay2.refine.Dispatch[A, B](pattern)`
is okay-refine's `Dispatch` on the Scala 2 core: lanes are typed handles
named by path (`lane[Swap]("rates/swaps/eur")`), the table is the user's
own `match` returning a `To` made only by a lane or `unrouted(why)`, a
sub-table is a method, `table(b, by)` routes on the verdict's path, and
`split` runs it over every `Routable` carrier (Vector, Bulk, Source).
A table that throws rejects that one document, named. `Router.Routed`
gains `under(prefix)`, a subtree's count.

Scala 2 differences: a lane's test is a `ClassTag` (checks the class;
`lane[Double]` works) instead of a `TypeTest`; `Split` holds the
`Routable` instead of polymorphic function values. Over a sealed trait a
forgotten case is scalac's "match may not be exhaustive", an error in
the okay2 build — verified by a probe, not pinnable (munit's
`compileErrors` sees the typer only).

Tests: TestDispatch 6 (JVM), TestRefineMatch +1 (JVM/JS/Native).
Docs: docs/okay2.md section on okay2-refine. Closes the sprint item
okay2-refine-dispatch.
