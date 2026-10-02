## threaded-zoom — the census's nesting sites off the strict-k bridge

Operator ask, 2026-10-02 (sprint cont-js-depth stage 3a). `PState.Threaded`
gains `zoomWith` — a typestate program over a part, run over the whole as
ONE operation, the outer program kept on a type-aligned waiting stack (no
cast), the four-parameter lens's type change kept — and okay-optics'
`PState.Threaded.zoom(lens)`. `PWizard` runs on data (`Get`/`Put`/`Show`,
a loop to `Machine`), names and syntax unchanged. A million nested zooms
on JVM (128 KB too), Scala.js and Native; a million wizard steps on
128 KB. The shift road's `PState.zoom` stays as the bridge.
specs/cont-js-depth.md.
