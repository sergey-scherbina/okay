## layered-stacked - layered reflection, stages 1-2: `reify = η $ e`, and stacked layers

specs/layered-reflection.md stages 1 and 2 (sprint item
monadic-reflection-stacked, now closed).

- `Layered.reify` is `Delim.dollar(p)(η)(body)`, λ$'s reading of
  Filinski's reify. It means the same as stage 0's `push(e.map(η))`
  and allocates 40 B less per resumption. Time was not measured
  cleanly (load 60-108), so backlog `layered-reify-time` owes it.
- `Layered.Stacked`: a layer is a stacked `dollar` with `η` as its
  return function, and the capability is that dollar's `In`.
  `m.reflect(layer)` is a stacked `shift0`, so a layer kept past its
  `reify` does not compile. TestLayeredStacked (3) covers both layer
  orders and the escape.
- docs/direct-style.md: the capability-escape paragraph now points at
  `Layered.Stacked`.
