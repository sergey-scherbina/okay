## handle-frames-catch — the last handler folds off the host stack: catch frames, Resource, Logic

The three handlers specs/handle-frames.md had left as folds — `Throws.rows`
and `Resource.run`, each running its steps under a JVM `try`, and
`Logic.msplit`'s search — now nest on a bounded host stack. 100 000
nested tries, scopes, cuts and splits run on JVM and JS (red first on
master: StackOverflowError; the old Throws fold also silently answered a
wrong value, because it caught its own overflow).

- The machine has a `try` of its own: `Cont0.Catching` frames. Once one
  exists, user code steps run under a guard that turns a throw into a
  value, and the loop hands it to the nearest catch frame below, dropping
  the frames above. A throw from a handler goes on to the frames beneath
  it; nothing catching, the same exception object is thrown on.
- `Throws.rows`: a depth-bounded fold, and a catch frame on a machine.
- `Resource.run`: a state frame that is also a catch frame. It releases
  at the end, on a throw (then throws on), and before a `Final`
  operation. An abort through the scope now answers; the old fold threw
  `ClassCastException`.
- `Logic.msplit` is a value, no longer run when called. Its fold is
  depth-bounded, and on a machine it is a search frame
  (`HandleFrames.handling`): the alternatives not yet run are handed out
  as `pending` programs.

Commits: 47a2cf6fe (catch frames, Throws), 835c5941f (Resource),
958fabd03 (Logic). Spec: specs/handle-frames.md, "Catch frames, Resource,
the search".

Cost, measured with alternating arms against the merge-base (history.d,
handle-frames-catch): ShiftBenchmark.shift0_seq 1.01x, stateSmall 1.01x,
handlePrebuilt 0.98x — the guard is one volatile read until a catch frame
exists, inside the noise.
