## r-arrow - frames cross to R as Arrow, okay-py's twin

specs/r.md ("r-arrow"): the JVM half already existed from py-arrow; what
was missing was R's own side of a negotiation `okay.codec.WireNegotiation`
already had generic — it needed no change at all.

- `RArrowFrames` (okay-r): an `RFrame` to `okay.arrow.Table` and back,
  over R's four atomic types (logical, integer, double, character) plus
  NA — `ArrowFrames`'s twin, minus Python's int32-to-int64 widening,
  since R has only one integer width to begin with. A mixed or
  uncarryable column (raw, a nested list, a held object) is a `Left`
  naming the column.
- `RSubprocess` takes an `okay.codec.FrameFormat` given, negotiates it in
  the same handshake as format/compression, and `REval.Frame` sends the
  table as one Arrow IPC stream with the request's header in the
  schema's metadata — Arrow needs a FRAMED wire, so it configures even
  `json/none` when nothing else would have. `wire` gains a `+arrow`
  suffix, `arrowFrames` counts which road a frame took. `ShimVersion` to
  8. `WireChoice.named(frames = "arrow")` reaches `startWithWire` too.
- Tests: `TestRArrowFrames` (7, default gate, no R needed) proves the
  column mapping. `TestRArrow` (Live, okay-py's `TestArrowFrames` twin)
  proves the wire end to end — skipped everywhere this was written, for
  want of an R with the `arrow` package.
- shim.R's own Arrow calls were UNVERIFIED when this landed (no R with
  the `arrow` package here); `r-arrow-verify`, the same day, ran them
  against R 4.6.1 + arrow 25.0.0 in a container: 6/6, no change needed.
