- elements-door-cursor — CHECKED 2026-09-19: the 2.2x is already
  fully explained in docs/benchmarks.md §5/§20, not a defect. The
  `.elements` door reads over the chunk transformers because it pays
  a per-element cursor where the chunked form steps once per CHUNK —
  "what the elements door pays over it is the per-element cursor, not
  the tree". Contextualized against the field: 1.6x the floor beside
  ZIO at 2.3x and fs2 at 1.5x, not an outlier to chase. No remedy
  proposed anywhere; closing rather than leaving an entry that reads
  as unaddressed work.
