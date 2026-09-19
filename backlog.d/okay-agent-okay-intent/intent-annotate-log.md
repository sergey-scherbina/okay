- [ ] intent-annotate-log — the model reads the LOGS and proposes
      labels for REAL messages (operator's direction; specs/intent-classify.md,
      "The harvest programme"). Not the distillation that failed: the
      model writes nothing, the messages are real traffic, only the
      label is proposed — the practice the literature supports
      (arxiv 2406.17633, 2503.17336). A row is kept only if the
      reading grounds in the message, conf >= Medium, k samples agree,
      and no deterministic tier contradicts at high margin; provenance
      per row (model, prompt fingerprint, date, filters passed).
      Criterion: 100+ kept rows, and a refit on them moves the
      autonomy rate without breaking the per-class law.
      (was filed under "the autonomy programme" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)
