- [ ] tod-single-sequence — SimpleTOD (Hosseini-Asl, McCann, Wu,
      Yavuz, Socher, 2020, arxiv 2005.00796): belief state, actions
      and response as ONE delimited sequence rather than three
      models. Filed LAST, and the reason is the useful part. Its
      result is a fine-tuning result (GPT-2 on MultiWOZ) that we
      cannot reproduce without training; the 2025 paper above argues
      the opposite architecture on ground that suits us better; and a
      single delimited sequence WEAKENS the invariant the demo is
      built on, since a belief cut out of raw text has not gone
      through a tool (recoverable only by decoding it through
      `Schema` before it touches the store — intent-classify's own
      rule). What stays attractive is the SHAPE: SimpleTOD's
      inference suspends after the belief state, queries the KB,
      appends the result and resumes the SAME generation — a
      coroutine that yields exactly once, which is `Stage` over
      `Cont` and something okay expresses better than a framework
      would. Worth building only if the items above leave a reason to.
      (was filed under "Task-oriented dialogue: the literature the operator " — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)
