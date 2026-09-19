- [ ] intent-crf-slots — sequence labelling for the frame's SLOTS
      (who, when, where) rather than its class. `Temporal` fills one
      slot with a parser; the general case is a tagger, and a CRF is
      the classical one. Only after the class problem is settled.
      GATED 2026-09-07: "only after the class problem is settled" —
      it is settled for the model tier (0.909, deterministic, the
      decoder reading every reply) and the four parsed slots cover
      what the meeting frame asks; the slots still open (who, places)
      are named entities, which is what a tagger is for, and which no
      frame in the fixture yet asks a question about. Opens with a
      frame that does.
