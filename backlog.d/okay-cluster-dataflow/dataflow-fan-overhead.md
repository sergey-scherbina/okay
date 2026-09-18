- [x] dataflow-fan-overhead — CLOSED by measurement, and the third
      is not there (MeasureFanOverhead, Live). Re-measured lane for
      lane: the fan is 101-111 ms and its three sinks, each run as its
      own fan, sum to 90-92 — a gap of 9-20%, not a third. The 50 ms
      came from arithmetic across differently-shaped lanes (the
      bunching sink has NO pre-pass; the fan's has two columns) on a
      loaded box, and the parts have changed under it since (topK
      stopped sorting). Of the three candidates: a pre-pass column is
      5-6 ms over 1.25M events and a second column in the same pass
      4-5; an `and` arm that does nothing is 0-3 ms, which is below
      this instrument. And the finding worth more than the entry: THE
      FAN IS NOT FASTER THAN THREE SEPARATE FANS on this feed
      (89-96 against 101-111) — one pass saves ~2 ms of source reads,
      because the source is an in-memory array, and pays more than
      that for three operators' state being live at once. What a fan
      buys is a source read ONCE, which matters when the source is a
      file, a topic or a socket, and no shuffle. Pricing the residue
      needs JMH; nothing has asked.
