- [x] dataflow-reconnect — LANDED, both halves, and the measurement
      the entry asked for says both are needed. TOLERANCE: a worker is
      buried after three CONSECUTIVE failures and any answer clears
      its count, which makes a blip on EVERY worker survivable — with
      a tolerance of one, the new test dies with the same sentence
      stage 5's first seeded test produced, "no workers left (4 were
      given)". That alone is enough for a worker that hiccups and
      cannot be enough for a SOCKET, whose failure is permanent by
      construction, so `Served.reconnecting` dials lazily and drops
      the socket on any failure. Against a server that hangs up after
      every request, `connect` dies and `reconnecting` finishes the
      job — the two roads differing only in which `Serve` the
      coordinator was handed. `Run.failed` (attempts lost) is reported
      beside `Run.retried` (workers buried), because a run can now
      recover from a failure without burying anybody, and two tests
      that asserted the burial were asking the older question.
