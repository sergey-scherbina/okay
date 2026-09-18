- [x] dataflow-recovery — LANDED as stage 5. A lost partition is
      recomputed on a survivor, and it is nearly free exactly as the
      entry guessed: a partition is a thunk and its partial is a pure
      function of (parameters, index, count, bounds). Under SEEDED
      schedules rather than luck — forty of them — plus a real worker
      process killed mid-run. Not under `Sim`: the seeds are the
      suite's own, because what varies is which worker dies at which
      request and that needs no virtual clock.
