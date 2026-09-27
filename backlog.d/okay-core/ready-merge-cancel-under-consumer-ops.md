- [ ] ready-merge-cancel-under-consumer-ops — on the callback drive
      (`own`/`adaptive`, a `DriveTask`), a `mergeReady` whose one source
      is PARKED while another keeps telling, consumed by a consumer that
      performs an operation per element (`runForeach` with an
      `Async.Run`), leaks the parked source's registration on cancel:
      50 of 50 on `own` (probe, 2026-09-27: a 20 M-element ready side and
      a gate; the gate's canceller never ran, waited on 1 s). The drive
      stops before the CONSUMER's next op; the merge's code sits inside
      the consumer's continuation (a function the drive must not call),
      and the merge never parks, so neither its park's canceller nor its
      `Discontinue` (ready-merge-own-cancel-window) is reachable. Nothing
      the merge builds can close it: it needs the drive to carry a cancel
      hook a program can register while it runs (not only the canceller
      of the Await it last parked on), or the Writer handlers
      (`Writer.loopWith`, `runForeach`) to forward a producer's
      `Discontinue` onto the operations they perform. Loom not measured
      in this shape (an interrupt the fiber sees at its next block). The
      same family as specs/ready-merge.md's "early stop is not
      cancellation". TRIGGER: a mergeReady user on `own` whose parked
      source holds something a missed cancel leaks (a channel receive, a
      socket read), or the `scheduler-default-decision` flipping the
      default to `adaptive`. Spec: specs/ready-merge.md Decisions.
      (2026-09-27, ready-merge-own-cancel-window)
