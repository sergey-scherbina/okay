- [ ] resource-abort-releases — PRIORITY: LOW (trigger). Found by
      handle-frames-catch (2026-10-03), pinned by
      TestHandleFramesResource "an abort through the scope". A capture to a
      prompt OUTSIDE a `Resource.run` scope whose body drops `k`
      (`Shift.abort`, or any `shift` that never resumes) leaves the scope
      with nothing released: the frame is inside the dropped `k`, and
      nothing tells it `k` is gone. No regression — the old fold threw
      `ClassCastException` on the same program. Two halves, not one:
      (1) an EXPLICIT abort is knowable — mark `Shift.abort`'s capture as
      final and have the machine's capture walk unwind the frames it
      crosses (as a throw does through `Cont0.Catching`); the obstacle is
      that the Resource frame's held list is parameter-passed (the frame
      answers `S => program`), so it is not where the walk can reach it.
      (2) a `shift` that merely never calls `k` cannot be detected without
      linearity or GC — the same wall as `logic-cut-releases`, whose
      OCaml answer is `discontinue`. TRIGGER: the first consumer that
      aborts through a scope. Until then: acquire OUTSIDE the prompt, or
      `bracketNow` inside the scope.
