- [ ] okay2-handler-control-resume — okay2's `Handler.control` answers
      every claimed operation by a `Cont.shift`, so a clause that resumes
      `k` in tail position still captures. The Scala 3 core's form does
      not: its `Resume` answers a tail resume with no capture, and its
      first `Delay` uses the object itself as the thunk (handler-forms,
      control 67.39 -> 37.96 us). Port that, and measure okay2's control
      lane against `!.handle` with a hand-written `Interpr`. (2026-10-02)
