- [ ] cont-program-answer — operator's ask (2026-10-02, "можешь решить эту
      проблему?" on way 1 of the bridge): an opaque `Cont.shift` body whose
      answer `S` is a PROGRAM and which calls `k` itself gets a lazy `k`:
      `k(a)` returns `Delay(owned(k(a)))` at once, no nested run. Any
      interpreter forces it (a bounded run of `k`'s rest, answering the
      program that goes on); a RUNNING machine steps in and continues into
      that program in its own loop, so the rest of the body is a frame of
      that machine — no translation of Free (handle-on-machine's 1.74x was
      the translation and the exit per operation). Bodies that only PASS `k`
      (`perform(e).flatMap(k)`) keep the strict leaf. Contract: host side
      effects after `k(a)` in such a body run before `k`'s rest. Acceptance:
      a million nested such bodies on a 128 KB JVM thread and on Scala.js
      and Native; answers equal to the strict leaf's; no lane slower.
      (2026-10-02)
