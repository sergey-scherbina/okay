- [ ] okay2-interop-async — okay2-cats/-fs2/-zio were written before
      okay2 had `Async` (stage 4) and `Resource` (stage 6), and still
      say so (CatsInterop.scala's header, docs/okay2.md section 9). What
      the Scala 3 core's interop has and they do not: an `Async`
      program run under `IO.blocking` (cats) and `ZIO.attemptBlocking`
      (zio), and `fromZStream`/`fromFs2` as a scoped pull under
      `Resource` instead of collecting the whole stream in one
      operation. Filed when `okay2-stage2` closed (okay2-aggregate,
      2026-09-24), which the docs had named as the home of this gap.
