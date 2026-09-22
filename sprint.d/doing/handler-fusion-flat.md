- [ ] handler-fusion-flat — `Handler.flat[R]`: `Handler.union`
      assembled inline so the nested chain unrolls to ONE match over
      the row's operation classes. REOPENED with its number
      (staged-block-lanes, 2026-09-22): the hand-written flat handler
      is **1.24x** over the nested union at position 4 and 1.08x at
      position 1 on a four-effect row, bytes identical — pure dispatch
      (specs/handler-fusion.md, the `Handler.flat` box; history rows
      `sbl-flat*`). Was GATED OFF by stage 0 for the wrong reason: pass
      fusion's 1.13–1.29x said nothing about comonadic dispatch.
      SHAPE: an `inline def flat[R[+_]]` recursing on the row TYPE
      (`inline erasedValue[R] match case _: (F + G) => …`), each level
      one `TypeableK[F].test` and the leaf `summonInline[Handler[F]]
      .handle`, the whole thing one expression per call site;
      `Distinct[R]` required as for `union`. LAWS: agrees with
      `Handler.union` on every operation of a four-effect row at all
      four positions (TestHandleForward's row or the README's
      `Model + (Tool + (Context + Async))`); a row of two is the base
      case. NUMBER: `FlatDispatchBenchmark` gains `inline4`/`inline1`
      lanes — within 10% of the hand-written `flat4`/`flat1` (the
      stage-1 rule) or the inline form left the win on the table.
      WHO USES IT: the README's agent-row recipe (okay-security
      TestReadmes, TestStepper) — update those call sites and the
      README paragraph that teaches `Handler.union` nesting.
