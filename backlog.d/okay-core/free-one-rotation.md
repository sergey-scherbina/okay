- [x] free-one-rotation — DONE. `Free.resume` is a member and the one
      rotation; `Free.fold`, `runFree` and Async's loop are three-case
      matches over it; `!.resume` is gone. Only `Cont.step` keeps a
      copy, because it composes through `bind` for absorption. Seven
      lanes faster (effCont24 0.862 at −1016 B/op, effFunc24 0.975 at
      −1064), the `fusedSWr` floor held at 0.985 with B/op identical.
      COST, recorded rather than netted away: relayPrebuilt 1.039,
      relayForward 1.029 — those lanes end in `runWith` over a 9 900-op
      residual, so `runFree` pays a `resume` call per operation, which
      is where it was predicted. Rows `onerot-*`.
