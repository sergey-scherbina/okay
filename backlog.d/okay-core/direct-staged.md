- [ ] direct-staged — road 2: a `direct` block emitted as a `Func`
      program over `Control` when the handlers are static at the call
      site (`Fused.runCtrl[Func, …](s)(direct { … })`), no tree; parity to
      the byte with the hand-written `rightCtrl[Func]` is the goal, the
      tree version (13.7 µs / 122 641 B) the baseline. The macro's
      pipeline gains a second emission target; the lowering is unchanged.
