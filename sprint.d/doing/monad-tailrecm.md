- [ ] monad-tailrecm — `tailRecM` on okay's `Monad`, DERIVED and
      stack-safe for any carrier through `Cont`'s data machine (operator
      ask, 2026-10-02, after effects-foldmap measured an eager `flatMap`
      safe there), and with it cats' `Monad` in `ToCats`. Measured first:
      a million iterations through `Option`; cats-laws' tailRecM
      stack-safety law on the bridge.
