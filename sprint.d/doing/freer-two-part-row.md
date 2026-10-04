- [ ] freer-two-part-row — PRIORITY: HIGH (operator, 2026-10-04: "если
      определить правильный Freer.flatMap то дальше все должно просто
      работать"). Freer's row in two parts, `Freer[U, I, S, R, A]`: the
      unary effects under ONE bridge (`U` always `Unary[F]`) and the
      indexed signatures `I`; `flatMap` answers `Unary[F + F2]`,
      `I +~ I2`, so a `for` over different effects needs no widen;
      `into enum Freer` and one Conversion by `Row.Sub` for the expected-
      type positions (assignment, argument, branch, return). Modelled
      green (specs/foreign-effects-in-tree.md probes v4, C). Port to the
      real core; count what breaks; measure the hot lanes.
