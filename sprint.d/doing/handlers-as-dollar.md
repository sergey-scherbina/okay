- [ ] handlers-as-dollar — stage 3 of specs/shift0-dollar.md: the FSCD 2019
      correspondence (Piróg, Polesiuk & Sieczkowski) made executable. A
      deep State handler written as `dollar` + `shift0` is checked
      against `State.handle` with `Bisim.check`, and a shallow one via
      `control0`. Verdict and price before anything in Handler.scala
      adopts it. AFTER delim-dollar. (2026-09-24)
