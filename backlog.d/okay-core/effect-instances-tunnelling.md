- [ ] effect-instances-tunnelling — PRIORITY: LOW (design; trigger). a known limit, now named after its
      literature: rows are keyed by CLASS, so a row holds ONE `Take`,
      one `Reader % X`, and two of a kind misroute (TestRowIdentity —
      loudly, by ClassCastException; `Writer.byValue` is the opt-in
      for two Writers). It is the "accidental handling" problem of
      Zhang & Myers, "Abstraction-safe effect handlers via tunneling"
      (POPL 2019): an operation raised by a callback is caught by the
      callee's own handler for the same effect. Biernacki, Piróg,
      Polesiuk & Sieczkowski, "Handle with care" (POPL 2018) and
      "Abstracting algebraic effects" (POPL 2019) give the lift-based
      answer; Xie, Cong, Ikemori & Leijen, "First-class names for
      effect handlers" (OOPSLA 2022) the named-instance one — the
      shape `Delim.Stacked` took today for prompts. THE LANE, a design
      one: effect INSTANCES — `Reader % E @ Db`-style labels, or
      handlers that are lexically scoped like prompts — such that two
      Readers of one type in one row route by name; the price on the
      hot path (a label test per op) measured before anything is
      adopted, and the class-keyed default kept where one of a kind is
      all there is. Not before a consumer asks twice. (2026-09-23)
