- [ ] zipper-mirror-derivative — the GENERIC zipper: McBride's
      derivative of a type, derived from a `Mirror` (recorded at the
      operator's request 2026-09-22, "for the future"; the plate road
      `zipper-plate` beside this is what the three known consumers
      need and lands first). The one-hole context of a product
      `A * B * C` is the sum `∂A * B * C + A * ∂B * C + A * B * ∂C`, of
      a sum it is the sum of the derivatives, and of a recursive type
      the chain rule closes it — docs/theory/10-optics.md:294-303
      already says an optic's residual IS this derivative and Huet's
      zipper is it carried with a focus. What a Mirror-derived
      `Zipper[S]` would give that the plate cannot: a cursor into a
      HETEROGENEOUS case class (a frame whose focus type changes with
      the field, so `down` into `Order.customer` focuses a `Customer`
      and `down` into `.lines` a `Vector[Line]`), typed at compile
      time, with `Lens.field[S]("name")` (Optic.scala:401) as the
      per-field step already derived. The shape is `Focus[S]`
      (Focus.scala) extended with a context: a `Frame[S, A]` per
      product field = the `Replaced`-style put-back (Optic.scala:422)
      with the field index, and a heterogeneous path
      `Frame[S, A] :: Frame[A, B] :: …` as a tuple type — the
      row-membership crash rule applies (AGENTS.md, an obligation over
      a row is CARRIED), so the path is a parameter, never summoned.
      Beside the plate zipper its `at` is `Focus`'s existing compose
      and its `focus` lens is the same two bridges. COST TO NAME
      BEFORE STARTING: every frame is a distinct type, so a loop that
      walks "the children" cannot exist without a uniform view — which
      is the plate again; the generic zipper is for a cursor that
      knows statically where it is (a typed form editor, a typestate
      protocol over a record: `PState` where the state's TYPE is the
      cursor position), not for tree walks. TRIGGER: a consumer whose
      cursor position must be a TYPE — a form whose editing program is
      written against `Customer` while parked in an `Order`, or a
      second `PState.zoom` caller wanting to descend two fields. Until
      then a record, so the next reader does not re-derive the
      chain rule.
