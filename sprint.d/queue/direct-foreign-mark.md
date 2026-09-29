- [ ] direct-foreign-mark — `z.?` / `z.reflect` on a FOREIGN effect inside a
      `direct` block over an okay program. Today the macro accepts the
      block's own `F[T]` or an operation of its row and refuses anything
      else. Add a third case: a value with a given lift into `A ! G`
      (a typeclass in okay-direct, instances in okay-zio for ZIO,
      okay-cats for IO, core for Future), G checked against the row like
      an operation. Follows zio-direct-cancel (its `fromZIO` is the lift).
      Prefix `!z` is refuted for ZIO (its own `unary_!` wins over any
      extension; specs/zio-direct-cancel.md Decisions) — check each
      foreign type for a `unary_!` member before promising it.
