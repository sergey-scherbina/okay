## shift-in-scope - inside reset { … } a shift names only its value type

Backlog shift-effect-level1's (3) (operator: "Продолжай", 2026-10-02;
specs/shift-effect.md).

- `reset` hands its block a `Shift.In[R, F]`, Delim's `Prompted` pattern.
  Inside the block `shift[A](k => …)` and `shift0[A]` take the answer and
  the row from it, and `reset { … }` takes them from its expected type:
  `val q: Int ! State % Int = reset { for x <- shift[Int](k => …) … }`.
- A program built elsewhere still passes to `reset` unchanged (a value
  adapts to a context function). The full `shift[R, A, F]`, an overload by
  the number of type arguments, is for code outside a block and for a
  capture that crosses a nearer block. The short form outside a block is
  refused with a message saying which form to use.
- TestShiftIn (6), plus one direct-style case in TestShiftDirect. The
  user page now uses the short form.
