## direct-layers-instances - direct blocks over Layered layers and Lexical instances

The operator asked for `direct` blocks that mix monads (Layered) and use
effect instances (Lexical). A stage-0 probe showed most of it already
worked, because instance operations and a layer's `reflect` are programs,
and a program block's mark reflects programs.

- NEW: inside a layer, `.?` on the monad's own value is its reflect. The
  macro (DirectRow `markTerm`/`layerTerm`) finds `Layered.Reflect[M, R]`
  through the value's base classes, so `Some[Int]` and an `if`'s
  `None | Some[Int]` reach Option's layer. It emits the ordinary
  `Layered.reflect` call, and `narrowRow` widens its Delim row with
  Row's proof, so there is no cast. A block row without Delim is refused
  by name, and with no layer in scope the refusal is unchanged.
- NEW: `Lexical.State`'s `put(v)`, `set` as a Unit statement, so
  `s.put(v).?` on its own line leaves nothing unused.
- TestDirectLayersInstances (8), with the full okay-direct suite (420)
  green. docs/direct-style.md (Layer 1½) shows the mark on the raw value.
