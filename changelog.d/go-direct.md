## go-direct - direct-style Go: c.Call(name, args) -> answer

Part of specs/polyglot-one-wire.md, the operator's
`okay_call(request) -> answer`.

- Ordinary Go functions (`okay.Functions`) now call okay's effects in the
  middle of a computation with `c.Call` or `okay.CallOp` (typed by a
  generated operation), and get the answer.
- It is the wire's callback dialogue (`start`/`ask`/`resume`), the same
  one Python's `okay.call` uses. It works over pipes and TCP, and a
  failed or unoffered callback is a Go `error`.
- The conformance suite has a direct-style case, green over both links,
  and a mutant is caught.

Docs: "Direct style" in docs/go.md.
