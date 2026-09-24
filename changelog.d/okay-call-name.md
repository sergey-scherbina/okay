## okay-call-name - one name for direct style: okay_call(request) -> answer

The operator: "я просил назвать okay_call". The direct style had five
spellings; it now has one, wherever the language allows it.

- Rust (the crate in okay-py's jar): `okay::okay_call(request) ->
  Result<A, OkayError>`, a free function. The call in progress is a
  thread-local of the thread the worker runs the function on, so a
  direct-style function takes only its arguments (`function(|args| ...)`),
  and `impl From<OkayError> for String` makes `okay_call(ops::price_of(sku))?`
  the whole call. `Ctx`, `ctx.call` and `ctx.call_op` are gone.
- Go: `okay.Call(c, request)`, typed, with `okay.Named(name, args...)` for
  an untyped request. `c.Call` and `okay.CallOp` are gone. Go cannot
  spell `okay_call`: an exported name begins with a capital, and a
  goroutine has no local storage to hide `c` in.
- Python `from okay import okay_call` and TypeScript `okay_call` are the
  name; `okay.call` and `call` stay as aliases. R already had
  `okay_call`. The refusal message says `okay_call(...)` in each.
- Docs: rust.md, go.md, one-language.md, python-and-r.md. New in
  one-language.md: "One okay_call, step by step" (a sequence diagram of
  start/ask/resume and why a thread holds the function's place). Spec:
  polyglot-one-wire.md results.
- Mutant: a Rust thread that does not hold its call fails exactly the
  direct-style conformance test.
