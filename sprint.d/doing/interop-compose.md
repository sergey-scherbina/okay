- [ ] interop-compose — one expression composing functions written with
      cats, ZIO, kyo and okay, and okay functions called from inside
      each library's code (operator ask, 2026-10-02, after
      interop-classes): a uniform `asOkay` in, `asIO`/`asZIO`/`asKyo`
      out, the same on functions so `>=>` composes them, and a direct
      block marking all four. Spec: specs/interop-compose.md.
