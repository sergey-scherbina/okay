- [ ] compile-time-only-is-not-a-guarantee — measured 2026-09-11 in
      optics-outside-route-syntax. `@compileTimeOnly` was tried as the
      carrier of a nicer refusal message (a poisoned `/` on
      `Route.Named`, since precedence sends the mistake there). It
      fired in five isolated variants — dotted, infix, overloaded
      method, overloaded caller, extension receiver, parenthesised and
      not — and silently did NOT fire in the real expression, even
      after a clean rebuild; worse, its presence made two invalid
      declarations compile, because the poisoned method returned a
      usable type where its ABSENCE had produced an error. The trigger
      was never isolated. Anyone reaching for `@compileTimeOnly` in
      `specs/error-messages.md` should read this first: it is fine for
      a message, and must not be the thing that makes an invalid
      program invalid.
