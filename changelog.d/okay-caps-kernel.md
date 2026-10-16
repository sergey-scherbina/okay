## okay-caps-kernel - the kernel capture-checked without -experimental, to the end: not merged

- okay-core's branch `capture-checking`: every non-inline signature and inline methods' parameters annotated
  (`handle`'s receiver too — a parameter's capture set does not reach callers; an inline RESULT does, as
  experimental `@retains`). Measured from okay-caps: `map`/`defer`/`recover` leaks refused, `flatMap`/`for` not —
  and honest capture-checked code using `.handle` stops compiling (inline `handle`'s result unannotated). Not
  merged. With `-experimental` everywhere (branch `capture-checking-experimental`): okay-core green, but the Scala 2
  module cannot read the kernel and every user needs the flag. specs/okay-caps.md, backlog okay-caps-next.
