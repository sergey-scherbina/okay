## foreign-one-ops — the caller's callbacks serve Frege and Clojure too (2026-09-26)

Stage 7 of specs/foreign-one.md, narrowed by Decision 19: there were no
core-effect `Cbs` to generate `okay.frege.Ops`/`okay.core` from (those bind
the row's own operations and stay); what was declared twice was a caller's
OWN operation. Now the `Foreign.callback`s that serve every wire language
answer a Frege or Clojure program: the JVM walker resolves an
`okay.Foreign.Call(name, arg)` against `calls = Jvm.calls(cbs)`, the
callback's Schema decoding the argument, and `Jvm.frege`/`Jvm.clojure`
write the typed Frege module and the Clojure namespace from the callbacks,
as `Hs.ops` writes Haskell's. Tests: TestFregeCallbacks (generated module
equals the checked-in one; performs under the caller's Reader; an
unoffered call and a callback without a Frege type refused by name; a
wrong argument a Frege compile error) and TestClojureCallbacks. Docs:
docs/jvm-languages.md, "The caller's own operations, declared once".
