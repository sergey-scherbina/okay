## scala2-docs-lessons - what wrapping the libraries taught about scalac 2.13, written where readers look

Done while the WebSocket lane's gate ran; the operator asked for the
documentation to be brought up to date meanwhile.

- Theory ch. 13's "What the Scala 2 reader forced" had the three facts
  of the core facade. It now also has the six found while wrapping
  codecs, HTTP, SQL, agents, UI and WebSockets: `Json.tasty` is
  unreadable (so JSON crosses as text), Scala 3 generic tuples are
  refused, top-level aliases are invisible though what they name is
  not, names in the facade's package are read during name resolution
  (`ProgBody`, `UiApp`), an enum case is typed as the case, and
  implicit search DOES find Scala 3 givens.
- docs/scala2.md section 10 has five more errors, each with its cause
  and fix: a constructor naming a row, generic tuples, top-level
  aliases, the enum-case type mismatch, and `DriverManager` in
  unforked tests. Section 11 names fair search as missing and points
  at the backlog item `residual-row-typeable`.
