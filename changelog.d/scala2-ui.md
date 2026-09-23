## scala2-ui - okay-ui from Scala 2.13, the last of the five areas

This closes the operator's list (HTTP, SQL, codecs, agents, UI). All
five are in package `okay.scala2`, each as its own module, and each
is compiled from Scala 2.13 in the gate.

- Probed from scalac 2.13.18: okay-ui's `Ui`, `Event`, `Frame`, `Form`,
  `Swing` and `Terminal` are readable, so a Scala 2 view uses okay-ui's
  own constructors and events. `Ui.run` and `Host` answer programs.
- The new module okay-scala2-ui adds `UiApp.run`/`runWith`/`window`,
  okay-ui's Elm loop as an `Eff`, plus `UiHost.terminal()`/`swing(container)`,
  and `ScriptedHost` (events in, frames kept) for tests. The object is
  named `UiApp`, not `App`, because `App` would capture
  `object Main extends App`.
- `TestUiFromScala2` (3 tests): the loop with frames drawn only on
  change; a frame as text; an external source ending the loop with
  exactly 3 increments. The first cut asserted a range, because the
  host's `Closed` raced the external events. `ScriptedHost.open` made
  it exact. A Scala 2 trap is recorded: an enum case constructor is
  typed as the case, so `Source[Event](...)` needs its type.
- Docs: section 8e of docs/scala2.md (copied from the probe), a module
  page, API reference, typepedia, spec stage 10. The summaries in
  README, docs/README and the guide's opening now name all five areas.
