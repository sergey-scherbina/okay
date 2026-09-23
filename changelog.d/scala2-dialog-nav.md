## scala2-dialog-nav - okay-ui's scenarios and screens from Scala 2.13

The last item of the Scala 2 queue.

- Probed from scalac 2.13.18: `okay.ui.Screen` can be IMPLEMENTED in
  Scala 2, and `Nav`'s cases and its pure `state`/`update`/`view` are
  readable. So a screen stack written in Scala 2 runs in the ordinary
  `UiApp.run` loop with no facade. `Nav.screen`, whose update answers a
  union, is replaced by `Screens.of` with an `Either`.
- `okay.scala2.Dialog` makes okay-ui's scenario effect a capability of
  `Eff`, with `show`, `ask` (a form from a `Schema`, over `Form.ask`),
  `run` on a host, and `replay` with no host. The last is how a
  scenario is tested.
- `TestDialogNavFromScala2` (4 tests): a two-question scenario
  replayed, `ask` submitted with `$ok` and cancelled with `$cancel`, a
  scenario on a `ScriptedHost`, and a stack of Scala 2 screens under
  `UiApp.run`.
- Docs: section 8i of docs/scala2.md (copied from the probe), the
  okay-scala2-ui page, API reference, typepedia, and spec stage 14.
  Still unwrapped: durable agents, and `Scope` inside a dialog.
