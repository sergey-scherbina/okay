# okay-scala2-ui

okay-ui for **Scala 2.13**. okay-ui's tree (`Ui`), its `Event`s and
`Frame` (the text renderer) are readable from Scala 2 and used directly.
The loop and the hosts answer programs, and this module provides them:

| | |
|---|---|
| `UiApp.run` / `runWith` / `window` | okay-ui's Elm loop as an `S ! Async`: in a host, with an external event source, or in a Swing window |
| `UiHost.terminal()` / `UiHost.swing(container)` | okay-ui's terminal and Swing hosts |
| `ScriptedHost(events*)` / `ScriptedHost.open(events*)` | a host for tests: scripted events, every frame kept |
| `Dialog` (`show`, `ask`, `run`, `replay`), `Screens.of` | okay-ui's scenarios as a capability of `Eff`, runnable on a host or replayed without one; `Nav`'s screen stack runs in `UiApp.run` as it is, and `Screens.of` replaces the unreadable `Nav.screen` |
| `FormState[A]` | okay-ui's `Form` over `A` without its `Json`: `view`, `edit(event)`, `errors`, `decoded`, `json`, `withLabels`; `FormState.blank[A]`, `FormState.of(a)` |

The walkthrough is section 8e of
[okay from Scala 2.13](../scala2.md#8e-ui-the-view-as-a-value-the-loop-as-a-fold),
and the signatures are in [okay-scala2](okay-scala2.md#api-reference).
