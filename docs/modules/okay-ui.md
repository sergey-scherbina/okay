# okay-ui

The toolkit that is not a toolkit (specs/ui.md): the view is a VALUE,
the loop is a fold over merged event sources, and the renderer is a
seam — so one application runs on a terminal, under React, on a test
host, unchanged. Elm's architecture, with the two things Elm fakes
made real: effects are the effect row, and `Cmd` is not needed
because subscriptions are `merge`.

## The pieces

| | |
|---|---|
| `Ui` | the tree — data only, keys instead of closures: equality, diffing, pure rendering, and (later) a tree that can cross a wire |
| `Event` | what the user did, naming keys; UNTRUSTED by doctrine — the shown tree is the capability list |
| `Ui.run(init)(view)(update)(host, external)` | the loop: pure update, the world merged in as sources |
| `Ui.diff` / `Patch` / `Host.diffing` | retained trees, narrow patches; a patch consumer becomes a Host |
| `Host` | the seam, React-shaped: hand over the whole tree — two functions, like `Link` |
| `Frame` | the terminal's pure half: frames are `Vector[String]`, keys interpret against the tree — tested with no tty |
| `Terminal.host()` / `Terminal.raw` | the thin impure edge: stty, stdin, painting (JVM + Native) |
| `React.elem` / `ReactJs.host` | pure `Ui => Elem` (JVM-tested), five lines of js glue over any `createElement`-shaped global — React, Preact, likes |
| `Form` | the fifth algebra over `Schema`: a form rendered from the same schema that decodes it; plus the dynamic (JSON Schema) side elicitation needs |

## The circle it closes

MCP elicitation — the server asking the HUMAN — was parked until a UI
contract existed. Now: `Duplex.Peer(elicit = ...)` answers
`elicitation/create`, and okay-ui's dynamic Form renders the
requested schema, folds the user's edits, and answers typed.
`TestElicitForm` (okay-demo) is the whole loop in one assertion.

## What is deliberately not here (v1)
Raw-DOM patch backend (React covers the browser), keyed reordering in
the diff, native toolkits (satellites over the same seam), styling
beyond bold/dim, Windows raw mode.

The architecture above v1 is largely BUILT since: scenarios as
programs (`Dialog`, with cancellable scopes — `Scope`, the Delim
door, now also in capability form: `mark`/`exit`/`bounded`, exit
to the NEAREST scope by nesting), screens as a stack (`Nav`, with
NAMED boundaries: `Nav.boundary`/`PopTo` drop intervening frames
as DATA — the mechanism the stack itself dictates), the raw-DOM
patch backend (`Dom`), forms from schemas, event-sourced sessions.
specs/ui.md holds the decisions; docs/typepedia.md the capability
patterns.

## Two vocabulary levels (specs/frontend.md, stage 0)

The tree has a CLOSED layout level a thin client must draw — `Box`
with weights/gap/pad (Row/Column are its two plain forms), `Text`
with style tokens, `Image`, `Input` kinds, `Button` roles, `Check`,
`Select`, `Scroll` — and an OPEN semantic level — `Form`, `Items`,
`Table`, `Tabs`, `Modal` — where each node is DEFINED by its lowering
to level L (`Ui.lower(ui, vocab)`). The laws: `Ui.keys(s) ==
Ui.keys(lower(s))`, so `update` cannot tell how a client drew a node;
and the diff commutes with lowering. The wire is `Protocol`
(stage 1): one derived definition — `Msg` = Hello / Tree / Patch /
Event / Close over the derived `Schema[Ui]`, `[Event]`, `[Patch]` —
as JSON lines or CBOR bytes; the client's `Hello {vocab, version}`
comes first and `Wire.serve` lowers what it did not claim; the
in-process hosts lower at their entry. `docs/protocol/frontend.md`
(rendered from the schemas) and `docs/protocol/conformance.jsonl` are
the contract a client in any language implements. The wire is HYBRID
(stage 2): a `Form`'s fields fold on the client and its button sends
ONE `Event.Submitted(key, edits)` (folded by `Form.submitted` through
the same `Form.edit`); a `live` input speaks per change; a claimed
`Tabs`/`Disclosure` switches locally; the server's `SetValue` wins.
