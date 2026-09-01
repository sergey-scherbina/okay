# Sprint

## Doing
- [ ] ui — okay-ui v1: core loop+diff (done), terminal host, React/DOM
      mapping, Form from Schema, MCP elicitation closes
      (spec: specs/ui.md; claim: .work/active/ui.claim)
- [ ] http-js-acceptance — (sibling's claim; see .work/active/)

## Queue
- [ ] ui-terminal — the terminal Host: pure frame rendering
      (Vector[String]), focus/tab, stty raw mode jvm+native
- [ ] ui-react — the pure Ui=>Elem mapping + js glue over global
      React.createElement; DOM patch Backend over js.Dynamic
- [ ] ui-form — Form: Schema[A] => Ui, edits fold into partial Json,
      decode by the same Schema; the JSON-Schema (dynamic) variant
      for elicitation
- [ ] mcp-elicitation — Peer.elicit + capabilities.elicitation,
      elicitation/create served by a Form on a scripted host;
      end-to-end in okay-demo

## Backlog gate
Promote from BACKLOG.md when the sprint empties; a task enters the
sprint only with a spec section it implements.
