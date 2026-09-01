# Backlog

## okay-ui: above v1 (specs/ui.md, "The architecture above v1")
- [ ] ui-scenarios — Dialog effect: show-await-validate flows as
      programs inside the loop; wizard test; elicitation becomes a
      one-step scenario
- [ ] ui-screens — screen stack, navigation events, routing; DOM
      address bar as an event source
- [ ] ui-wire — Schema[Ui]/Event/Patch derivations; server-driven
      session over okay-http WS (events up, patches down); the
      security rule "the tree is the capability list"; reconnect =
      full tree then patches
- [ ] ui-durable — event-sourced sessions: intent-first journal
      (Durable shape), refold recovery, snapshots
- [ ] ui-dom-patch — a raw-DOM patch Backend over js.Dynamic (the
      React host covers the browser today; Host.diffing is ready)
- [ ] ui-keyed-diff — keyed children matching in the diff (API
      unchanged, keys already in the tree)
- [ ] ui-native-toolkits — GTK/Cocoa satellites over the Backend seam
- [ ] ui-windows-terminal — raw mode beyond stty

## okay-mcp
- [ ] mcp-completion — completion/complete for prompt/resource args
- [ ] mcp-resource-templates — RFC 6570 templates, list + expand
- [ ] mcp-resumable-sse — Last-Event-ID on the HTTP GET stream
      (needs a server-side event journal — pairs with ui-durable)

## okay-http (sibling's area — coordinate before taking)
- [ ] http-streaming-responses — incremental bodies on the NIO and
      Netty backends (Jetty has it); unblocks MCP push there
- [ ] http-post-body-audit — Netty/NIO: do POST bodies reach routes?
      (Jetty's did not — found by mcp-push, fixed there)

## Elsewhere
- [ ] agent-langchain4j — their providers as Model handlers (ROADMAP
      P9 leftover)
- [ ] history-tsv-tabs — six rows with literal \t instead of tabs
      (bpeScan, indexKeyword, keywordTerms, jsonToCst, jsonProject,
      jsonDecode, buildOnly area; flagged in the room 2026-08-31)
