- [ ] mcp-tools-granted — the principal AMBIENT in the gated route,
      the way `Secure.granted` and `McpAuth.granted` already close
      that family (`Principal ?=> route`). `tools` hands the route
      nothing today, so a handler that wants to know WHO is calling
      reads it out of the request again. Small, and it waits for a
      caller: nothing in the tree wants it yet. One of three
      follow-ups mcp-tool-authorization (security.md stage 7, the
      tool gate and capability door) named rather than did.
