- [ ] capability-as-a-credential-elsewhere — `Capability` reaches
      exactly one door (MCP tools). `Secure.bearer` still takes a
      `String => Verified`, which a capability cannot be: whether it
      permits something is a question about the REQUEST, not about
      the token alone. If a second consumer appears, the shape to
      copy is the one used here — a liveness probe against the
      capability's own scopes, and the real decision per action. One
      of three follow-ups mcp-tool-authorization (security.md stage
      7, the tool gate and capability door) named rather than did.
