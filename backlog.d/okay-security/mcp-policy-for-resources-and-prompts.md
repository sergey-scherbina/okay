- [ ] mcp-policy-for-resources-and-prompts — the same per-item
      question for `resources/read` and `prompts/get`. The shape is
      settled now (ask the policy with the item's name, filter the
      answer to the list); what is missing is a caller who serves
      resources under authorization. Do it when one exists rather
      than by symmetry — a door with no traffic is a door nobody
      tests. One of three follow-ups mcp-tool-authorization
      (security.md stage 7, the tool gate and capability door) named
      rather than did.
