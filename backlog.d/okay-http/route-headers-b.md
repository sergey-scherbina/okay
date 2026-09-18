- [x] route-headers-b — DONE 2026-09-11. `.secured(scopes*)` on the
      request-shaped declaration, `Router.enforcing(verify)` on the
      table, 401/403 in `answers` without the author writing them, and
      `securitySchemes` + a per-operation `security` in the document.
      okay-admin converted: its seven existing ladder tests pass
      through the new road unchanged, which is the evidence the
      conversion preserved behaviour.
      Two decisions worth keeping. The module boundary shaped the
      interface — okay-security depends on okay-http, so a route
      declares DATA (scheme, scopes, realm) and `Router.Verify` is
      `String => Either[String, Set[String]]`, with `Secure.verifier`
      adapting; a `Policy` that reads the action or resource stays
      with `Secure.granted`. And FAIL CLOSED: a secured entry whose
      table never got a verifier answers 401 `no_verifier`, because
      declaring a requirement and forgetting to enforce it would open
      a hole the document swears is shut.
      The law is asserted twice, as set equalities: `enforcing`
      refuses exactly the secured entries, and the document marks
      exactly those operations.
