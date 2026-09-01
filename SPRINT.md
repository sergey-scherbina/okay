# Sprint

## Doing
- [ ] security-oidc — OpenID Connect on the existing pieces:
      discovery (.well-known/openid-configuration), the code+PKCE
      login URL with nonce, id_token validation (signature via JWKS,
      issuer, audience, nonce, at_hash) answering a Principal; a stub
      IdP proves the whole flow and every refusal
      (spec: specs/security.md; claim: .work/active/security-oidc.claim)

## Queue
(next candidates from BACKLOG.md: persist-stage1, sql-seam,
 conf-impl — the seams the most filed work binds to)

## Backlog gate
Promote from BACKLOG.md when the sprint empties; a task enters the
sprint only with a spec section it implements.
