## capability-revocation-handle — a deny-list of tokens is not revocation

Landed as 1e7b7d1a (the spec, before the code), 5c2ef755 (the caveat,
the predicate and the door) and fda488c6 (the spec's boxes).

Attenuation is PREVENTION: the holder narrows before delegating. It
does not reach an agent misbehaving now, because the holder who would
narrow it is the one misbehaving. `Capability` says out loud that it
is not revocation and that stays true — a verifier needing no registry
cannot reach out and cancel anything. What this settles is which
handle a registry would be keyed on, because that is a property of the
type rather than a matter of taste.

`attenuate` keeps `id` and `subject`, grows `caveats`, and mints a NEW
`tag` at every step. Hence:

- denying an `id` voids the whole tree — this user's grant is gone;
- **denying a `tag` voids one leaf and none of its descendants**, and
  the tag IS the token, so it is the first thing anyone reaches for.
  One more `attenuate` — free, and needing nobody — and the holder is
  out from under the list. That trap now sits in the type's own
  documentation beside the ones already there;
- a caveat survives downward and cannot be removed, so the handle that
  catches ONE BRANCH is `Caveat.Agent(id)`, written when the
  capability is handed over.

`Capability.checking(now, scopes, revoked)` takes the caller's own
predicate — defaulting to revoking nothing, so every existing call is
unchanged — and `McpAuth.capabilities(..., revoked)` asks it about
every identifier a capability carries: the root id, and each Agent
caveat. A predicate rather than a set, so a database or a cache fits
and okay-security still needs no registry of its own.

The registry itself stays out, deliberately: what store, what TTL, who
writes and how it reaches several verifiers are a deployment's
answers, and designed blind they are what this repository defers
elsewhere (backlog.d/okay-security/capability-revocation-registry.md).
80 tests in okay-security, the load-bearing one being that a holder
who attenuates again does not escape the list, with its counterpart
pinning why the tag cannot be the key.
