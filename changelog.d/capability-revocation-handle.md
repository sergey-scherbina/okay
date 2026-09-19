## capability-revocation-handle — a deny-list of tokens is not revocation

Landed as 1e7b7d1a and 7de0187e (the spec, before the code), 5c2ef755
(the caveat, the predicate and the door), de689794 (the external-list
seam) and fda488c6 (the spec's boxes).

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

We do not keep the list, and `Revocations` is how somebody else's is
consumed: a local snapshot of a remote list, refreshed by a program
the caller runs, read synchronously by the door — because a
capability is checked per tool and `tools/list` checks every tool, so
a network call inside the predicate is one round trip per tool per
request. A company whose business IS that list is a supplier rather
than a competitor, and this is the seam that takes one without
knowing whose it is.

An external list does not change the check; it changes what can go
wrong, and the seam names both:

- **the dangerous failure is not "the registry is down" but "the
  registry answered empty"** — a source that turns an error into an
  empty set un-revokes everyone the instant it breaks. `Source`
  answers an Either, a failed refresh KEEPS the list it had (a
  throwing source included, through `Async.attempt`), and an emptied
  list is only ever an answer;
- **staleness is a decision, so there is no default** — past
  `freshFor` the snapshot stops being evidence, and `whileStale` says
  what that means: Allow where the list is advisory, Deny where it is
  load-bearing. Same rule before the first fetch, which is why Deny
  refuses everything until one arrives.

What stays out is the list itself — what store, what TTL, who writes
and how it reaches several verifiers are a deployment's answers
(backlog.d/okay-security/capability-revocation-registry.md). 87 tests
in okay-security and 38 on the JS leg, the load-bearing ones being
that a holder who attenuates again does not escape the list, and that
a broken registry un-revokes nobody.
