## identity-roster - a channel address bound to a principal, roles as grants, a roster that is a fold

- `okay.security.Roster` over an okay-persist `Topic`: `Channel` (total,
  `Other(kind)`), `Binding`, `Grant`; `bind`/`unbind`/`whoIs`/`addressesOf`,
  `grant`/`revoke`/`rolesOf`/`principals`; every change appended then
  applied, `load()` replays. The roles ride in the principal's
  `claims.json` under `roles`, where `Policy.role` already reads them.
- `Roster.role(r, role)`: a `Policy`; a scoped grant permits the resource
  prefix it names. Composes with `allOf`/`anyOf`.
- `Roster.owned(topic, channel, address)`: the configured address and the
  console are `owner`; idempotent on an existing roster.
- okay-security gains `.dependsOn(okayPersist)` (build.sbt). Lifted from
  okay-chat's `Identity` minus its product. Spec: specs/identity-roster.md,
  all items checked; `TestRoster` (7, cross). Consumer: `../nadia` `app/`.
