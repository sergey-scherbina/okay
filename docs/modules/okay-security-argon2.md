# okay-security-argon2

The one satellite that buys a dependency (specs/security.md, stage
5): Argon2id via Bouncy Castle, because a memory-hard KDF cannot be
had from the JDK — and a separate module keeps okay-security's zero.
Services opt in by classpath.

| | |
|---|---|
| `Argon2.hash` | Argon2id with OWASP-shaped defaults; the stored form is the PHC STRING every other implementation reads (`$argon2id$v=19$m=…,t=…,p=…$salt$hash`) — parameters ride the row, so raising them is a hash-by-hash migration, never a flag day |
| `Argon2.verify` | total against a HOSTILE stored form: garbage refuses, and ABSURD parameters refuse BEFORE allocating — a row claiming gigabytes is an attack on the verifier, not a password |
| `Argon2.verifyAny` | the migration door: one call reads a mixed `pbkdf2$`/`$argon2id$` store |

The RFC 9106 test vector pins the provider against the standard
rather than against itself. PBKDF2 (in okay-security proper) stays
the zero-dependency default for existing stores.
