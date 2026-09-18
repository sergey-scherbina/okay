- [ ] named-pairs-rest — the SURVEY behind named-pairs-security, kept
      whole because the operator asked for everything found. Seven
      functions in main sources return a SAME-TYPED pair, so a
      positional swap compiles and no test can catch it by type. Two
      landed (`OAuth2.pkce` -> `(verifier, challenge)`,
      `ApiKey.issue` -> `(key, digest)`), where a swap is a security
      defect. These five remain, each with what a swap would do:

      | where | returns | a swap gives you |
      |---|---|---|
      | `Secrets.scheme` (okay-conf) | `(String, String)` | the scheme and the rest of the reference exchanged, so a `vault:` ref reads as a literal |
      | `KafkaStore.range` (okay-kafka) | `(Long, Long)` | begin and end offsets exchanged, an empty or backwards range |
      | `Smtp.stamp` (okay-mail) | `(String, String)` | the Date header holding a Message-ID and the reverse |
      | ~~`Site.splitUrl` (okay-script)~~ | DONE 2026-09-12 | it was PUBLIC, see the correction below |
      | `FileStore.readHeader` (okay-persist) | a pair read off a buffer | (unread — check before taking) |

      "All are private or module-local, so the blast radius is small
      and so is the value" — THAT SENTENCE WAS WRONG, and it is left
      standing rather than edited away because it is the reason the
      row above was not taken with the security two. `Site.splitUrl`
      carries no modifier at all: it is public API, its caller may not
      be in this repository, and a swap hands them the query string as
      the path. Corrected and done by `split-url-named` (2026-09-12)
      when the operator asked "so nothing needs doing?" and the
      question sent me back to check the claim instead of repeating
      it. The remaining FOUR really are private or module-local
      (`private[conf]`, `private[KafkaStore]`, `private`, `private`),
      checked one at a time this time. Naming them costs nothing at runtime — a named
      tuple IS the plain tuple, measured in named-tuples-stage0 — and
      the call sites need no change, since a positional destructure
      still works. Take them when touching those files for another
      reason rather than as a sweep.

      METHOD NOTE, which is the reusable part: `val (a, b) = named()`
      still binds BY POSITION, so naming a return does not by itself
      stop a caller swapping. The protection is at the call site,
      through `p.verifier` or a named destructure
      (`val (challenge = c, verifier = v) = pkce()`, which binds by
      name whatever order it is written in). named-pairs-security
      moved its call sites for exactly that reason.
