# okay-acme

An ACME (RFC 8555) client: the protocol a certificate authority
speaks to issue a certificate without a human — prove you control the
name, get 90 days, renew before they run out. specs/acme.md holds the
decisions.

| | |
|---|---|
| `Acme.ensure(cfg, http, challenges)` | issue if needed, renew when less than `renewBefore` is left, answer `Issued` or `Current` |
| `Acme.Config` | email, domains, where the account key, certificate and key live, the directory, the renewal window |
| `Acme.Dns` | the seam dns-01 needs: write a TXT record, remove it, and say how long this provider takes to propagate. No implementation ships |
| `Acme.Challenges.Memory` | somewhere to put the token, plus the `routes` to chain in front of a plaintext server |
| `Acme.revoke(cfg, http, reason)` | take a certificate back before it expires (RFC 8555 §7.6); `okay.acme.Revoke` is the same thing from a shell |
| `Acme.renewalWindow(cfg, http)` | the CA's suggested renewal window for the certificate on disk, when it publishes one |
| `Acme.Directory` | Let's Encrypt's staging (the default) and production URLs |

Narrow on purpose: one order, one server, no provider implementations
(a `Dns` is the deployment's). HTTP-01 by default and DNS-01 — with
wildcards — when a `Dns` is given. Revocation, external account binding
(`Config.eab`, for a CA that will not open an account for a stranger)
and ARI (the CA's own renewal window, read beside `renewBefore` and
never instead of it) ARE here. Wide is certbot's
job, and a half-maintained wide client is a site that stops renewing
on a Saturday.

The parts that are not obvious: ACME's JWS is a flattened JSON
serialization with `nonce`/`url` in the protected header and the key
as `jwk` on the first request then `kid` after; reading a resource is
a POST with an empty payload; the key authorization hashes a
canonical JWK whose exact shape (RFC 7638) is the specification. The
CSR is built by `openssl req` because no exported JDK API builds a
PKCS#10 — absent openssl, the refusal says so and points at a proxy.

Tested against a fake CA in-process that fetches the challenge over a
real socket and signs the CSR; interop with a real CA is Pebble, filed
as `acme-pebble`.
