# okay-acme — a certificate, earned

## Overview

ACME (RFC 8555) is the protocol a certificate authority speaks to
issue a certificate without a human in it: the client proves it
controls a name, the CA signs for 90 days, the client renews before
the time runs out. Let's Encrypt made it the ordinary way a public
site gets HTTPS.

This module is a client for it, and the shape of the client is the
decision worth reading. It is deliberately NARROW — one server, one
order, one challenge type — because the wide version is certbot, and
a half-maintained wide version is a site that stops renewing on a
Saturday.

```scala
object Acme:
  final case class Config(email, domains, accountKey, certFile, keyFile,
                          directory = Directory.letsEncryptStaging,
                          renewBefore = 30.days, timeout = 60.seconds)
  trait Challenges:  put(token, keyAuthorization) / remove(token)
  object Challenges: final class Memory  // with `routes` to chain into a server
  enum Outcome: case Issued(domains, notAfter); case Current(notAfter)
  def ensure(cfg, http: Http, challenges): Either[String, Outcome]
  def issue(cfg, http, challenges): Either[String, Unit]
```

## What is in, and what is out

**HTTP-01 only.** The CA fetches
`http://<domain>/.well-known/acme-challenge/<token>` and expects
`<token>.<thumbprint>`. This stack owns a server, so serving that is
a `PartialFunction` chained in front of everything else. DNS-01 would
need a DNS provider seam this repository does not have, and wildcards
need DNS-01 — so no wildcards, said plainly rather than discovered.

**Staging is the default directory.** A first run against production
that gets the setup wrong burns a rate limit you wait a week to undo;
`OKAY_ACME_PROD=1` is the deliberate step out of the sandbox.

**One account key, one certificate key, kept.** The account key IS
the account: a new key is a new account, and a CA rate-limits those.
The certificate key is reused across renewals, which is what lets a
pinned key survive one. Both are PKCS#8 PEM beside the certificate.

**Not a certificate manager.** No fleet, no multiple orders, no
revocation, no EAB (the external-account binding some commercial CAs
require), no ARI (the renewal-window hint). Each of those is a real
thing a real manager does; a deployment that needs them runs certbot
or a proxy, and this module says so instead of growing.

## The parts, and why they are these parts

- **JWS, ACME's flavour.** Not a JWT: a flattened JSON serialization
  whose protected header carries `nonce` and `url`, and the account
  key as `jwk` on the FIRST request and as `kid` (the account URL the
  CA answered) on every one after. Signed with `Es256` — the DER↔JOSE
  conversion okay-security already owns.
- **The thumbprint** (RFC 7638) is a SHA-256 over a canonical JWK:
  exactly `{"crv":"P-256","kty":"EC","x":…,"y":…}`, members in
  lexicographic order, no whitespace. The shape is the specification,
  not a formatting choice, which is why it is built as a string.
- **Nonces.** Every POST spends one and the answer carries the next;
  the client keeps the last one it saw and asks `newNonce` only when
  it has none. A stale nonce is the CA's own retry story, and the
  client's job is not to hoard.
- **POST-as-GET.** RFC 8555 has no authenticated GET: reading a
  resource is a POST with an EMPTY payload. That surprises everyone
  once.
- **The CSR is openssl's.** No exported JDK API builds a PKCS#10
  (`sun.security.pkcs10` is not open), so the request is built by
  `openssl req -new`, and its absence is a NAMED refusal that points
  at the other road. Same decision, same reason, as the self-signed
  certificate in specs/tls.md.
- **Refusals are the CA's own sentence.** `application/problem+json`
  carries a `detail`; "the CA answered 403: the account is not
  authorized for this name" is something an operator can act on, and
  a status code is not.

## Testing

A fake CA in this process (`FakeCa`, test scope): a real HTTP server
speaking the protocol's shapes, which fetches the challenge back over
a real socket and signs the client's CSR with its own CA key. It
proves the state machine, the jwk-then-kid switch, the poll through
`pending`, and the HTTP-01 round trip.

**And Pebble, because a double checks our reading against our own
writing** (acme-pebble, 2026-09-07). Pebble is Let's Encrypt's own
small ACME server, deliberately strict, run in docker: our client
against someone else's implementation. It found a real bug in the
first run — `badNonce`.

The bug: `freshNonce` returned the `Replay-Nonce` of the HEAD it had
just made, while `send` had ALSO cached that same value from that
same response. The next POST spent the nonce again. Our own double
accepted a replayed nonce, so nothing before this could have caught
it; Pebble rejects one, which is exactly what a CA is supposed to do.
A nonce is now taken once — reading it clears the cache — and, per
§6.5, a `badNonce` answer is retried once with the fresh nonce the CA
sent along with it, which is what every client does and what our
tests now assert by simply passing.

Pebble's shape in the test: it generates its own CA per run, so the
certificate is pulled out with `docker cp` and trusted by a
test-scope `Http` over the JDK client (production transports
untouched, and `Acme` is proven to work over any `Http`); the domain
is `host.docker.internal` with `--add-host` so Linux behaves as
Docker Desktop does, and the challenge server binds the port Pebble's
own config validates against.

## Behavior

- [x] the whole flow against the fake CA: an account is created, an
      order placed, the challenge fetched over a socket, the CSR
      finalized, and a PEM chain lands on disk with the account and
      certificate keys beside it.
- [x] a second run with a valid certificate asks the CA for nothing
      (`Current`), and one whose remaining life is inside
      `renewBefore` renews (`Issued`).
- [x] a refusal carries the CA's own sentence.
- [x] the account key is generated once and reused; the canonical JWK
      is stable and in RFC 7638's order.
- [x] (Live, docker) PEBBLE issues a certificate to this client: a
      chain, for the name asked for, signed by an issuer that is not
      us — and a name it cannot reach is refused with the CA's own
      sentence, leaving no certificate behind.

## Wired into okay-script

`OKAY_ACME=<email>` with `OKAY_ACME_DOMAINS=a,b` (and
`OKAY_ACME_PROD=1` to leave staging). The certificate is asked for
BEFORE the server binds, so the first start is the one that earns it;
the issued pair is then read through `Tls.reloading`, so every later
renewal reaches the next connection without a restart. The challenge
is served on `OKAY_HTTP_PORT` — the plaintext port, ahead of the
https redirect, because a CA speaks plain HTTP and follows no
redirect for this. Without `OKAY_DATA` the account key lands in a
temp directory and a restart registers a NEW account, which the
caller is told on the way past rather than discovering in a rate
limit.
