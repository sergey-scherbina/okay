# Service discovery and client-side balancing

## Overview

`Resilient.http` (specs/resilience.md) hardens one call, and every
call goes to the one host in its URL. A service in Kubernetes is N
pods behind a name; specs/cluster.md lists "membership/discovery
beyond static configuration" as out of scope and nothing since has
taken it: nothing here turns a name into addresses, picks one,
skips one that just failed, or spreads calls across them. This spec
adds the smallest such thing, as one more handler around `Http`.

Two decisions shape it. The first: discovery is a SEAM (`Discovery`:
a name to a vector of endpoints) with three sources that cover where
a service actually runs — a static table (tests, compose files),
the environment (Kubernetes writes `<NAME>_SERVICE_HOST` and
`_PORT` into every pod; a comma list `OKAY_SERVICE_<NAME>` covers
everything else), and DNS (a headless Service is N A-records; JVM
only, since the browser has no resolver API). A registry of our own
is NOT here: the log-backed election in okay-persist is the shape
that would take, and it is a different spec.

The second: balancing is a URL REWRITE. The program says
`http://orders/v1/…`; `Balanced.http` asks the discovery for
`orders`, picks an endpoint round-robin among those not cooling
down, and sends `http://10.0.3.7:8080/v1/…`. A host the discovery
does not know passes through unchanged, so one client serves service
calls and the outside world alike. A thrown wire error marks the
endpoint down for a cool-down; an answered 5xx does not — the far
end spoke, and what to do about that is the breaker's decision, one
layer out. When every endpoint is cooling down the least recently
failed one is tried rather than nothing — a cool-down is a
preference, not a verdict; only an EMPTY resolution refuses, with
`Refused.NoEndpoint`.

## Interface

```scala
package okay.resilience

final case class Endpoint(host: String, port: Int)

trait Discovery:
  def resolve(service: String): Vector[Endpoint] ! Async

object Discovery:
  def static(table: Map[String, Vector[Endpoint]]): Discovery
  /** `<NAME>_SERVICE_HOST`/`_PORT` (Kubernetes; name upper-cased, `-` → `_`),
    * then `OKAY_SERVICE_<NAME>` = "host:port,host:port" */
  def env(lookup: String => Option[String]): Discovery
  /** the first source with an answer wins */
  def chain(sources: Discovery*): Discovery
  /** answers remembered for `ttlMillis` per name — for DNS */
  def cached(inner: Discovery, ttlMillis: Long, clock: () => Long = wall): Discovery
  /** "h1:p1,h2:p2"; a damaged entry is dropped, not guessed */
  def parse(list: String, defaultPort: Int = 80): Vector[Endpoint]

object DiscoveryJvm:                           // JVM only (scala-jvm)
  def env(): Discovery                         // Discovery.env over sys.env
  def dns(port: Int): Discovery                // every A/AAAA record of the name

object Balanced:
  final case class Stats(picked: Long, failed: Long, refused: Long,
                         down: Vector[String]) derives Schema

final class Balanced(discovery: Discovery, cooldownMillis: Long = 5_000,
                     clock: () => Long = wall) extends Reporting[Balanced.Stats]:
  def http(inner: Http): Http

object Refused:
  final class NoEndpoint(service: String) extends Refused   // retryAfter None
```

## Behavior

- [x] `parse` reads "a:1,b:2", applies the default port to a bare
      host, drops a damaged entry and keeps the rest
- [x] `env`: `ORDERS_SERVICE_HOST`/`PORT` answer for `orders`; the
      comma list answers when Kubernetes's pair is absent; a name
      with a dash maps to underscores; nothing set answers empty
- [x] `chain` takes the first non-empty answer; `cached` asks the
      inner once per name within the ttl and again after it (the
      clock is the test's)
- [x] `Balanced.http`: `http://orders/v1/x?q` goes to the endpoints
      in turn (round-robin), the path and query kept, the scheme
      kept; a host the discovery does not know passes through
      unchanged
- [x] a thrown wire error marks that endpoint down for the cool-down
      and the next call goes elsewhere; after the cool-down it is
      tried again; an answered 503 marks nothing
- [x] all endpoints down: the least recently failed is tried, not
      refused; an EMPTY resolution refuses with `Refused.NoEndpoint`
      and `Resilient.route` maps it to 503
- [x] stats count picks, failures, refusals and name what is down
- [x] JVM: `dns(port)` resolves `localhost` to at least one endpoint
      with that port; an unknown name answers empty, not a throw
- [x] docs: the okay-resilience page gains the discovery section

## Out of scope

- A registry of our own (register on start, heartbeat, deregister):
  that is the persist election's shape and its own spec.
- SRV records, weights, zones, sticky sessions — none has a consumer
  here; round-robin with a cool-down is what every first balancer
  ships with.
- Rewriting the `Host` header: pods do not need it, and a gateway
  that does is configured by the operator, who can set it on the
  request.

## Design

**One cell per balancer**: the round-robin cursor per name and the
cool-down per endpoint live in one `TRef` map moved by one `modify`,
the okay-resilience shape. **Time injected**, so a cool-down expiry
is a clock jump in a test. **Failure = a throw.** A `Response` is the
far end's answer, whatever its status; the layers around (breaker,
retry) act on statuses. **The DNS half is JVM-only source**
(`scala-jvm`), so the shared module keeps no `InetAddress`.

## Decisions

- **In okay-resilience, not a module** — it is one more handler
  around `Http`, shares `Refused`, `Reporting` and the one-cell
  style, and `Resilient.http` composes it (balancing innermost of
  the deadline, outermost of the breaker: a breaker per SERVICE, the
  endpoint chosen under it).
- **URL rewrite, not a `Request` field** — the program stays blind;
  a service name in the URL is what every platform's DNS-based
  discovery already assumes, and the same URL works unbalanced
  against a real DNS name.
- **Cool-down is a preference** — a balancer that refuses when every
  pod hiccuped at once turns a blip into an outage; trying the least
  recently failed one is what the rest of the stack would do next.

## Results

**Landed (discovery, 2026-09-09).** `Discovery` (static, env, chain,
cached, parse), `DiscoveryJvm` (env over `sys.env`, dns), `Balanced`,
`Refused.NoEndpoint` (503), `Resilient.http(..., balanced)` with the
balancer innermost. `TestDiscovery` (6, shared — green on JS
unchanged) and `TestDiscoveryJvm` (2). One thing the first test
draft had wrong and the code had right: round-robin keeps ONE
cursor per service over the LIVING pool, so after an endpoint is
cooled down the sequence continues from where the cursor was, not
from the head of the shrunken pool — the test now says so.
