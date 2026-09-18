- [ ] openapi-ops — okay-ops builds all four of its responses by hand,
      so its operations are the `undeclared` ones left in the
      repository. `/stats` is a straight `out` over `Store.Stats`,
      whose `Schema` it already summons to encode. `/metrics` is
      `media` with Prometheus's own content-type. `/healthz` and
      `/readyz` are the interesting pair and the shape of the next
      decision: their STATUS is chosen per request (200 or 503), and
      every declaring combinator fixes the status at declaration
      time — so they need either two declared answers and a handler
      that picks between them, or an answer type that carries its
      status. Worth deciding on that case rather than widening the
      whole surface for it.
