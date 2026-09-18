- [ ] microservices-next — the audit's remaining gaps, each its own
      spec when picked. DONE 2026-09-09 (service-lifecycle): graceful
      shutdown and RED metrics, both in okay-ops. DONE 2026-09-09
      (outbox): transactional outbox / inbox / dead-letter as
      okay-outbox (specs/outbox.md). DONE 2026-09-09 (discovery):
      service discovery + client-side balancing in okay-resilience
      (specs/discovery.md). DONE 2026-09-09 (schema-compat): Schema
      compatibility between services, `okay.codec.Compat`
      (specs/codecs.md). DONE 2026-09-09 (obs-log): a Log effect with
      trace correlation, `okay.obs.Log` (specs/obs.md, "The third
      leg") — the audit's list is now closed except: saga over `Durable`
      + persist with compensations as values; transactional outbox /
      inbox / dead-letter when the truth is in SQL; service discovery
      + client-side balancing (cluster.md lists it out of scope);
      Schema compatibility checks between services; a `Log` effect
      with trace correlation (0 hits for one today).
      (was filed under "resilience" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)
