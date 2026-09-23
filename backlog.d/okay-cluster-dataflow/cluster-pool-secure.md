- [ ] cluster-pool-secure — stage 4 of specs/cluster-pool.md: mTLS
      between members through okay-tls (one certificate reference on
      every target), an okay-security capability at the submission
      route checked BEFORE the Schema (a stranger learns nothing — the
      order `Cluster.guarded` already keeps), a NetworkPolicy on
      `cluster` admitting the pool port from the pool and the
      submitter's namespace only, and a pool told to listen on a
      non-loopback address with neither TLS nor a capability REFUSING
      to start unless `OKAY_POOL_INSECURE=true` — an open-by-default
      pool is the Spark REST server's CVE. Trigger: cluster-pool-process
      landed.
