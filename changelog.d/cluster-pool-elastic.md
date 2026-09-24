## cluster-pool-elastic - peers re-resolved, a queued metric, Kubernetes/Consul leases

Stage 5 of specs/cluster-pool.md: a pool that changes size.
`Cluster.stream`/`leading` gain a `resolve` callback re-run at every
epoch boundary; a peer count that changed from what the attempt
started with ends it with `Cluster.Rescale` for a `Job.rescalable` job
(caught by `Pool.nudge`, which rewrites the run's stored `parts` and
restarts immediately) or is refused by name, unaffected, for one that
is not. `okay_pool_queued` is a new gauge behind `GET /metrics` on
`okay-pool` itself. `KubeLease` and `ConsulLease` are two
`okay.cluster.Lease` implementations behind `PoolConf.leaseKind` — the
ONLY place this engine ever speaks to a manager's API — plain
synchronous `java.net.http.HttpClient`, since `Lease`'s own trait has
no effect type and a blocking round trip per epoch is already the
documented cost of `held`.

Real infrastructure, not mocks, found two real defects on the first
run: `Instant.now().toString` is not Kubernetes's `metav1.MicroTime`
(exactly six fractional digits; a bare `Instant` omits the fraction
when nanos are zero and carries up to nine otherwise, and a real API
server answers a 400 naming the layout it wanted), and the JDK
`HttpClient`'s default HTTP/2-with-upgrade fails "invalid upgrade
response" against any HTTP/1.1-only server (`kubectl proxy`, Consul's
own API) the moment a request answers with an ordinary 2xx — fixed
by `microTime` and `.version(HttpClient.Version.HTTP_1_1)`
respectively. The exact Behavior-box claim — two members, one seat,
the deposed one's next commit throws `Checkpoint.Deposed` — is proven
against a real `kind` cluster with a raw, second HTTP write standing
in for a genuine competitor.

Also caught by `TestClusterElastic`, before it ever reached okay-pool:
the rescale trigger's first cut compared the resolved peer count
against `parts`, not `workers.length` — the two are independent by
design (a partition already runs on worker `i % workers.length`), and
the mistake broke six of `TestRescale`'s own existing tests
immediately.

203 tests across `okay-cluster`/`okay-ops`/`okay-pool`'s default
suites (fresh `clean` first, JVM and JS), clean compile, thirteen real
`Live` tests (seven against `kind`, six against Consul).
