package okay.pool

import okay.codec.Schema
import okay.conf.Conf

/**
 * A pool member's whole configuration, as ONE value with its own
 * defaults (specs/cluster-pool.md, stage 1) — `okay-script`'s
 * `Serve.Config` is the shape this copies: the names an environment
 * answers to are DERIVED from these fields (`Conf.envName`), so a
 * field renamed here is renamed for every deployment at once.
 */
final case class PoolConf(
  /** the worker protocol's own port — a raw socket, `Cluster.Serve`
   * on the wire, never HTTP */
  port: Int = 7100,
  /** the HTTP door: submissions, status, the probes. A SEPARATE port
   * from `port` on purpose — two different protocols cannot share one
   * listener */
  httpPort: Int = 7101,
  /** a name `Discovery` resolves to this pool's peers; "" = the
   * static list alone */
  service: String = "",
  /** "host:port,host:port" — a static list, UNIONED with whatever
   * `service` resolves to, never in place of it */
  peers: String = "",
  /** comma-separated class names whose loading registers the jobs
   * this build can run — `WorkerMain`'s own convention */
  registrars: String = "",
  /** a class name whose loading registers ONE `(runId: String) =>
   * (Checkpoint, Lease)` factory (`Stores.set`) every submission's
   * journal is opened through. "" = the in-memory default, which
   * `Pool.main` refuses once more than one peer is configured — see
   * specs/cluster-pool.md, "This needs a shared store, and says so" */
  store: String = "",
  /** consecutive failures that bury a peer (`dataflow-reconnect`) */
  tolerance: Int = 3,
  /** an opaque identifier for this build. Compared against a peer's
   * own before handing it work — see `Pool.workers` — so a rolling
   * update mixing two artifacts fails loudly rather than mixing
   * answers inside one run. "" (the default) turns the check off. */
  build: String = "",
  /** answer the worker protocol MEASURED (specs/dataflow.md, stage
   * 15): every answer says the rows it read and the time it spent,
   * and how much of it in foreign code, so the coordinating member's
   * `/metrics` and traces can split a run's time into engine, foreign
   * and wire. Costs one extra encoding of each answer; false turns it
   * off and the coordinator still sees round trips. */
  measured: Boolean = true,
  /** the pool's ONE shared certificate — mTLS between members
   * (specs/cluster-pool.md, stage 4): every member is handed the
   * SAME cert and key, so a connection is authenticated by "does the
   * peer hold what I hold", not by a CA. "" turns TLS off on the
   * worker protocol; `tlsKey` must be set together with it. */
  tlsCert: String = "",
  tlsKey: okay.conf.Secret = okay.conf.Secret(""),
  /** the root key an `Authorization: Bearer` capability on
   * `POST /pool/jobs/{name}` is checked against (okay-security's
   * `Capability`). "" turns the check off. */
  capabilityKey: okay.conf.Secret = okay.conf.Secret(""),
  /** the explicit override: without mTLS or a capability configured,
   * `Pool.run` refuses to start — an open-by-default pool is the
   * Spark REST server's own CVE. Set this to mean it. */
  insecure: Boolean = false,
  /** which manager's own primitive backs the coordinator lease
   * (specs/cluster-pool.md, stage 5) — "" (the default) is
   * `Lease.solitary`, correct for a manager with no election of its
   * own (Slurm, YARN) and for a pool with only one member ever
   * leading. "kube" is a `coordination.k8s.io/v1 Lease`; "consul" is
   * a session over Consul's KV store. THE ONLY PLACE this engine ever
   * speaks to a manager's API. */
  leaseKind: String = "",
  /** the Lease object's name (kube) or the KV key (consul) — every
   * member of one pool names the SAME one, so they contend for it */
  leaseName: String = "okay-pool",
  /** kube only: the namespace the Lease object lives in. "" reads
   * this pod's own namespace from its ServiceAccount files. */
  leaseNamespace: String = "",
  /** the API server's base URL (kube) or the agent's base URL
   * (consul). "" means the in-cluster default for "kube"
   * (`$KUBERNETES_SERVICE_HOST`) and the local agent for "consul"
   * (`http://127.0.0.1:8500`) — set for a `kubectl proxy` or a remote
   * agent, which is also how a test points either one at a real
   * instance with no token and no TLS. */
  leaseUrl: String = "",
  /** kube only: this pod's bearer token, as a Secret reference. ""
   * reads the in-cluster ServiceAccount token file, which is what
   * every pod already carries without this being set at all. */
  leaseToken: okay.conf.Secret = okay.conf.Secret(""),
  /** how long this member's hold is good for before another may take
   * it over, once renewal stops arriving */
  leaseSeconds: Int = 15,
) derives Schema

object PoolConf:
  /** the prefix every one of this program's variables carries:
   * `OKAYPOOL_PORT`, `OKAYPOOL_SERVICE`, and so on */
  val prefix = "okaypool"

  /** defaults, then the file `OKAYPOOL_CONF` names, then the
   * environment (specs/conf.md's layering, `okay-script`'s own
   * `Serve.parse` shape) */
  def load(
    env: String => Option[String] = k => Option(System.getenv(k)),
    slurp: String => Either[String, String] = p =>
      try Right(java.nio.file.Files.readString(java.nio.file.Paths.get(p)))
      catch case e: Exception => Left(Option(e.getMessage).getOrElse(e.getClass.getSimpleName)),
  ): Either[String, PoolConf] =
    val file = env("OKAYPOOL_CONF").filter(_.nonEmpty)
    for
      text <- file match
        case None => Right(None)
        case Some(path) => slurp(path).left.map(m => s"OKAYPOOL_CONF names $path and it could not be read: $m").map(Some(_))
      conf <- Conf.layered(PoolConf(), text, env, prefix)
    yield conf
