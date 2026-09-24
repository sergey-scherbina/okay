package okay.pool

import okay.*
import okay.given
import okay.cluster.{Checkpoint, Cluster, Folded, Job, Jobs, Lease, Req, Resp, Served}
import okay.codec.{Codecs, Json}
import okay.conf.Schemes
import okay.resilience.{Discovery, DiscoveryJvm, Endpoint}
import okay.jetty.Jetty
import java.net.{ServerSocket, Socket}

/**
 * A POOL MEMBER (specs/cluster-pool.md, stage 1): every member serves
 * partitions on the worker protocol AND takes submissions over HTTP,
 * and whichever one accepts a submission coordinates it under
 * `Cluster.leading` — never a bare `Cluster.run`, so the member that
 * accepted it dying is a resume by any other member (see `statusOf`
 * and `nudge` below, and specs/cluster-pool.md's "the run id is the
 * journal name").
 */
object Pool:

  /** discovery ∪ the static list, deduplicated — what `/pool/peers`
   * reports and what `workers` builds `Cluster.Serve`s from */
  def resolve(conf: PoolConf, discovery: Discovery): Vector[Endpoint] ! Async =
    val static = Discovery.parse(conf.peers)
    if conf.service.isEmpty then pure(static)
    else discovery.resolve(conf.service).map(ds => (ds ++ static).distinct)

  /**
   * THE WORKERS A COORDINATOR ON THIS MEMBER SEES: itself
   * (`Cluster.local`, in-process — no socket to itself) plus every
   * discovered/static peer whose build fingerprint agrees with this
   * one's, each a `Served.reconnecting` (heals across a peer's
   * restart, `dataflow-reconnect`).
   *
   * A peer whose build disagrees is not a failure worth burying — it
   * is simply not offered the work, so a rolling update fails loudly
   * (this run answers from ONE build only) rather than mixing two
   * versions inside one answer.
   */
  def workers(conf: PoolConf, discovery: Discovery): Vector[Cluster.Serve] ! Async =
    resolve(conf, discovery).map { es =>
      val remote = es.flatMap { e =>
        val s = Served.reconnecting(e.host, e.port, connect = connectOf(conf))
        if agrees(conf.build, s, e) then Some(s) else None
      }
      Cluster.local +: remote
    }

  /** the plain dial, or the pool's own mTLS wrapped around it
   * (specs/cluster-pool.md, stage 4) — `Served.reconnecting` knows
   * nothing about TLS and does not need to; this is the ONE place a
   * dial becomes an authenticated one */
  private def connectOf(conf: PoolConf): (String, Int) => Socket =
    if conf.tlsCert.isEmpty then Served.plainSocket
    else (host, port) =>
      val plain = Served.plainSocket(host, port)
      okay.tls.Tls.mutualClient(plain, host, conf.tlsCert, conf.tlsKey, Schemes.all()) match
        case Right(s) => s
        case Left(why) => throw java.io.IOException(s"mTLS to $host:$port failed: $why")

  /** `Req.Known` doubles as the build handshake: `Resp.Names.build` is
   * "" from plain `Cluster.local` and stamped by `fingerprinted`
   * below, so the check is opt-in by `conf.build` being non-empty */
  private def agrees(build: String, peer: Cluster.Serve, e: Endpoint): Boolean =
    if build.isEmpty then true
    else
      try peer(Req.Known) match
        case Resp.Names(_, theirs) if theirs.nonEmpty && theirs != build =>
          System.err.println(s"okay-pool: excluding ${e.authority} — its build '$theirs' disagrees with this one's '$build'")
          false
        case _ => true
      catch case _: Throwable => true   // a peer that is simply gone: the run's own retry/bury handles it

  /** the worker protocol's serving side, stamped with this member's
   * build — what lets a REMOTE coordinator's `workers` exclude this
   * member if it is running a different artifact */
  def fingerprinted(build: String): Cluster.Serve =
    case Req.Known =>
      Cluster.local(Req.Known) match
        case Resp.Names(names, _) => Resp.Names(names, build)
        case other => other
    case req => Cluster.local(req)

  /**
   * ONE ATTEMPT AT A TIME, PER PROCESS. `Cluster.leading` is already
   * safe and cheap to call whenever asked — `None` when somebody else
   * holds the seat — so this guard exists only to stop THIS process
   * from re-asking its own lease on every poll of a run it is already
   * driving (specs/cluster-pool.md, "A GET resumes the run; nothing
   * sweeps for it" — deliberately not a background sweep, so a DIFFERENT
   * process is free to attempt independently at any time).
   */
  private object Attempts:
    private val active = scala.collection.mutable.Set.empty[String]
    def start(id: String): Boolean = synchronized { if active(id) then false else { val _ = active.add(id); true } }
    def stop(id: String): Unit = synchronized { active.remove(id): Unit }

  /** fire this run's leading program in the background; the caller
   * never awaits it — the NEXT read of the checkpoint (a `GET`, from
   * anyone) is how its progress becomes visible */
  def nudge(id: String, job: Job[?, ?], m: RunMeta, peers: Vector[Cluster.Serve],
           checkpoint: Checkpoint, lease: Lease)(using Scheduler): Unit =
    if Attempts.start(id) then
      job.lead(m.params, m.parts, peers, effectiveTake(m.take), checkpoint, lease) match
        case Left(_) => Attempts.stop(id)   // params were validated at submission; unreachable here
        case Right(prog) =>
          val f = Async.spawn(prog)
          f.onComplete(_ => Attempts.stop(id))

  /** `take <= 0` means "one epoch, run to completion" — the client-
   * facing word for a batch job over the SAME epoch loop a genuinely
   * unbounded submission uses (specs/cluster-pool.md) */
  def effectiveTake(take: Int): Int = if take <= 0 then Int.MaxValue else take

  private def decodeFolded(bytes: Array[Byte]): Option[Folded] =
    Codecs.cbor(Folded.given_Schema_Folded).decode(bytes).toOption

  /**
   * THE STATUS OF A RUN, READ FROM ITS JOURNAL ALONE — never from a
   * live process's memory, so any member answers the same way
   * (specs/cluster-pool.md, "the run id is the journal name"). A run
   * not yet finished is nudged before this answers, so the NEXT read
   * — from anyone — sees progress even when the member that accepted
   * the original submission is gone. `None` here means the id names
   * no run this store has ever heard of.
   */
  def statusOf(id: String, conf: PoolConf, discovery: Discovery, store: String => (Checkpoint, Lease))
              (using Scheduler): Option[Status] ! Async =
    val (metaCk, _) = store(s"$id.meta")
    metaCk.latest match
      case None => pure(None)
      case Some((_, metaBytes)) =>
        RunMeta.decode(metaBytes) match
          case Left(why) => pure(Some(Status.Failed(s"a stored run record for '$id' is corrupt: $why")))
          case Right(m) =>
            Jobs.find(m.job) match
              case None => pure(Some(Status.Failed(s"job '${m.job}' is not known to this build")))
              case Some(job) =>
                val (checkpoint, lease) = store(id)
                val folded = checkpoint.latest.flatMap((_, bytes) => decodeFolded(bytes))
                folded.filter(_.done) match
                  case Some(f) =>
                    pure(Some(job.answerOf(m.params, f) match
                      case Right(a) => Status.Done(a.value, a.dropped, a.merged, a.retried, a.failed)
                      case Left(why) => Status.Failed(s"the journal for '$id' no longer matches job '${m.job}': $why")))
                  case None =>
                    workers(conf, discovery).map { peers =>
                      nudge(id, job, m, peers, checkpoint, lease)
                      Some(Status.Running(folded.map(_.epoch).getOrElse(0), peers.length))
                    }

  /**
   * ACCEPT A SUBMISSION: validate the job and its parameters at the
   * door — the same order `Cluster.guarded`/`schemaChecked` already
   * keep — then write the run's record ONCE and nudge it the same way
   * a `GET` would. A repeat `POST` of an id ALREADY on record reuses
   * the STORED record rather than the new body: idempotent without
   * ever asking whether the two bodies agree.
   */
  def submit(name: String, params: Json, parts: Int, take: Int, journal: String,
            conf: PoolConf, discovery: Discovery, store: String => (Checkpoint, Lease))
           (using Scheduler): Either[(Int, String), Submitted] ! Async =
    Jobs.find(name) match
      case None => pure(Left((404, s"no job named '$name'; this build knows ${Jobs.names.mkString(", ")}")))
      case Some(job) =>
        Codecs.json(job.params).decode(params) match
          case Left(why) => pure(Left((400, s"parameters for '$name': $why")))
          case Right(_) =>
            val id = if journal.nonEmpty then journal else s"$name-${java.util.UUID.randomUUID()}"
            val (checkpoint, lease) = store(id)
            val (metaCk, _) = store(s"$id.meta")
            metaCk.latest match
              case Some((_, existing)) =>
                RunMeta.decode(existing) match
                  case Right(m) if m.job == name =>
                    workers(conf, discovery).map { peers => nudge(id, job, m, peers, checkpoint, lease); Right(Submitted(id)) }
                  case Right(m) =>
                    pure(Left((409, s"'$id' already names a run of '${m.job}', not '$name'")))
                  case Left(why) =>
                    pure(Left((500, s"'$id' already names a run, and its record is corrupt: $why")))
              case None =>
                workers(conf, discovery).map { peers =>
                  val resolvedParts = if parts > 0 then parts else peers.length
                  val m = RunMeta(name, params, resolvedParts, take)
                  metaCk.save(0, RunMeta.encode(m))
                  nudge(id, job, m, peers, checkpoint, lease)
                  Right(Submitted(id))
                }

  /** `<a>,<b>,<c>` -> the non-empty names, trimmed */
  private def list(s: String): Vector[String] = s.split(",").toVector.map(_.trim).filter(_.nonEmpty)

  /** `PoolConf` from defaults -> file -> environment; the worker
   * protocol and the HTTP door on their own ports */
  def main(args: Array[String]): Unit =
    PoolConf.load() match
      case Left(msg) => System.err.println(s"okay-pool: $msg"); System.exit(2)
      case Right(conf) => run(conf)

  def run(conf: PoolConf)(using okay.CanBlock, Scheduler): Unit =
    for name <- list(conf.registrars) do Class.forName(name): Unit
    if conf.store.nonEmpty then Class.forName(conf.store): Unit
    val store = Stores.get.getOrElse {
      System.err.println("okay-pool: PoolConf.store is unset — running on the in-memory default, which only works alone")
      Stores.memory
    }
    // "any member can answer a GET" is exactly what the in-memory
    // default cannot promise once there is more than one member
    // (specs/cluster-pool.md, "This needs a shared store, and says so")
    if store == Stores.memory && (conf.service.nonEmpty || conf.peers.nonEmpty) then
      System.err.println("okay-pool: PoolConf.store is unset but peers are configured — refusing to start; " +
        "a real pool needs a shared Checkpoint/Lease store, not this process's own memory")
      System.exit(3)
    else if !secured(conf) then
      System.err.println("okay-pool: neither tlsCert/tlsKey nor capabilityKey is configured — refusing to " +
        "listen; an open-by-default pool is the Spark REST server's own CVE. Set OKAY_POOL_INSECURE=true " +
        "to run this way on purpose (a loopback-only dev pool, say).")
      System.exit(3)
    else
      val discovery = Discovery.chain(DiscoveryJvm.env(), DiscoveryJvm.dns(conf.port))
      var ready = false
      val router = Routes.router(conf, discovery, store, () => ready)
      val socket = serverSocketOf(conf)
      okay.Threads.spawn("okay-pool-worker")(() => Served.serve(socket, fingerprinted(conf.build)))
      ready = true
      val serving = Jetty.serve(conf.httpPort)(router.routes)()
      Resource.run[Unit, Pure](serving.map { s =>
        println(s"okay-pool: worker protocol on ${socket.getLocalPort}${if conf.tlsCert.nonEmpty then " (mTLS)" else ""}, " +
          s"http on ${Jetty.port(s)}, jobs [${Jobs.names.mkString(", ")}]")
        Thread.sleep(Long.MaxValue)
      }).runWith

  /** mTLS on the worker protocol, or a capability at the submission
   * door, or the explicit override — never silence (specs/cluster-pool.md,
   * stage 4) */
  private[pool] def secured(conf: PoolConf): Boolean =
    conf.tlsCert.nonEmpty || conf.capabilityKey.ref.nonEmpty || conf.insecure

  private def serverSocketOf(conf: PoolConf): ServerSocket =
    if conf.tlsCert.isEmpty then ServerSocket(conf.port)
    else okay.tls.Tls.mutualServerSocket(conf.port, conf.tlsCert, conf.tlsKey, Schemes.all()) match
      case Right(ss) => ss
      case Left(why) =>
        System.err.println(s"okay-pool: the worker protocol's mTLS did not build: $why")
        System.exit(3)
        throw IllegalStateException("unreachable")
