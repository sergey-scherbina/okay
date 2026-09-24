package okay.deploy

import okay.codec.Json

import java.nio.file.Path

/**
 * THE OTHER MANAGERS (specs/cluster-pool.md, stage 3): every one of
 * these renders as a pure function, exactly the shape `laptop`/
 * `host`/`cluster`/the clouds already are, and every one is gated by
 * a REAL parser or validator for its own format — never a golden
 * file, the lesson stage 1 of specs/deployment.md's own cluster
 * target already paid for.
 *
 * SCOPED to what a pool actually needs, not full parity with
 * `cluster`: `Need.Port`, `Need.Peers`, `Settings`, and a named
 * refusal for `Need.Database`/`Need.Cache` (none of these five runs a
 * managed stateful service any more than `host` does). Secrets are
 * OUT of scope for this landing — each of these platforms has its own
 * secret story (Vault for Nomad, `docker secret` for Swarm, Secrets
 * Manager for Batch, nothing native for YARN or Slurm) and wiring
 * five more is a lane of its own; a service with `secrets.nonEmpty`
 * is refused by name rather than silently dropping them.
 */
object Managers:

  private def str(s: String): String = "\"" + s.replace("\\", "\\\\").replace("\"", "\\\"") + "\""
  private def imageOf(d: Deployment, s: Service): String = s.run match
    case Run.Image(repo, tag) => s"$repo:$tag"
    case _: Run.Module => s"${d.name}/${s.name}:local"

  /** every one of these five refuses the same two things, for the
   * same reason `host` does — installing and running someone's
   * database is not a renderer's business — and refuses a secret
   * this landing does not wire */
  private def unsupported(d: Deployment, managerName: String): Vector[String] =
    d.services.flatMap { s =>
      s.databases.map(db => s"${s.name} needs a ${db.engine} and $managerName does not run one") ++
        s.caches.map(c => s"${s.name} needs a ${c.engine} and $managerName does not run one") ++
        (if s.secrets.nonEmpty then Vector(s"${s.name} reads a secret and $managerName's own secret " +
          "story is not wired here yet — point it at one you provide another way") else Vector.empty)
    }

  private def refusalOf(d: Deployment, managerName: String): Option[String] =
    val bad = unsupported(d, managerName)
    if bad.isEmpty then None
    else Some(bad.mkString("; ") + " — point the service at one you run, or use the laptop/cluster target")

  // ====================================================================
  // Nomad — the Services API's own JSON, so `nomad job run -json`
  // accepts exactly what this renders; no HCL parser is needed and
  // none is assumed on the machine writing this (specs/cluster-pool.md)
  // ====================================================================

  object Nomad extends Target:
    val name = "nomad"

    def requires(d: Deployment): Vector[String] = Vector("nomad")
    def up(dir: Path): Vector[String] = Vector("nomad", "job", "run", "-json", dir.resolve("job.json").toString)
    def down(dir: Path): Vector[String] = Vector("nomad", "job", "stop", jobId(dir))

    private def jobId(dir: Path): String =
      Option(dir.getParent).flatMap(p => Option(p.getParent)).map(_.getFileName.toString).getOrElse("okay")

    def render(d: Deployment): Either[String, Vector[(String, String)]] =
      refusalOf(d, "nomad") match
        case Some(why) => Left(why)
        case None => d.ordered.map { services =>
          Vector("job.json" -> Json.print(job(d, services)))
        }

    private def job(d: Deployment, services: Vector[Service]): Json =
      Json.JObj(Vector("Job" -> Json.JObj(Vector(
        "ID" -> Json.JStr(d.name),
        "Name" -> Json.JStr(d.name),
        "Type" -> Json.JStr("service"),
        "Datacenters" -> Json.JArr(Vector(Json.JStr("dc1"))),
        "TaskGroups" -> Json.JArr(services.map(group(d, _)))))))

    private def group(d: Deployment, s: Service): Json =
      val fields = scala.collection.mutable.ArrayBuffer[(String, Json)](
        "Name" -> Json.JStr(s.name),
        "Count" -> Json.JNum(s.scale.replicas.toDouble))
      if s.ports.nonEmpty then
        fields += "Networks" -> Json.JArr(Vector(Json.JObj(Vector(
          "DynamicPorts" -> Json.JArr(s.ports.map(p =>
            Json.JObj(Vector("Label" -> Json.JStr(s"p${p.number}"), "To" -> Json.JNum(p.number.toDouble)))))))))
        // Nomad's OWN service discovery (1.3+, no Consul needed): a
        // lookup on `<service>.service.nomad` answers every healthy
        // instance, which is exactly what a pool's Discovery wants
        fields += "Services" -> Json.JArr(s.ports.map(p =>
          Json.JObj(Vector(
            "Name" -> Json.JStr(s.name),
            "PortLabel" -> Json.JStr(s"p${p.number}"),
            "Provider" -> Json.JStr("nomad")))))
      fields += "Tasks" -> Json.JArr(Vector(task(d, s)))
      Json.JObj(fields.toVector)

    private def task(d: Deployment, s: Service): Json =
      val env = s.settings.env ++
        (if s.peers then Vector("OKAY_POOL_SERVICE" -> s"${s.name}.service.nomad") else Vector.empty)
      val config = scala.collection.mutable.ArrayBuffer[(String, Json)]("image" -> Json.JStr(imageOf(d, s)))
      if s.ports.nonEmpty then config += "ports" -> Json.JArr(s.ports.map(p => Json.JStr(s"p${p.number}")))
      Json.JObj(Vector(
        "Name" -> Json.JStr(s.name),
        "Driver" -> Json.JStr("docker"),
        "Config" -> Json.JObj(config.toVector),
        "Env" -> Json.JObj(env.map((k, v) => k -> Json.JStr(v)))))

  // ====================================================================
  // YARN — the Services API's own JSON (a Yarnfile, Hadoop 3.1+); the
  // same reasoning as Nomad, a real format with no subprocess needed
  // to check it is well-formed
  // ====================================================================

  object Yarn extends Target:
    val name = "yarn"

    def requires(d: Deployment): Vector[String] = Vector("yarn")
    def up(dir: Path): Vector[String] = Vector("yarn", "app", "-launch", d0(dir), dir.resolve("Yarnfile").toString)
    def down(dir: Path): Vector[String] = Vector("yarn", "app", "-destroy", d0(dir))

    private def d0(dir: Path): String =
      Option(dir.getParent).flatMap(p => Option(p.getParent)).map(_.getFileName.toString).getOrElse("okay")

    def render(d: Deployment): Either[String, Vector[(String, String)]] =
      refusalOf(d, "yarn") match
        case Some(why) => Left(why)
        case None => Right(Vector("Yarnfile" -> Json.print(yarnfile(d))))

    private def yarnfile(d: Deployment): Json =
      Json.JObj(Vector(
        "name" -> Json.JStr(d.name),
        "version" -> Json.JStr("1.0.0"),
        "components" -> Json.JArr(d.services.map(component(d, _)))))

    private def component(d: Deployment, s: Service): Json =
      val env = s.settings.env ++
        // the registry DNS YARN Services gives every component --
        // resolvable inside the SAME cluster's own zone, which this
        // model cannot name (it is a cluster-operator fact, not a
        // deployment one): a template, not a guess, the same honesty
        // `host`'s own peer list already keeps
        (if s.peers then
          Vector("OKAY_POOL_SERVICE" -> s"${s.name}.${d.name}.$$USER.<your cluster's DNS zone>")
        else Vector.empty)
      Json.JObj(Vector(
        "name" -> Json.JStr(s.name),
        "number_of_containers" -> Json.JNum(s.scale.replicas.toDouble),
        "launch_command" -> Json.JStr(launch(d, s)),
        "resource" -> Json.JObj(Vector("cpus" -> Json.JNum(1), "memory" -> Json.JStr("512"))),
        "configuration" -> Json.JObj(Vector("env" -> Json.JObj(env.map((k, v) => k -> Json.JStr(v)))))))

    private def launch(d: Deployment, s: Service): String = s.run match
      case m: Run.Module =>
        val opts = if m.javaOpts.isBlank then "" else m.javaOpts.trim + " "
        s"java $opts-cp app.jar ${m.mainClass}"
      case Run.Image(_, _) => s"docker run --rm ${imageOf(d, s)}"

  // ====================================================================
  // Slurm — one `sbatch` script for N tasks of the ONE peers service a
  // pool actually is; the peer list is not guessed, because Slurm
  // KNOWS every task's real hostname at allocation time
  // (specs/cluster-pool.md: "the peer list from `scontrol show
  // hostnames $SLURM_JOB_NODELIST`, written before the members start")
  // ====================================================================

  object Slurm extends Target:
    val name = "slurm"

    def requires(d: Deployment): Vector[String] = Vector("sbatch")
    def up(dir: Path): Vector[String] = Vector("sbatch", dir.resolve("job.sbatch").toString)
    // scancel takes a NUMERIC job id sbatch only answers at submit
    // time, which this renderer never has -- --name is the one thing
    // rendered here, and it is what the job carries
    def down(dir: Path): Vector[String] = Vector("scancel", "--name", jobName(dir))

    private def jobName(dir: Path): String =
      Option(dir.getParent).flatMap(p => Option(p.getParent)).map(_.getFileName.toString).getOrElse("okay")

    def render(d: Deployment): Either[String, Vector[(String, String)]] =
      refusalOf(d, "slurm") match
        case Some(why) => Left(why)
        case None =>
          val pooled = d.services.filter(_.peers)
          if pooled.length > 1 then
            Left(s"slurm renders ONE job for ONE pool per deployment; " +
              s"${pooled.map(_.name).mkString(", ")} all ask for Need.Peers — split them into separate deployments")
          else d.services match
            case Vector(s) => Right(Vector("job.sbatch" -> script(d, s)))
            case _ => Left("slurm renders one service at a time; this deployment has more than one")

    private def script(d: Deployment, s: Service): String =
      val n = math.max(1, s.scale.replicas)
      val exec = s.run match
        case m: Run.Module =>
          val opts = if m.javaOpts.isBlank then "" else m.javaOpts.trim + " "
          s"java $opts-cp app.jar ${m.mainClass}"
        case Run.Image(_, _) => s"docker run --rm --network host ${imageOf(d, s)}"
      val exports = s.settings.env.map((k, v) => s"export $k=${str(v)}").mkString("\n")
      s"""#!/bin/sh
         |# generated by okay-deploy from ${d.name}'s Deployment value — edit the value, not this file
         |#SBATCH --job-name=${d.name}
         |#SBATCH --ntasks=$n
         |#SBATCH --ntasks-per-node=1
         |
         |$exports
         |${if s.peers then
             "# the real hostnames, known only now -- never guessed\n" +
             "export OKAY_POOL_PEERS=$(scontrol show hostnames \"$SLURM_JOB_NODELIST\" | paste -sd, -)"
           else ""}
         |srun $exec
         |""".stripMargin

  // ====================================================================
  // Swarm — the SAME compose shape `laptop` renders, in swarm mode:
  // `tasks.<service>` is Swarm's OWN internal DNS name for every one
  // of a service's replicas, distinct from the plain service name
  // (which load-balances through a single virtual IP)
  // ====================================================================

  object Swarm extends Target:
    val name = "swarm"

    def requires(d: Deployment): Vector[String] = Vector("docker")
    def up(dir: Path): Vector[String] = Vector("docker", "stack", "deploy", "-c", dir.resolve("compose.yaml").toString, d0(dir))
    def down(dir: Path): Vector[String] = Vector("docker", "stack", "rm", d0(dir))

    private def d0(dir: Path): String =
      Option(dir.getParent).flatMap(p => Option(p.getParent)).map(_.getFileName.toString).getOrElse("okay")

    def render(d: Deployment): Either[String, Vector[(String, String)]] =
      refusalOf(d, "swarm") match
        case Some(why) => Left(why)
        case None => d.ordered.map { services =>
          val sb = new StringBuilder
          sb ++= s"# generated by okay-deploy from ${d.name}'s Deployment value — edit the value, not this file\n"
          sb ++= "services:\n"
          for s <- services do
            sb ++= s"  ${s.name}:\n    image: ${imageOf(d, s)}\n"
            if s.ports.nonEmpty then
              sb ++= "    ports:\n"
              for p <- s.ports if p.public do sb ++= s"""      - "${p.number}:${p.number}"\n"""
            val env = s.settings.env ++
              (if s.peers then Vector("OKAY_POOL_SERVICE" -> s"tasks.${s.name}") else Vector.empty)
            if env.nonEmpty then
              sb ++= "    environment:\n"
              for (k, v) <- env do sb ++= s"      $k: ${str(v)}\n"
            sb ++= s"    deploy:\n      mode: replicated\n      replicas: ${math.max(1, s.scale.replicas)}\n"
          Vector("compose.yaml" -> sb.result())
        }

  // ====================================================================
  // AWS Batch — a multi-node parallel job definition, Terraform (the
  // same road `aws`/`gcp`/`azure` already take, `align`ed the same way)
  // ====================================================================

  object Batch extends Target:
    val name = "batch"

    def requires(d: Deployment): Vector[String] = Vector("terraform", "aws")
    def up(dir: Path): Vector[String] = Vector("terraform", s"-chdir=${dir.toString}", "apply")
    def down(dir: Path): Vector[String] = Vector("terraform", s"-chdir=${dir.toString}", "destroy")

    def render(d: Deployment): Either[String, Vector[(String, String)]] =
      val noRegion = d.services.filter(_.region.isEmpty).map(_.name)
      val bad = unsupported(d, "batch")
      if noRegion.nonEmpty then
        Left(s"batch needs a region for ${noRegion.mkString(", ")} and no target can invent one — " +
          "add Need.Region(\"eu-central-1\") (or wherever your users are)")
      else if bad.nonEmpty then
        Left(bad.mkString("; ") + " — point the service at one you run, or use the laptop/cluster target")
      else d.services match
        case Vector(s) => Right(Vector("main.tf" -> Aws.align(main(d, s))))
        case _ => Left("batch renders one multi-node job at a time; this deployment has more than one service")

    /**
     * WHAT BATCH ACTUALLY GIVES A NODE, AND WHAT IT DOES NOT
     * (specs/cluster-pool.md's own honest limit, not glossed over
     * here): every node of a multi-node parallel job is handed
     * `AWS_BATCH_JOB_MAIN_NODE_PRIVATE_IPV4_ADDRESS`, `AWS_BATCH_JOB_
     * NODE_INDEX` and `AWS_BATCH_JOB_NUM_NODES` by the platform
     * itself — the coordinator's address and the node count, never a
     * full peer list. `OKAY_POOL_PEERS` is therefore NOT rendered
     * here; a pool that needs every member to find every OTHER
     * member (not just the main one) needs something Batch itself
     * does not offer — filed as `cluster-pool-batch-full-mesh` if
     * this ever needs closing.
     */
    private def main(d: Deployment, s: Service): String =
      val n = s.name
      val full = s"${d.name}-$n"
      val nodes = math.max(1, s.scale.replicas)
      val cmd = s.run match
        case m: Run.Module =>
          val opts = if m.javaOpts.isBlank then "" else m.javaOpts.trim + " "
          Vector("java") ++ (if opts.isBlank then Vector.empty else opts.trim.split(" ").toVector) ++
            Vector("-cp", "app.jar", m.mainClass)
        case Run.Image(_, _) => Vector("true")   // an image already carries its own entrypoint
      val cmdJson = cmd.map(str).mkString("[", ", ", "]")
      s"""# generated by okay-deploy from ${d.name}'s Deployment value — edit the value, not this file
         |
         |data "aws_iam_role" "batch_service" {
         |  name = "AWSServiceRoleForBatch"
         |}
         |
         |# a multi-node parallel job's `nodeProperties` is a raw JSON
         |# BLOB on the real AWS API, and the provider mirrors that: a
         |# STRING attribute, never a block and never a bare HCL object
         |# either. `terraform validate` caught both wrong shapes in
         |# turn -- first "did you mean to use =?", then "string
         |# required, but have object" -- which is why this is
         |# `jsonencode(...)`, the same road `container_definitions`
         |# already takes above.
         |resource "aws_batch_job_definition" "this" {
         |  name = ${str(full)}
         |  type = "multinode"
         |
         |  node_properties = jsonencode({
         |    main_node = 0
         |    num_nodes = $nodes
         |
         |    node_range_properties = [
         |      {
         |        target_nodes = "0:${nodes - 1}"
         |
         |        container = {
         |          image   = ${str(imageOf(d, s))}
         |          command = $cmdJson
         |
         |          resource_requirements = [
         |            { type = "VCPU", value = "1" },
         |            { type = "MEMORY", value = "2048" },
         |          ]
         |        }
         |      },
         |    ]
         |  })
         |}
         |""".stripMargin
