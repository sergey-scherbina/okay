package okay.conf

/**
 * The four resolvers that shell out to a vendor's own CLI
 * (specs/conf.md, "The four that shell out").
 *
 * `sops:`, `aws-sm:`, `gcp-sm:` and `azure-kv:` were named in
 * specs/deployment.md and checked for by okay-deploy's doctor — which
 * tells an operator which binary to install for each — and
 * implemented by nothing. This is that promise kept.
 *
 * A CLI rather than an SDK, deliberately: an SDK would be four
 * dependencies, four credential chains and four things to keep
 * current, while the CLI is one the operator already has, whose
 * profile, SSO session, `gcloud auth` and `az login` already work.
 *
 * And the rule that matters most here, which INVERTS okay-deploy's:
 * a failure never carries the command's output, because a secret
 * resolver's stdout may BE the secret.
 */
object Schemes:

  /** `sops:<file>#<key>` — the encrypted file rides in git and the
   * key is a path inside it. `sops:secrets.yaml` with no `#` decrypts
   * the whole file, which is what a single-value file wants. */
  def sops(command: Vector[String] = Vector("sops")): Secrets = s =>
    Secrets.scheme(s.ref) match
      case ("sops", rest) =>
        val (file, key) = rest.indexOf('#') match
          case -1 => (rest, None)
          case i => (rest.take(i), Some(rest.drop(i + 1)))
        if file.isEmpty then Left(s"'${s.ref}' names no file — sops:<file>#<key>")
        else
          val extract = key.filter(_.nonEmpty).toVector.flatMap(k => Vector("--extract", path(k)))
          run(command ++ Vector("-d") ++ extract ++ Vector(file), s, "sops")
      case _ => Secrets.unrecognized(s)

  /** `aws-sm:<id>` — an id or a full ARN, whichever the operator has */
  def awsSecrets(command: Vector[String] = Vector("aws")): Secrets = s =>
    Secrets.scheme(s.ref) match
      case ("aws-sm", id) if id.nonEmpty =>
        run(command ++ Vector("secretsmanager", "get-secret-value",
          "--secret-id", id, "--query", "SecretString", "--output", "text"), s, "aws")
      case ("aws-sm", _) => Left(s"'${s.ref}' names no secret — aws-sm:<id or arn>")
      case _ => Secrets.unrecognized(s)

  /** `gcp-sm:<name>` or `gcp-sm:<name>#<version>`; `latest` by default */
  def gcpSecrets(command: Vector[String] = Vector("gcloud")): Secrets = s =>
    Secrets.scheme(s.ref) match
      case ("gcp-sm", rest) if rest.nonEmpty =>
        val (name, version) = rest.indexOf('#') match
          case -1 => (rest, "latest")
          case i => (rest.take(i), rest.drop(i + 1))
        run(command ++ Vector("secrets", "versions", "access", version, s"--secret=$name"), s, "gcloud")
      case ("gcp-sm", _) => Left(s"'${s.ref}' names no secret — gcp-sm:<name>[#<version>]")
      case _ => Secrets.unrecognized(s)

  /** `azure-kv:<vault>/<name>` — the vault is part of the reference
   * because a deployment reading two vaults is a normal deployment */
  def azureVault(command: Vector[String] = Vector("az")): Secrets = s =>
    Secrets.scheme(s.ref) match
      case ("azure-kv", rest) =>
        rest.indexOf('/') match
          case i if i > 0 && i < rest.length - 1 =>
            run(command ++ Vector("keyvault", "secret", "show",
              "--vault-name", rest.take(i), "--name", rest.drop(i + 1),
              "--query", "value", "-o", "tsv"), s, "az")
          case _ => Left(s"'${s.ref}' is not <vault>/<name> — azure-kv:my-vault/pg-password")
      case _ => Secrets.unrecognized(s)

  /** every scheme this platform can resolve, in one resolver: the
   * chain okay-deploy's `doctor` describes and a deployment reads */
  def all(): Secrets = Secrets.chain(
    Secrets.env, Secrets.file, sops(), awsSecrets(), gcpSecrets(), azureVault())

  // ------------------------------------------------------------------

  /** sops' `--extract` takes a path expression: `a.b` is `["a"]["b"]` */
  private def path(key: String): String =
    key.split('.').filter(_.nonEmpty).map(k => "[\"" + k.replace("\"", "\\\"") + "\"]").mkString

  /**
   * Run it, and answer only the value or a refusal that says nothing
   * about what came back.
   *
   * The output is NEVER in the message. okay-deploy's `Shell.failure`
   * carries the last lines because those commands render files; here
   * stdout may be the secret itself, and a tool that wrote a key into
   * its own stderr would have it copied into a log by a "helpful"
   * error message. The refusal says which command to run by hand
   * instead, which puts the output on the operator's terminal.
   */
  private def run(cmd: Vector[String], s: Secret, tool: String): Either[String, String] =
    try
      val p = ProcessBuilder(cmd*).redirectErrorStream(false).start()
      p.getOutputStream.close()
      val out = String(p.getInputStream.readAllBytes(), "UTF-8")
      p.getErrorStream.readAllBytes(): Unit  // drained and DISCARDED, never reported
      val code = p.waitFor()
      if code == 0 then
        val value = trimNewline(out)
        if value.isEmpty then
          Left(s"'${s.ref}' resolved to nothing — $tool succeeded and returned an empty value")
        else Right(value)
      else
        Left(s"'${s.ref}' could not be resolved: $tool exited $code. " +
          s"Its output is deliberately not repeated here (it may hold the secret) — " +
          s"run `${line(cmd)}` yourself to see it.")
    catch
      case _: java.io.IOException =>
        Left(s"'${s.ref}' needs $tool and it is not on the PATH — " +
          s"okay-deploy's `okay deploy doctor` names the command that installs it")
      case _: InterruptedException =>
        Thread.currentThread.interrupt()
        Left(s"'${s.ref}' was interrupted while $tool ran")
      case e: Exception =>
        Left(s"'${s.ref}' could not be resolved: ${e.getClass.getSimpleName}")

  /** one trailing newline — every one of these CLIs adds one */
  private def trimNewline(v: String): String =
    if v.endsWith("\r\n") then v.dropRight(2)
    else if v.endsWith("\n") then v.dropRight(1)
    else v

  /** a command line an operator can paste back; no value is ever in
   * one of these, since a reference is all we pass */
  private[conf] def line(cmd: Vector[String]): String =
    cmd.map(a => if a.exists(c => c.isWhitespace || c == '"') then "\"" + a.replace("\"", "\\\"") + "\"" else a)
      .mkString(" ")
