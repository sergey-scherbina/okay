package okay.conf

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

/**
 * The resolvers with something actually running behind them
 * (specs/conf.md, "What is tested, and what is not").
 *
 * `sops` is end to end and real: a container with sops and age, a
 * generated key, an encrypted file, decrypted THROUGH the resolver.
 * The three cloud managers have no account on any machine this
 * repository builds on, so what is checked there is the command each
 * builds — with `/bin/echo` standing in for the CLI — and the named
 * refusal when the binary is absent.
 *
 * Live, because every one of these starts a process.
 */
class TestSchemesLive extends munit.FunSuite:

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  override val munitTimeout = scala.concurrent.duration.Duration(10, "min")

  private def sh(cmd: Vector[String], env: Map[String, String] = Map.empty): (Int, String) =
    val pb = ProcessBuilder(cmd*).redirectErrorStream(true)
    env.foreach((k, v) => pb.environment.put(k, v): Unit)
    val p = pb.start()
    val out = String(p.getInputStream.readAllBytes(), UTF_8)
    (p.waitFor(), out)

  private def sandbox(body: Path => Unit): Unit =
    val root = Files.createTempDirectory("okay-schemes")
    try body(root)
    finally
      Files.walk(root).sorted(java.util.Comparator.reverseOrder[Path]())
        .forEach(p => Files.deleteIfExists(p): Unit)

  // ---- the commands, with echo standing in for the CLI ---------------

  test("aws-sm asks Secrets Manager for the string, by the id in the reference") {
    val said = Schemes.awsSecrets(Vector("/bin/echo")).get(Secret("aws-sm:prod/pg-password"))
    assertEquals(said, Right(
      "secretsmanager get-secret-value --secret-id prod/pg-password --query SecretString --output text"))
  }

  test("gcp-sm accesses a version, `latest` unless the reference says otherwise") {
    assertEquals(Schemes.gcpSecrets(Vector("/bin/echo")).get(Secret("gcp-sm:pg-password")),
      Right("secrets versions access latest --secret=pg-password"))
    assertEquals(Schemes.gcpSecrets(Vector("/bin/echo")).get(Secret("gcp-sm:pg-password#7")),
      Right("secrets versions access 7 --secret=pg-password"))
  }

  test("azure-kv splits the vault from the name and asks for the value alone") {
    assertEquals(Schemes.azureVault(Vector("/bin/echo")).get(Secret("azure-kv:prod-vault/pg-password")),
      Right("keyvault secret show --vault-name prod-vault --name pg-password --query value -o tsv"))
  }

  test("sops extracts a dotted key as the path expression sops wants") {
    assertEquals(Schemes.sops(Vector("/bin/echo")).get(Secret("sops:secrets.yaml#db.password")),
      Right("""-d --extract ["db"]["password"] secrets.yaml"""))
    // no key is the whole file, which is what a single-value file wants
    assertEquals(Schemes.sops(Vector("/bin/echo")).get(Secret("sops:secrets.yaml")),
      Right("-d secrets.yaml"))
  }

  // ---- the refusals --------------------------------------------------

  test("a binary that is not there is a NAMED refusal, not a crash") {
    val m = Schemes.sops(Vector("nosuchbinary-okay-conf")).get(Secret("sops:x.yaml#k")).left.getOrElse("")
    assert(m.contains("sops:x.yaml#k"), m)
    assert(m.contains("not on the PATH"), m)
    assert(m.contains("doctor"), m)
  }

  test("A FAILURE NEVER CARRIES THE OUTPUT, because the output may be the secret") {
    sandbox { root =>
      // a tool that prints a secret and THEN fails — which is exactly
      // how a half-configured CLI behaves
      val leaky = root.resolve("leaky")
      Files.writeString(leaky, "#!/bin/sh\necho 'hunter2-the-actual-secret'\necho 'also on stderr' >&2\nexit 4\n", UTF_8): Unit
      leaky.toFile.setExecutable(true): Unit

      val m = Schemes.awsSecrets(Vector(leaky.toString)).get(Secret("aws-sm:prod/pg")).left.getOrElse("")
      assert(!m.contains("hunter2"), s"the value leaked into the refusal: $m")
      assert(!m.contains("also on stderr"), s"stderr leaked into the refusal: $m")
      assert(m.contains("exited 4"), m)
      assert(m.contains("aws-sm:prod/pg"), m)
      // and it says how to see it, on the operator's own terminal
      assert(m.contains("run `"), m)
    }
  }

  test("a tool that succeeds with nothing is a refusal, not an empty secret") {
    sandbox { root =>
      val quiet = root.resolve("quiet")
      Files.writeString(quiet, "#!/bin/sh\nexit 0\n", UTF_8): Unit
      quiet.toFile.setExecutable(true): Unit
      val m = Schemes.gcpSecrets(Vector(quiet.toString)).get(Secret("gcp-sm:pg")).left.getOrElse("")
      assert(m.contains("resolved to nothing"), m)
    }
  }

  test("exactly one trailing newline is trimmed — every one of these CLIs adds one") {
    sandbox { root =>
      val two = root.resolve("two")
      Files.writeString(two, "#!/bin/sh\nprintf 'value\\n\\n'\n", UTF_8): Unit
      two.toFile.setExecutable(true): Unit
      assertEquals(Schemes.awsSecrets(Vector(two.toString)).get(Secret("aws-sm:x")), Right("value\n"))
    }
  }

  // ---- sops, for real ------------------------------------------------

  private val image = "okay-conf-sops-test"

  private def sopsImage(): Boolean =
    if !sh(Vector("docker", "image", "inspect", image))._1.eq(0) then
      val dir = Files.createTempDirectory("okay-sops-image")
      try
        Files.writeString(dir.resolve("Dockerfile"),
          "FROM alpine:3.20\nRUN apk add --no-cache sops age\n", UTF_8): Unit
        sh(Vector("docker", "build", "-t", image, dir.toString))._1 == 0
      finally
        Files.walk(dir).sorted(java.util.Comparator.reverseOrder[Path]())
          .forEach(p => Files.deleteIfExists(p): Unit)
    else true

  test("sops decrypts a real file with a real age key, through the resolver") {
    assume(sh(Vector("docker", "version"))._1 == 0, "no docker on this machine")
    assume(sopsImage(), "could not build the sops test image")

    sandbox { root =>
      // the container mounts the temp directory at its OWN path, so a
      // path the resolver passes means the same thing on both sides
      val w = root.toAbsolutePath.toString
      def inImage(args: Vector[String], env: Vector[String] = Vector.empty) =
        sh(Vector("docker", "run", "--rm", "-v", s"$w:$w", "-w", w) ++
          env.flatMap(e => Vector("-e", e)) ++ Vector(image) ++ args)

      val (keyCode, keyOut) = inImage(Vector("age-keygen", "-o", s"$w/key.txt"))
      assert(keyCode == 0, keyOut)
      val recipient = Files.readString(root.resolve("key.txt"), UTF_8)
        .linesIterator.find(_.startsWith("# public key:")).map(_.split(':').last.trim)
        .getOrElse(fail(s"no recipient in the generated key"))

      Files.writeString(root.resolve("secrets.yaml"),
        "pg: hunter2-the-actual-secret\napi:\n  token: second-one\n", UTF_8): Unit
      val (encCode, encOut) = inImage(Vector("sops", "--age", recipient, "-e", "-i", s"$w/secrets.yaml"))
      assert(encCode == 0, encOut)
      // it really is encrypted: the value is not in the file
      val onDisk = Files.readString(root.resolve("secrets.yaml"), UTF_8)
      assert(!onDisk.contains("hunter2"), onDisk)
      assert(onDisk.contains("sops:") || onDisk.contains("ENC["), onDisk)

      // the shim: `sops` as far as the resolver is concerned
      val shim = root.resolve("sops")
      Files.writeString(shim,
        s"""#!/bin/sh
           |exec docker run --rm -e SOPS_AGE_KEY_FILE=$w/key.txt -v $w:$w -w $w $image sops "$$@"
           |""".stripMargin, UTF_8): Unit
      shim.toFile.setExecutable(true): Unit

      val resolver = Schemes.sops(Vector(shim.toString))
      assertEquals(resolver.get(Secret(s"sops:$w/secrets.yaml#pg")), Right("hunter2-the-actual-secret"))
      // a nested key, through the path expression
      assertEquals(resolver.get(Secret(s"sops:$w/secrets.yaml#api.token")), Right("second-one"))

      // and a key that is not there refuses WITHOUT saying what is
      val missing = resolver.get(Secret(s"sops:$w/secrets.yaml#nope")).left.getOrElse("")
      assert(!missing.contains("hunter2"), missing)
      assert(missing.contains("sops exited"), missing)
    }
  }
