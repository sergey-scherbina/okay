package okay.deploy

import okay.conf.Secret

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

/**
 * specs/deployment.md, "The CLI, and the deployment as data".
 *
 * `Cli.run` is driven as a function, with the subprocess runner and
 * the tool prober injected — which is what lets this suite assert
 * what `up` WOULD have run on a machine that has no docker, and
 * assert equally that it ran nothing when a tool was missing.
 */
class TestCli extends munit.FunSuite:

  private val web = Service(
    name = "web",
    run = Run.Image("ghcr.io/okay/web", "1.4"),
    settings = Settings.of("okay")("port" -> "8080"),
    secrets = Vector(Secret("env:TOKEN")),
    needs = Vector(Need.Port(8080), Need.Volume("/app/data")))

  private val shop = Deployment("shop", Vector(web))

  private def sandbox(body: Path => Unit): Unit =
    val root = Files.createTempDirectory("okay-cli")
    try body(root)
    finally
      Files.walk(root).sorted(java.util.Comparator.reverseOrder[Path]())
        .forEach(p => Files.deleteIfExists(p): Unit)

  /** an artifacts directory as it reaches a server: the rendered
   * files and deployment.json, and NOTHING else — no repository, no
   * build, no source */
  private def artifacts(root: Path, d: Deployment = shop, t: Target = Targets.Laptop): Path =
    Deployment.write(d, t, root): Unit
    root.resolve(Deployment.dir(d, t.name))

  private class Runner:
    val out = Vector.newBuilder[String]
    val err = Vector.newBuilder[String]
    val ran = Vector.newBuilder[Vector[String]]
    def said: String = out.result().mkString("\n")
    def complained: String = err.result().mkString("\n")
    def commands: Vector[Vector[String]] = ran.result()
    def apply(args: String*)(using cwd: Path)(
      exec: Vector[String] => Shell.Out = _ => Shell.Out(0, ""),
      probing: Tool => Presence = _ => Presence.Ok("1.0"),
    ): Int =
      Cli.run(args.toVector, out += _, err += _, cwd, c => { ran += c; exec(c) }, probing)

  // ------------------------------------------------------------------

  test("the CLI reads the deployment as DATA: an artifacts directory with no repository is enough") {
    sandbox { root =>
      given Path = artifacts(root)
      val files = Files.list(summon[Path]).toArray.map(_.toString).toVector
      assert(files.exists(_.endsWith("deployment.json")), files.toString)
      val r = new Runner
      // no target argument: it comes from the directory the JSON was
      // found in, which is how `okay deploy up` works on a server
      assertEquals(r.apply("deploy", "up")(), Cli.Exit.ok)
      assert(r.commands.exists(_.mkString(" ").contains("docker compose")), r.commands.toString)
      assert(r.said.contains("applying shop to laptop: done."), r.said)
    }
  }

  test("`up` on a machine without the tools applies NOTHING and exits 3, the prerequisite code") {
    sandbox { root =>
      given Path = artifacts(root)
      val r = new Runner
      val code = r.apply("up", "laptop")(probing = _ => Presence.Missing)
      assertEquals(code, Cli.Exit.prerequisite)
      assertEquals(r.commands, Vector.empty)
      assert(r.said.contains("MISSING"), r.said)
      assert(r.complained.contains("Nothing has been applied."), r.complained)
    }
  }

  test("`up` re-renders what is stale first, then applies") {
    sandbox { root =>
      given cwd: Path = artifacts(root)
      Files.writeString(cwd.resolve("compose.yaml"), "hand edited\n", UTF_8): Unit
      val r = new Runner
      assertEquals(r.apply("up", "laptop")(), Cli.Exit.ok)
      assert(r.said.contains("stale"), r.said)
      assert(Files.readString(cwd.resolve("compose.yaml"), UTF_8).contains("services:"))
      assert(r.commands.exists(_.contains("up")), r.commands.toString)
    }
  }

  test("--dry-run prints the exact command and runs nothing") {
    sandbox { root =>
      given Path = artifacts(root)
      val r = new Runner
      assertEquals(r.apply("up", "laptop", "--dry-run")(), Cli.Exit.ok)
      assertEquals(r.commands, Vector.empty)
      assert(r.said.contains("would run: docker compose -f"), r.said)
    }
  }

  test("a failed apply names the command, the exit code and the last output") {
    sandbox { root =>
      given Path = artifacts(root)
      val r = new Runner
      val code = r.apply("up", "laptop")(exec = _ => Shell.Out(1, "network shop_default  Error\nfailed to create network"))
      assertEquals(code, Cli.Exit.failed)
      assert(r.complained.contains("exit 1"), r.complained)
      assert(r.complained.contains("command: docker compose -f"), r.complained)
      assert(r.complained.contains("failed to create network"), r.complained)
    }
  }

  test("a failed apply that printed NOTHING still says something an operator can act on") {
    sandbox { root =>
      given Path = artifacts(root)
      val r = new Runner
      assertEquals(r.apply("down", "laptop")(exec = _ => Shell.Out(1, "")), Cli.Exit.failed)
      assert(r.complained.contains("none — the command printed nothing at all"), r.complained)
    }
  }

  test("`diff` finds a hand edit and exits non-zero, so a pipeline can gate on drift") {
    sandbox { root =>
      given cwd: Path = artifacts(root)
      val clean = new Runner
      assertEquals(clean.apply("diff", "laptop")(), Cli.Exit.ok)
      assert(clean.said.contains("no drift"), clean.said)

      Files.writeString(cwd.resolve("compose.yaml"), "hand edited\n", UTF_8): Unit
      val dirty = new Runner
      assertEquals(dirty.apply("diff", "laptop")(), Cli.Exit.failed)
      assert(dirty.said.contains("compose.yaml"), dirty.said)
      assert(dirty.said.contains("okay deploy render laptop"), dirty.said)
    }
  }

  test("`render` writes only what changed, and says so") {
    sandbox { root =>
      given cwd: Path = artifacts(root)
      val same = new Runner
      assertEquals(same.apply("render", "laptop")(), Cli.Exit.ok)
      assert(same.said.contains("were already what the value renders"), same.said)

      Files.delete(cwd.resolve("compose.yaml"))
      val again = new Runner
      assertEquals(again.apply("render", "laptop")(), Cli.Exit.ok)
      assert(again.said.contains("wrote 1 of"), again.said)
      assert(Files.exists(cwd.resolve("compose.yaml")))
    }
  }

  test("`targets` says what this deployment can go to, and names what a target refuses") {
    sandbox { root =>
      val d = shop.copy(services = Vector(web.copy(needs = web.needs :+ Need.Database(Engine.Postgres, "16", "shop"))))
      given Path = artifacts(root, d)
      val r = new Runner
      assertEquals(r.apply("targets")(), Cli.Exit.ok)
      assert(r.said.contains("laptop"), r.said)
      assert(r.said.contains("host") && r.said.contains("cannot:"), r.said)
      assert(r.said.contains("does not install one"), r.said)
    }
  }

  test("`doctor --json` answers with the report and the prerequisite code") {
    sandbox { root =>
      given Path = artifacts(root)
      val r = new Runner
      val code = r.apply("doctor", "laptop", "--json")(probing = t => if t.name == "docker" then Presence.Missing else Presence.Ok("1"))
      assertEquals(code, Cli.Exit.prerequisite)
      assert(r.said.startsWith("{"), r.said)
      assert(r.said.contains("\"state\":\"missing\""), r.said)
    }
  }

  test("`doctor --install` installs, re-checks, and ends on the table it started from") {
    sandbox { root =>
      given Path = artifacts(root)
      val r = new Runner
      var installed = false
      val code = r.apply("doctor", "laptop", "--install")(
        exec = cmd => { if cmd.mkString(" ").contains("install") then installed = true; Shell.Out(0, "") },
        probing = _ => if installed then Presence.Ok("27.0.0") else Presence.Missing)
      assertEquals(code, Cli.Exit.ok)
      assert(installed)
      assert(r.said.contains("all "), r.said)
      assert(r.said.lastIndexOf("ok 27.0.0") > r.said.indexOf("MISSING"), r.said)
    }
  }

  test("bad arguments are exit 2, and every one of them says what to do instead") {
    sandbox { root =>
      given Path = artifacts(root)
      assertEquals(new Runner().apply()(), Cli.Exit.usage)

      val unknownVerb = new Runner
      assertEquals(unknownVerb.apply("frobnicate")(), Cli.Exit.usage)
      assert(unknownVerb.complained.contains("unknown command: frobnicate"), unknownVerb.complained)

      val unknownFlag = new Runner
      assertEquals(unknownFlag.apply("up", "laptop", "--force")(), Cli.Exit.usage)
      assert(unknownFlag.complained.contains("unknown option: --force"), unknownFlag.complained)

      // a JSON whose directory is not a target's name: nothing to infer
      val loose = Files.createTempDirectory("okay-cli-loose")
      Files.writeString(loose.resolve("deployment.json"), Deployment.json(shop), UTF_8): Unit
      val noTarget = new Runner
      assertEquals(noTarget.apply("render")(using loose)(), Cli.Exit.usage)
      assert(noTarget.complained.contains("needs a target"), noTarget.complained)

      val badTarget = new Runner
      assertEquals(badTarget.apply("up", "mars")(), Cli.Exit.usage)
      assert(badTarget.complained.contains("no target named `mars`"), badTarget.complained)
    }
  }

  test("`--yes` is accepted and changes nothing: this CLI never prompts") {
    sandbox { root =>
      given Path = artifacts(root)
      val r = new Runner
      assertEquals(r.apply("up", "laptop", "--yes", "--dry-run")(), Cli.Exit.ok)
      assert(r.said.contains("would run:"), r.said)
    }
  }

  test("no deployment.json is not a stack trace: it says where it looked and how to make one") {
    sandbox { root =>
      given Path = root
      val r = new Runner
      assertEquals(r.apply("up", "laptop")(), Cli.Exit.usage)
      assert(r.complained.contains("no deployment.json found"), r.complained)
      assert(r.complained.contains(root.resolve("deployment.json").toString), r.complained)
      assert(r.complained.contains("runMain"), r.complained)
    }
  }

  test("a deployment.json this build cannot read is named, not swallowed") {
    sandbox { root =>
      Files.writeString(root.resolve("deployment.json"), "{\"name\":\"shop\"}", UTF_8): Unit
      given Path = root
      val r = new Runner
      assertEquals(r.apply("diff", "laptop")(), Cli.Exit.failed)
      assert(r.complained.contains("is not a deployment this build understands"), r.complained)
    }
  }

  test("a repository checkout is found too: <module>/deploy/<target>/ one level down") {
    sandbox { root =>
      val d = shop.copy(services = Vector(web.copy(run = Run.Module("okayScript", "okay-script", "okay.script.Serve"))))
      artifacts(root, d): Unit
      given Path = root
      val r = new Runner
      assertEquals(r.apply("diff", "laptop")(), Cli.Exit.ok)
      assert(r.said.contains("no drift"), r.said)
      assert(Cli.candidates(root, Some("laptop")).exists(_.toString.contains("okay-script/deploy/laptop")),
        Cli.candidates(root, Some("laptop")).toString)
    }
  }

  test("--file points at one directly, and wins over the search") {
    sandbox { root =>
      val dir = artifacts(root)
      given Path = root
      val r = new Runner
      assertEquals(r.apply("diff", "--file", dir.resolve("deployment.json").toString)(), Cli.Exit.ok)
      assert(r.said.contains("no drift"), r.said)
    }
  }

  test("the help fits a screen and leads with the three commands an operator uses") {
    assert(Cli.help.linesIterator.size <= 24, Cli.help)
    val body = Cli.help.take(Cli.help.indexOf("options"))
    assert(body.contains("okay deploy up"), body)
    assert(body.contains("okay deploy doctor"), body)
    assert(body.contains("okay deploy diff"), body)
    assert(Cli.help.contains("exit codes: 0 applied · 1 failed · 2 bad arguments · 3 a prerequisite is missing"), Cli.help)
  }
