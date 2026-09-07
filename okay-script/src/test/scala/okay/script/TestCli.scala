package okay.script

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

/**
 * specs/okay-script.md, "The command line": run, render, build,
 * check, new — driven as a function, so the exit code and both
 * streams are the assertions.
 *
 * `serve` is deliberately not here: it is `Serve.main`, which blocks
 * until interrupted, and a second implementation to make it testable
 * would be the second set of switches this CLI exists to avoid.
 */
class TestCli extends munit.FunSuite:

  private class Run:
    val out = Vector.newBuilder[String]
    val err = Vector.newBuilder[String]
    def said: String = out.result().mkString("\n")
    def complained: String = err.result().mkString("\n")
    def apply(args: String*)(using cwd: Path): Int =
      Cli.run(args.toVector, out += _, err += _, cwd)

  private def sandbox(body: Path => Unit): Unit =
    val root = Files.createTempDirectory("okay-cli")
    try body(root)
    finally
      Files.walk(root).sorted(java.util.Comparator.reverseOrder[Path]())
        .forEach(p => Files.deleteIfExists(p): Unit)

  private def write(dir: Path, name: String, content: String): Path =
    val f = dir.resolve(name)
    Option(f.getParent).foreach(Files.createDirectories(_): Unit)
    Files.writeString(f, content, UTF_8): Unit
    f

  // ---- run ----------------------------------------------------------

  test("`run` executes a document and answers its stdout") {
    sandbox { root =>
      given Path = root
      write(root, "hello.md", "# a doc\n\n```scala\nprintln(\"two: \" + (1 + 1))\n```\n")
      val r = new Run
      assertEquals(r("run", "hello.md"), Cli.Exit.ok, r.complained)
      assert(r.said.contains("two: 2"), r.said)
      // `run` runs the BLOCKS; the prose is not output
      assert(!r.said.contains("# a doc"), r.said)
    }
  }

  test("a document that throws is exit 1, and the message names the file") {
    sandbox { root =>
      given Path = root
      write(root, "boom.md", "```scala\nsys.error(\"deliberate\")\n```\n")
      val r = new Run
      assertEquals(r("run", "boom.md"), Cli.Exit.failed)
      assert(r.complained.contains("boom.md"), r.complained)
      assert(r.complained.contains("deliberate"), r.complained)
    }
  }

  test("a document that does not compile is exit 1 with the line") {
    sandbox { root =>
      given Path = root
      write(root, "bad.md", "```scala\nval x: Int = \"not an int\"\n```\n")
      val r = new Run
      assertEquals(r("run", "bad.md"), Cli.Exit.failed)
      assert(r.complained.contains("bad.md"), r.complained)
      assert(r.complained.contains("L2"), r.complained)
    }
  }

  // ---- render -------------------------------------------------------

  test("`render` writes the prose with its expressions evaluated") {
    sandbox { root =>
      given Path = root
      write(root, "page.md", "Two plus two is ${2 + 2}.\n")
      val toStdout = new Run
      assertEquals(toStdout("render", "page.md"), Cli.Exit.ok, toStdout.complained)
      assert(toStdout.said.contains("Two plus two is 4."), toStdout.said)

      val toFile = new Run
      assertEquals(toFile("render", "page.md", "-o", "out.html"), Cli.Exit.ok, toFile.complained)
      assert(Files.readString(root.resolve("out.html"), UTF_8).contains("Two plus two is 4."))
    }
  }

  // ---- build --------------------------------------------------------

  test("`build` renders every page once, copies the static files, and needs no JVM afterwards") {
    sandbox { root =>
      given Path = root
      val pages = root.resolve("pages")
      write(pages, "index.md", "<h1>home</h1>\n<p>${1 + 1}</p>\n")
      write(pages, "about.md", "<h1>about</h1>\n")
      write(pages, "deep/thing.md", "<h1>deep</h1>\n")
      write(pages, "style.css", "body { color: red }\n")

      val r = new Run
      assertEquals(r("build", "pages", "-o", "site"), Cli.Exit.ok, r.complained)
      val site = root.resolve("site")
      assertEquals(Files.readString(site.resolve("index.html"), UTF_8).trim, "<h1>home</h1>\n<p>2</p>")
      assert(Files.isRegularFile(site.resolve("about.html")))
      assert(Files.isRegularFile(site.resolve("deep/thing.html")))
      // byte-identical, not re-rendered
      assertEquals(Files.readString(site.resolve("style.css"), UTF_8), "body { color: red }\n")
      assert(r.said.contains("no JVM needed"), r.said)
    }
  }

  test("`build` needs somewhere to put the site, and says so") {
    sandbox { root =>
      given Path = root
      Files.createDirectories(root.resolve("pages")): Unit
      val r = new Run
      assertEquals(r("build", "pages"), Cli.Exit.usage)
      assert(r.complained.contains("-o"), r.complained)
    }
  }

  test("`build` REFUSES a page that touches its session, naming the page and the call") {
    sandbox { root =>
      given Path = root
      val pages = root.resolve("pages")
      write(pages, "index.md", "<h1>fine</h1>\n")
      write(pages, "cart.md",
        "```scala\nimport okay.script.api.*\nprintln(Session.current.get(\"items\").getOrElse(\"none\"))\n```\n")

      val r = new Run
      assertEquals(r("build", "pages", "-o", "site"), Cli.Exit.failed)
      assert(r.complained.contains("cart.md"), r.complained)
      assert(r.complained.contains("Session.get"), r.complained)
      assert(r.complained.contains("a static site has no request"), r.complained)
      // the page that CAN be built still was: one bad page does not
      // cost an operator the rest of the site
      assert(Files.isRegularFile(root.resolve("site/index.html")))
    }
  }

  test("`build` REFUSES a page that redirects, because a file cannot be a 302") {
    sandbox { root =>
      given Path = root
      val pages = root.resolve("pages")
      write(pages, "go.md", "```scala\nokay.script.api.Response.current.redirect(\"/elsewhere\")\n```\n")
      val r = new Run
      assertEquals(r("build", "pages", "-o", "site"), Cli.Exit.failed)
      assert(r.complained.contains("go.md"), r.complained)
      assert(r.complained.contains("302"), r.complained)
      assert(r.complained.contains("/elsewhere"), r.complained)
    }
  }

  test("`build` skips a [param] page BY NAME rather than rendering an empty one") {
    sandbox { root =>
      given Path = root
      val pages = root.resolve("pages")
      write(pages, "index.md", "<h1>home</h1>\n")
      write(pages, "product/[sku].md", "<h1>${okay.script.api.Web.current.params(\"sku\")}</h1>\n")
      val r = new Run
      assertEquals(r("build", "pages", "-o", "site"), Cli.Exit.ok, r.complained)
      assert(r.said.contains("[sku].md"), r.said)
      assert(r.said.contains("no parameter to bind"), r.said)
      assert(!Files.exists(root.resolve("site/product")), "an empty [param] page was written")
    }
  }

  test("`build --lang` writes the whole site once per language, the default at the root") {
    sandbox { root =>
      given Path = root
      val pages = root.resolve("pages")
      write(pages, "index.md", "<h1>hello</h1>\n")
      write(pages, "index.uk.md", "<h1>вітаю</h1>\n")
      val r = new Run
      assertEquals(r("build", "pages", "-o", "site", "--lang", "en,uk"), Cli.Exit.ok, r.complained)
      assert(Files.readString(root.resolve("site/index.html"), UTF_8).contains("hello"))
      assert(Files.readString(root.resolve("site/uk/index.html"), UTF_8).contains("вітаю"),
        Files.readString(root.resolve("site/uk/index.html"), UTF_8))
    }
  }

  test("`build --json` says the same thing a pipeline can read") {
    sandbox { root =>
      given Path = root
      val pages = root.resolve("pages")
      write(pages, "index.md", "<h1>home</h1>\n")
      val r = new Run
      assertEquals(r("build", "pages", "-o", "site", "--json"), Cli.Exit.ok, r.complained)
      okay.codec.Json.parse(r.said) match
        case okay.codec.Json.JObj(fs) =>
          assertEquals(fs.collectFirst { case ("ok", okay.codec.Json.JBool(b)) => b }, Some(true))
          assert(fs.exists(_._1 == "written"), r.said)
        case other => fail(s"not an object: $other")
    }
  }

  // ---- check --------------------------------------------------------

  test("`check` is the mdoc gate: a matching fence passes, a mismatch is exit 1 with both sides") {
    sandbox { root =>
      given Path = root
      write(root, "good.md", "```scala\nprintln(\"expected\")\n```\n\n```stdout\nexpected\n```\n")
      write(root, "bad.md", "```scala\nprintln(\"actual\")\n```\n\n```stdout\nsomething else\n```\n")

      val good = new Run
      assertEquals(good("check", "good.md"), Cli.Exit.ok, good.complained)
      assert(good.said.contains("all matching"), good.said)

      val bad = new Run
      assertEquals(bad("check", "bad.md"), Cli.Exit.failed)
      assert(bad.complained.contains("bad.md"), bad.complained)
      assert(bad.complained.contains("something else"), bad.complained)
    }
  }

  test("`check` takes a whole directory in one run") {
    sandbox { root =>
      given Path = root
      val docs = root.resolve("docs")
      write(docs, "a.md", "```scala\nprintln(\"a\")\n```\n\n```stdout\na\n```\n")
      write(docs, "b.md", "```scala\nprintln(\"b\")\n```\n\n```stdout\nb\n```\n")
      val r = new Run
      assertEquals(r("check", "docs"), Cli.Exit.ok, r.complained)
      assert(r.said.contains("2 checked"), r.said)
    }
  }

  // ---- new ----------------------------------------------------------

  test("`new` writes a starter that BUILDS with nothing to edit — the only test of a starter that matters") {
    sandbox { root =>
      given Path = root
      val made = new Run
      assertEquals(made("new", "site"), Cli.Exit.ok, made.complained)
      assert(Files.isRegularFile(root.resolve("site/index.md")))

      val built = new Run
      assertEquals(built("build", "site", "-o", "out"), Cli.Exit.ok, built.complained)
      val html = Files.readString(root.resolve("out/index.html"), UTF_8)
      assert(html.contains("<h1>A page that is a program</h1>"), html)
      assert(html.contains("Two plus two is 4"), html)
      assert(html.contains("when this page ran"), html)
      assert(Files.isRegularFile(root.resolve("out/style.css")))
    }
  }

  test("`new` will not overwrite a directory that has something in it") {
    sandbox { root =>
      given Path = root
      write(root, "site/mine.md", "do not touch\n")
      val r = new Run
      assertEquals(r("new", "site"), Cli.Exit.usage)
      assert(r.complained.contains("not empty"), r.complained)
      assertEquals(Files.readString(root.resolve("site/mine.md"), UTF_8), "do not touch\n")
    }
  }

  // ---- the shape of the tool ----------------------------------------

  test("bad arguments are exit 2, and each says what to do instead") {
    sandbox { root =>
      given Path = root
      assertEquals(new Run()(), Cli.Exit.usage)

      val unknown = new Run
      assertEquals(unknown("frobnicate"), Cli.Exit.usage)
      assert(unknown.complained.contains("unknown command: frobnicate"), unknown.complained)

      val missing = new Run
      assertEquals(missing("run", "nope.md"), Cli.Exit.usage)
      assert(missing.complained.contains("not a file"), missing.complained)
    }
  }

  test("`okay script <verb>` and `okay <verb>` are the same program") {
    sandbox { root =>
      given Path = root
      write(root, "page.md", "${6 * 7}\n")
      val withGroup = new Run
      val without = new Run
      assertEquals(withGroup("script", "render", "page.md"), Cli.Exit.ok)
      assertEquals(without("render", "page.md"), Cli.Exit.ok)
      assertEquals(withGroup.said, without.said)
      assert(withGroup.said.contains("42"), withGroup.said)
    }
  }

  test("the combined binary answers `deploy` too — okay-script's jar carries okay-deploy") {
    sandbox { root =>
      given Path = root
      val r = new Run
      // no deployment.json anywhere: the DEPLOY cli's own refusal,
      // which is how we know the dispatch reached it
      assertEquals(r("deploy", "up", "laptop"), okay.deploy.Cli.Exit.usage)
      assert(r.complained.contains("no deployment.json found"), r.complained)
    }
  }

  test("the help fits a screen and leads with what a person does most") {
    assert(Cli.help.linesIterator.size <= 24, Cli.help)
    assert(Cli.help.contains("okay script run"), Cli.help)
    assert(Cli.help.contains("okay script build"), Cli.help)
    assert(Cli.help.contains("exit codes: 0 fine · 1 the operation failed · 2 bad arguments"), Cli.help)
  }
