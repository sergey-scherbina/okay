package okay.script

import java.nio.file.Files

/**
 * specs/okay-script.md, "The configuration" (script-config): the
 * names the program reads and the names the deployment renders are
 * ONE list, derived from one case class.
 *
 * The test that matters here is the last one — it is the only thing
 * standing between this and the two lists that used to agree because
 * a person kept them agreeing.
 */
class TestScriptConfig extends munit.FunSuite:

  private def pages(body: java.nio.file.Path => Unit): Unit =
    val root = Files.createTempDirectory("okay-script-config-")
    try body(root) finally Files.deleteIfExists(root): Unit

  test("every setting the deployment renders is a field of the config the program reads") {
    val rendered = ScriptDeploy.system.services.flatMap(_.settings.env.map(_._1))
    assert(rendered.nonEmpty)
    for name <- rendered do
      assert(Serve.Config.names.contains(name),
        s"the deployment sets $name and Serve.Config has no field for it; it would be read by nothing")
  }

  test("the names are DERIVED, so this is the whole list and nothing else answers") {
    assert(Serve.Config.names.contains("OKAY_TLS_RELOAD"), Serve.Config.names.toString)
    assert(Serve.Config.names.contains("OKAY_ACME_DOMAINS"), Serve.Config.names.toString)
    assert(Serve.Config.names.contains("OKAY_HTTPS_ONLY"), Serve.Config.names.toString)
    assertEquals(Serve.Config.names.length, 17)
    assertEquals(Serve.Config.names.distinct.length, 17)
  }

  test("defaults, then the file OKAY_CONF names, then the environment, then the command line") {
    pages { root =>
      val conf = root.resolve("okay.json")
      Files.writeString(conf, s"""{"pages":"${root.toString}","port":9000,"ops":true}"""): Unit
      try
        def parse(env: Map[String, String], args: String*) = Serve.parse(args.toArray, env.get)

        // the file over the defaults
        assertEquals(parse(Map("OKAY_CONF" -> conf.toString)).map(a => (a.port, a.ops)), Right((9000, true)))
        // the environment over the file
        assertEquals(parse(Map("OKAY_CONF" -> conf.toString, "OKAY_PORT" -> "9500")).map(_.port), Right(9500))
        // the command line over the environment
        assertEquals(parse(Map("OKAY_CONF" -> conf.toString, "OKAY_PORT" -> "9500"), root.toString, "9900").map(_.port),
          Right(9900))
        // and what nobody set keeps the program's own default
        assertEquals(parse(Map("OKAY_CONF" -> conf.toString)).map(_.languages), Right(Vector("en")))
      finally Files.deleteIfExists(conf): Unit
    }
  }

  test("OKAY_CONF naming a file that is not there is a REFUSAL, not a silent run on defaults") {
    pages { root =>
      val missing = root.resolve("nope.json").toString
      val got = Serve.parse(Array(root.toString), Map("OKAY_CONF" -> missing).get)
      assert(got.left.exists(_.contains("OKAY_CONF names")), got.toString)
      assert(got.left.exists(_.contains(missing)), got.toString)
    }
  }

  test("a wrong value names its variable — the old code silently dropped it") {
    pages { root =>
      val got = Serve.parse(Array(root.toString), Map("OKAY_TLS_RELOAD" -> "hourly").get)
      assert(got.left.exists(_.contains("OKAY_TLS_RELOAD is not a whole number: 'hourly'")), got.toString)
    }
  }

  test("the pairs are still pairs: a certificate without its key is named, never a fall back to plaintext") {
    pages { root =>
      val half = Serve.parse(Array(root.toString), Map("OKAY_TLS_CERT" -> "/x/cert.pem").get)
      assert(half.left.exists(_.contains("OKAY_TLS_CERT is set without OKAY_TLS_KEY")), half.toString)

      val other = Serve.parse(Array(root.toString), Map("OKAY_TLS_KEY" -> "file:/x/key.pem").get)
      assert(other.left.exists(_.contains("OKAY_TLS_KEY is set without OKAY_TLS_CERT")), other.toString)

      val both = Serve.parse(Array(root.toString),
        Map("OKAY_TLS_CERT" -> "/x/cert.pem", "OKAY_TLS_KEY" -> "file:/x/key.pem").get)
      assertEquals(both.map(_.tls), Right(Some(("/x/cert.pem", okay.conf.Secret("file:/x/key.pem")))))
      // the key travelled as a REFERENCE the whole way
      assertEquals(both.map(_.tls.get._2.toString), Right("file:/x/key.pem"))
    }
  }

  test("an ACME email without domains asks for nothing, and with them is the whole triple") {
    pages { root =>
      assertEquals(Serve.parse(Array(root.toString), Map("OKAY_ACME" -> "a@b.c").get).map(_.acme), Right(None))
      assertEquals(
        Serve.parse(Array(root.toString),
          Map("OKAY_ACME" -> "a@b.c", "OKAY_ACME_DOMAINS" -> "shop.example.com, www.example.com",
            "OKAY_ACME_PROD" -> "1").get).map(_.acme),
        Right(Some(("a@b.c", Vector("shop.example.com", "www.example.com"), true))))
    }
  }

  test("the deployment writes only what it overrides, not the program's own defaults restated") {
    val web = ScriptDeploy.system.service("web").getOrElse(fail("no web service"))
    assertEquals(web.settings.env.map(_._1).sorted, Vector("OKAY_OPS", "OKAY_PAGES", "OKAY_PORT"))
    // a unit file restating a default is a lie waiting for the
    // default to change: OKAY_LANGS is not in there
    assert(!web.settings.env.exists(_._1 == "OKAY_LANGS"), web.settings.env.toString)
  }

  test("what the deployment sets is what the program then reads, end to end") {
    pages { root =>
      val web = ScriptDeploy.system.service("web").getOrElse(fail("no web service"))
      // the deployment's own settings, with the one path that must be
      // real on this machine pointed at a real directory
      val env = web.settings.env.toMap.updated("OKAY_PAGES", root.toString)
      val a = Serve.parse(Array.empty, env.get)
      assertEquals(a.map(_.port), Right(8080))
      assertEquals(a.map(_.ops), Right(true))
      assertEquals(a.map(_.root.toString), Right(root.toString))
    }
  }
