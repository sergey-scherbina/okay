package okay.mail

import okay.conf.Secrets
import java.io.{BufferedReader, InputStreamReader, OutputStreamWriter}
import java.net.{InetAddress, ServerSocket}
import java.nio.charset.StandardCharsets.UTF_8

/**
 * The socket half, against a real server on a real port
 * (specs/mail.md).
 *
 * `TestSession` proves every line the client decides to write.
 * This proves the bytes reach a server and the answers come back, and
 * it is the only part that needs a socket — which is why it is
 * `Live`-tagged and out of `sbt test`, under the standing policy that
 * anything binding a real port is.
 *
 * The server here answers a script and records what it was told. It is
 * about forty lines because SMTP is a line protocol, which is the same
 * fact that makes the client small.
 */
class TestSmtpLoopback extends munit.FunSuite {

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  /** a server that answers `script` in order and keeps every line it
   * was sent; returns the port and a handle to what it heard */
  private def serve(script: Seq[String]): (Int, () => Vector[String], () => Unit) =
    val server = new ServerSocket(0, 1, InetAddress.getLoopbackAddress)
    val heard = scala.collection.mutable.Buffer[String]()
    val thread = new Thread(() =>
      try
        val sock = server.accept()
        val in = new BufferedReader(new InputStreamReader(sock.getInputStream, UTF_8))
        val out = new OutputStreamWriter(sock.getOutputStream, UTF_8)
        def say(s: String): Unit = { out.write(s); out.write("\r\n"); out.flush() }
        val rest = scala.collection.mutable.Queue(script*)
        say(rest.dequeue())                       // the greeting, unprompted
        var inData = false
        var line = in.readLine()
        while line != null && rest.nonEmpty do
          heard.synchronized { heard += line; () }
          if inData then
            // in DATA the server answers only the terminating dot
            if line == "." then { inData = false; say(rest.dequeue()) }
          else
            if line.equalsIgnoreCase("DATA") then { inData = true; say(rest.dequeue()) }
            else say(rest.dequeue())
          line = if rest.isEmpty then null else in.readLine()
        sock.close()
      catch case _: Throwable => ()
      finally try server.close() catch case _: Throwable => ())
    thread.setDaemon(true)
    thread.start()
    (server.getLocalPort, () => heard.synchronized(heard.toVector), () => server.close())

  private val from = Address("noreply@example.com", Some("Example"))
  private val mail = Mail(from, Seq(Address("ada@example.org")),
    "Ваш код", "Ваш код: 123456")

  test("a message reaches a server, and every line of it is what the wire expects") {
    val (port, heard, stop) = serve(Seq(
      "220 test ESMTP",
      "250-test\r\n250 SIZE 1000",   // no STARTTLS: this leg is in the clear
      "250 ok",                       // MAIL FROM
      "250 ok",                       // RCPT TO
      "354 go ahead",                 // DATA
      "250 2.0.0 queued as TEST1",    // the terminating dot
      "221 bye"))
    try
      val server = Smtp.Server("127.0.0.1", port, from, requireTls = false)
      val result = Smtp.blocking(server, Secrets.env, mail)
      assert(result.isRight, result.toString)

      val lines = heard()
      assert(lines.exists(_.startsWith("EHLO ")), lines.toString)
      assert(lines.contains("MAIL FROM:<noreply@example.com>"), lines.toString)
      assert(lines.contains("RCPT TO:<ada@example.org>"), lines.toString)
      assert(lines.contains("DATA"), lines.toString)
      assert(lines.contains("."), "the body must be terminated by a bare dot")
      // the subject travelled as an encoded word and the body as
      // base64, which is what makes Cyrillic survive a server that
      // never advertised 8BITMIME
      assert(lines.exists(_.startsWith("Subject: =?UTF-8?B?")), lines.toString)
      val body = lines.dropWhile(_ != "").drop(1).takeWhile(_ != ".").mkString
      assertEquals(new String(java.util.Base64.getDecoder.decode(body), UTF_8), "Ваш код: 123456")
    finally stop()
  }

  test("a refusal comes back as data naming the address, not as an exception") {
    val (port, _, stop) = serve(Seq(
      "220 test ESMTP",
      "250 test",
      "250 ok",
      "550 5.1.1 no such user"))
    try
      val server = Smtp.Server("127.0.0.1", port, from, requireTls = false)
      Smtp.blocking(server, Secrets.env, mail) match
        case Left(Mail.Rejection.Recipient(a, 550, _)) => assertEquals(a, "ada@example.org")
        case other => fail(s"expected a named recipient rejection, got $other")
    finally stop()
  }

  test("a server that is not there is a Connection rejection, and nothing throws") {
    // port 1 on loopback: nothing listens, and a caller must still get
    // an answer it can show a person
    val server = Smtp.Server("127.0.0.1", 1, from, requireTls = false, timeoutMs = 1000)
    Smtp.blocking(server, Secrets.env, mail) match
      case Left(Mail.Rejection.Connection(_)) => ()
      case other => fail(s"expected a connection rejection, got $other")
  }
}
