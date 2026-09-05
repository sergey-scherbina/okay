package okay.mail

import okay.mail.Mail.Rejection

/**
 * The whole protocol, with no network (specs/mail.md).
 *
 * Every line this client will ever write is decided by `Session`, so
 * every line can be tested by handing it scripted server replies. That
 * is the reason the wire and the socket are separate files: a mail
 * client whose only test needs a mail server has no tests.
 */
class TestSession extends munit.FunSuite {

  private val from = Address("noreply@example.com", Some("Example"))
  private val to = Address("ada@example.org")
  private val mail = Mail(from, Seq(to), "Your code", "Your code is 123456.")
  private val stamp = ("Fri, 5 Sep 2026 10:00:00 +0000", "<id@example.com>")

  private def cfg(login: Option[Session.Login] = None, requireTls: Boolean = true) =
    Session.Config("mail.example.com", from.email, login, requireTls)

  private def step(c: Session.Config)(s: Session.State, code: Int, text: String) =
    Session.next(c, mail, stamp)(s, Session.Reply(code, text))

  /** drive the conversation with a script of replies, collecting every
   * line the client writes */
  private def run(c: Session.Config, script: Seq[(Int, String)])
  : (Vector[String], Option[Either[Rejection, Mail.Accepted]]) =
    var state = Session.start
    val written = scala.collection.mutable.Buffer[String]()
    var out: Option[Either[Rejection, Mail.Accepted]] = None
    script.foreach { (code, text) =>
      if out.isEmpty then
        step(c)(state, code, text) match
          case Session.Step.Done(r) => out = Some(r)
          case Session.Step.Write(lines, next) => written ++= lines; state = next
          case Session.Step.Upgrade(next) => written += "<upgrade>"; state = next
    }
    (written.toVector, out)

  // ── reading what the server said ──────────────────────────────────

  test("a multi-line reply is ONE reply, and its middle lines are the capabilities") {
    val r = Session.reply(Vector("250-mail.example.com", "250-STARTTLS", "250 HELP"))
    assertEquals(r.map(_.code), Some(250))
    assert(r.exists(_.text.contains("STARTTLS")),
      "reading only the last line is how a client decides a server has no STARTTLS")
  }

  test("a continuation line is not the end of a group") {
    assert(!Session.complete("250-STARTTLS"))
    assert(Session.complete("250 HELP"))
    assert(Session.complete("220"))
  }

  // ── the happy path ────────────────────────────────────────────────

  test("greeting, EHLO, STARTTLS, EHLO again, envelope, data, quit") {
    val (lines, out) = run(cfg(), Seq(
      220 -> "mail.example.com ESMTP",
      250 -> "mail.example.com\nSTARTTLS",
      220 -> "ready to start TLS",
      250 -> "mail.example.com\nSIZE 35882577",
      250 -> "ok",
      250 -> "ok",
      354 -> "end data with <CRLF>.<CRLF>",
      250 -> "queued as ABC123",
      221 -> "bye"))
    assertEquals(lines.take(3), Vector("EHLO localhost", "STARTTLS", "<upgrade>"))
    assert(lines.contains("MAIL FROM:<noreply@example.com>"), lines.toString)
    assert(lines.contains("RCPT TO:<ada@example.org>"), lines.toString)
    assert(lines.contains("DATA"))
    assert(lines.contains("."), "the body must end with a bare dot")
    assert(lines.contains("QUIT"))
    assertEquals(out.map(_.isRight), Some(true))
  }

  test("one RCPT per recipient, so a refusal can name the address") {
    val three = mail.copy(to = Seq(to, Address("b@example.org"), Address("c@example.org")))
    var state = Session.start
    val written = scala.collection.mutable.Buffer[String]()
    Seq(220 -> "x", 250 -> "x\nSTARTTLS", 220 -> "x", 250 -> "x", 250 -> "ok",
      250 -> "ok", 250 -> "ok", 250 -> "ok").foreach { (c, t) =>
      Session.next(cfg(), three, stamp)(state, Session.Reply(c, t)) match
        case Session.Step.Write(ls, n) => written ++= ls; state = n
        case Session.Step.Upgrade(n) => state = n
        case Session.Step.Done(_) => ()
    }
    assertEquals(written.count(_.startsWith("RCPT TO:")), 3)
  }

  // ── failure as data ───────────────────────────────────────────────

  test("a dead address is a Recipient rejection that NAMES it") {
    val (_, out) = run(cfg(), Seq(
      220 -> "x", 250 -> "x\nSTARTTLS", 220 -> "x", 250 -> "x", 250 -> "ok",
      550 -> "5.1.1 no such user"))
    out match
      case Some(Left(Rejection.Recipient(a, 550, _))) => assertEquals(a, "ada@example.org")
      case other => fail(s"expected a named recipient rejection, got $other")
  }

  test("a full mailbox is not a bad address, and a refused relay is neither") {
    assertEquals(
      Session.rejection("RCPT TO", Some("a@b.c"), Session.Reply(452, "mailbox full")),
      Rejection.MailboxFull("a@b.c", 452, "mailbox full"))
    assertEquals(
      Session.rejection("RCPT TO", Some("a@b.c"), Session.Reply(550, "relay not permitted")),
      Rejection.RelayRefused(550, "relay not permitted"))
    // the same code, a different meaning, and a person sent to fix the
    // wrong one of these fixes nothing
    assertEquals(
      Session.rejection("RCPT TO", Some("a@b.c"), Session.Reply(550, "no such user")),
      Rejection.Recipient("a@b.c", 550, "no such user"))
  }

  test("a server with no STARTTLS is refused by default, and allowed when a caller says so") {
    val (_, refused) = run(cfg(), Seq(220 -> "x", 250 -> "mail.example.com\nSIZE 100"))
    refused match
      case Some(Left(Rejection.Connection(why))) => assert(why.contains("STARTTLS"), why)
      case other => fail(s"sending in the clear must not be the default: $other")

    val (lines, _) = run(cfg(requireTls = false), Seq(220 -> "x", 250 -> "x\nSIZE 100"))
    assert(lines.contains("MAIL FROM:<noreply@example.com>"),
      "with requireTls = false it should proceed in the clear")
  }

  // ── authentication ────────────────────────────────────────────────

  test("AUTH PLAIN when the server offers it") {
    val (lines, _) = run(cfg(Some(Session.Login("u", "p"))), Seq(
      220 -> "x", 250 -> "x\nSTARTTLS", 220 -> "x", 250 -> "x\nAUTH PLAIN LOGIN", 235 -> "ok"))
    assert(lines.exists(_.startsWith("AUTH PLAIN ")), lines.toString)
  }

  test("AUTH LOGIN when that is all there is, user then password") {
    val (lines, _) = run(cfg(Some(Session.Login("u", "p"))), Seq(
      220 -> "x", 250 -> "x\nSTARTTLS", 220 -> "x", 250 -> "x\nAUTH LOGIN",
      334 -> "VXNlcm5hbWU6", 334 -> "UGFzc3dvcmQ6", 235 -> "ok"))
    assertEquals(lines.filter(_ == "AUTH LOGIN").size, 1)
    // dXNlcg== style: the user and then the password, base64
    assert(lines.contains("dQ=="), lines.toString)  // "u"
    assert(lines.contains("cA=="), lines.toString)  // "p"
  }

  test("bad credentials are an Auth rejection, not a protocol one") {
    val (_, out) = run(cfg(Some(Session.Login("u", "p"))), Seq(
      220 -> "x", 250 -> "x\nSTARTTLS", 220 -> "x", 250 -> "x\nAUTH PLAIN",
      535 -> "5.7.8 authentication failed"))
    out match
      case Some(Left(Rejection.Auth(535, _))) => ()
      case other => fail(s"expected an Auth rejection, got $other")
  }

  // ── what goes down the wire ───────────────────────────────────────

  test("a body line beginning with a dot is doubled") {
    val m = mail.copy(body = "ok")
    val withDot = Mail.data(m.copy(headers = Vector("X-Odd" -> "v")), stamp._1, stamp._2)
    assert(withDot.forall(l => !l.startsWith(".") || l.startsWith("..")),
      "an undoubled leading dot ends the message early: the oldest bug in SMTP clients")
  }

  test("a UTF-8 body travels as base64 under a MIME trio") {
    val cyrillic = mail.copy(body = "Ваш код: 123456")
    val lines = Mail.data(cyrillic, stamp._1, stamp._2)
    assert(lines.contains("Content-Transfer-Encoding: base64"))
    assert(lines.contains("Content-Type: text/plain; charset=utf-8"))
    assert(lines.contains("MIME-Version: 1.0"))
    val body = lines.drop(lines.indexOf("") + 1).mkString
    assertEquals(new String(java.util.Base64.getDecoder.decode(body), "UTF-8"), "Ваш код: 123456")
  }

  test("a non-ASCII subject is an encoded word, and an ASCII one is left alone") {
    assertEquals(Mail.word("Your code"), "Your code")
    assertEquals(Mail.word("Ваш код"), "=?UTF-8?B?0JLQsNGIINC60L7QtA==?=")
    val ua = Mail.data(mail.copy(subject = "Ваш код"), stamp._1, stamp._2)
    assert(ua.exists(_.startsWith("Subject: =?UTF-8?B?")), ua.toString)
  }

  test("a display name is encoded in the header and never in the envelope") {
    val a = Address("ada@example.org", Some("Ада"))
    assert(a.header.startsWith("=?UTF-8?B?"), a.header)
    assertEquals(a.envelope, "<ada@example.org>")
  }

  test("a caller's own headers survive, and cannot overwrite the ones we set") {
    val m = mail.copy(headers = Vector("X-Trace" -> "abc", "Content-Type" -> "text/html"))
    val lines = Mail.data(m, stamp._1, stamp._2)
    assert(lines.contains("X-Trace: abc"))
    assertEquals(lines.count(_.startsWith("Content-Type:")), 1)
    assert(lines.contains("Content-Type: text/plain; charset=utf-8"),
      "a caller cannot claim a content type the body is not in")
  }

  // ── the test double ───────────────────────────────────────────────

  test("the recorder keeps what it was given, which is what a test asserts on") {
    val r = new Mail.Recorder
    val send: Mail.Send = r
    assert(send(mail).isRight)
    assertEquals(r.sent.size, 1)
    assertEquals(r.last.map(_.subject), Some("Your code"))
  }
}
