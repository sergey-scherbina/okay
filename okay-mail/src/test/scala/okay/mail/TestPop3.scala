package okay.mail

import okay.conf.{Secret, Secrets}

/**
 * The whole protocol, with no network (specs/mail.md, "Receiving").
 *
 * `TestSession` scripts server replies for the sending half; this
 * scripts a `Wire` for the receiving one, and for the same reason: a
 * mail client whose only test needs a mail server has no tests.
 */
class TestPop3 extends munit.FunSuite {

  /** a mailbox in a script: what the server answers, in order, and
   * what the client wrote to get there */
  final class Scripted(answers: Seq[Either[String, Vector[String]]]) extends Pop3.Wire:
    private var left = answers.toList
    val wrote = scala.collection.mutable.Buffer[String]()
    var upgraded = false

    private def next(): Either[String, Vector[String]] = left match
      case h :: t => left = t; h
      case Nil => Left("-ERR nothing scripted")

    def line(): Option[String] = next() match
      case Left(l) => Some(l)
      case Right(_) => Some("+OK")           // a block's own +OK was consumed by its Right

    def block(): Vector[String] = next() match
      case Right(b) => b
      case Left(_) => Vector.empty

    def write(s: String): Unit = wrote += s
    def upgrade(): Either[String, Unit] = { upgraded = true; Right(()) }

  private val secrets = new Secrets:
    def get(s: Secret): Either[String, String] =
      if s.ref == "missing" then Left("missing") else Right("hunter2")

  private val server = Pop3.Server("mail.example.com", 995, "ada@example.com",
    Secret("pass"), Pop3.Security.Implicit)

  private def mail(subject: String, body: String) =
    Vector(s"Subject: $subject", "From: someone@example.org", "", body)

  test("a pass: greet, log in, list, fetch, and delete only what was taken") {
    val w = Scripted(Seq(
      Left("+OK POP3 ready"),                       // greeting
      Left("+OK user accepted"),                    // USER
      Left("+OK logged in"),                        // PASS
      Left("+OK 2 messages"), Right(Vector("1 abc", "2 def")),   // UIDL + block
      Left("+OK 120 octets"), Right(mail("first", "здравствуйте")),
      Left("+OK deleted"),                          // DELE 1
      Left("+OK 90 octets"), Right(mail("second", "и вам")),
      Left("+OK bye")))                             // QUIT
    // the first is taken, the second is not
    var seen = Vector.empty[String]
    val got = Pop3.talk(w, server, secrets, most = 10) { m =>
      seen = seen :+ m.uid
      m.uid == "abc"
    }
    assertEquals(got.map(_.map(_.uid)), Right(Vector("abc")))
    assertEquals(seen, Vector("abc", "def"), "both were read")
    assertEquals(w.wrote, scala.collection.mutable.Buffer(
      "USER ada@example.com", "PASS hunter2", "UIDL", "RETR 1", "DELE 1", "RETR 2", "QUIT"),
      "DELE only for the message the caller took responsibility for")
    assert(got.toOption.get.head.raw.contains("здравствуйте"))
  }

  test("`most` bounds one pass, so ten thousand messages are not one turn") {
    val w = Scripted(Seq(
      Left("+OK ready"), Left("+OK"), Left("+OK"),
      Left("+OK"), Right(Vector("1 a", "2 b", "3 c", "4 d")),
      Left("+OK"), Right(mail("one", "x")), Left("+OK"),
      Left("+OK"), Right(mail("two", "y")), Left("+OK"),
      Left("+OK bye")))
    val got = Pop3.talk(w, server, secrets, most = 2)(_ => true)
    assertEquals(got.map(_.length), Right(2))
    assert(!w.wrote.contains("RETR 3"), w.wrote.toString)
  }

  test("a handler that throws has taken responsibility for nothing") {
    val w = Scripted(Seq(
      Left("+OK ready"), Left("+OK"), Left("+OK"),
      Left("+OK"), Right(Vector("1 a")),
      Left("+OK"), Right(mail("one", "x")),
      Left("+OK bye")))
    val got = Pop3.talk(w, server, secrets, most = 5)(_ => throw RuntimeException("the store is down"))
    assertEquals(got, Right(Vector.empty))
    assert(!w.wrote.exists(_.startsWith("DELE")),
      "a message whose handler threw stays in the mailbox for the next pass")
  }

  test("a refused password is Auth, not Connection — an operator needs the difference") {
    val w = Scripted(Seq(Left("+OK ready"), Left("+OK"), Left("-ERR authentication failed")))
    assertEquals(Pop3.talk(w, server, secrets, most = 5)(_ => true),
      Left(Pop3.Failure.Auth("PASS refused")))
    val closed = Scripted(Seq(Left("-ERR try later")))
    assert(Pop3.talk(closed, server, secrets, most = 5)(_ => true)
      .swap.exists(_.isInstanceOf[Pop3.Failure.Connection]))
  }

  test("a password that is not in Secrets never reaches the wire") {
    val w = Scripted(Seq(Left("+OK ready")))
    val got = Pop3.talk(w, server.copy(password = Secret("missing")), secrets, most = 5)(_ => true)
    assert(got.swap.exists(_.isInstanceOf[Pop3.Failure.Auth]), got.toString)
    assert(!w.wrote.exists(_.startsWith("PASS")), w.wrote.toString)
  }

  test("STLS upgrades before the credentials are written") {
    val w = Scripted(Seq(
      Left("+OK ready"), Left("+OK begin TLS"),      // greeting, STLS
      Left("+OK"), Left("+OK"),                      // USER, PASS
      Left("+OK"), Right(Vector.empty),              // UIDL, an empty mailbox
      Left("+OK bye")))
    val got = Pop3.talk(w, server.copy(security = Pop3.Security.StartTls), secrets, most = 5)(_ => true)
    assertEquals(got, Right(Vector.empty))
    assert(w.upgraded, "the channel was made private")
    assertEquals(w.wrote.take(2), scala.collection.mutable.Buffer("STLS", "USER ada@example.com"),
      "nothing about the password was written in the clear")
  }

  test("a server with no UIDL falls back to LIST, and says the identity is weak") {
    val w = Scripted(Seq(
      Left("+OK ready"), Left("+OK"), Left("+OK"),
      Left("-ERR UIDL not supported"),
      Left("+OK 1 messages"), Right(Vector("1 120")),
      Left("+OK"), Right(mail("one", "x")), Left("+OK"),
      Left("+OK bye")))
    val got = Pop3.talk(w, server, secrets, most = 5)(_ => true)
    assertEquals(got.map(_.map(_.uid)), Right(Vector("n1")))
    assert(w.wrote.contains("LIST"), w.wrote.toString)
  }
}
