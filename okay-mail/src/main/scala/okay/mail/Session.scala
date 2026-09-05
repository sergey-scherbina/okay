package okay.mail

import okay.mail.Mail.Rejection
import java.nio.charset.StandardCharsets.UTF_8
import java.util.Base64

/**
 * The SMTP conversation, as a function (specs/mail.md).
 *
 * A client writes a line, the server answers with a code and some
 * text, and what the client writes next depends on that answer. That
 * is a pure function of the state and the reply, and keeping it one is
 * what makes this module testable without a network: every line the
 * client will ever write is decided here, against scripted replies, in
 * the default gate. The socket half only carries bytes.
 */
object Session {

  /** one server reply, however many lines it arrived on */
  final case class Reply(code: Int, text: String):
    def ok: Boolean = code >= 200 && code < 400

  /**
   * Server replies are multi-line when a hyphen follows the code:
   *
   *     250-mail.example.com
   *     250-STARTTLS
   *     250 HELP
   *
   * The code is the last line's and the text is every line's, joined,
   * because the capability list a client needs is in the MIDDLE lines
   * and reading only the last one is how a client decides a server has
   * no STARTTLS when it has just announced one.
   */
  def reply(lines: Seq[String]): Option[Reply] =
    lines.lastOption.flatMap { last =>
      last.take(3).toIntOption.map(c => Reply(c, lines.map(_.drop(4)).mkString("\n")))
    }

  /** is this reply line the last of its group */
  def complete(line: String): Boolean =
    (line.length >= 4 && line.charAt(3) != '-') || line.length == 3

  /** how to authenticate, and with what */
  final case class Login(user: String, password: String)

  final case class Config(host: String,
                          from: String,
                          login: Option[Login] = None,
                          /** refuse to send in the clear: the DEFAULT,
                           * so sending without TLS is a choice a
                           * caller states rather than one it inherits */
                          requireTls: Boolean = true,
                          ehloAs: String = "localhost")

  /** where the conversation is */
  enum State:
    case Greeting, Ehlo, StartTls, EhloAgain, Auth, AuthUser, AuthPassword
    case MailFrom
    case Rcpt(remaining: List[Address], sent: List[Address])
    case Data, Body, Quit

  /** what the client does next */
  enum Step:
    /** write these lines, then expect a reply */
    case Write(lines: Vector[String], next: State)
    /** the same, but upgrade the socket to TLS before writing */
    case Upgrade(next: State)
    case Done(result: Either[Rejection, Mail.Accepted])

  private def advertises(r: Reply, what: String): Boolean =
    r.text.toUpperCase.contains(what)

  /** the SASL PLAIN payload: NUL, user, NUL, password, base64.
   * The separators are written as ESCAPES rather than typed: a literal
   * NUL in a source file is invisible to every reader of it, and this
   * one arrived that way once already. */
  private def plain(l: Login): String =
    Base64.getEncoder.encodeToString(
      ("\u0000" + l.user + "\u0000" + l.password).getBytes(UTF_8))

  private def b64(s: String): String =
    Base64.getEncoder.encodeToString(s.getBytes(UTF_8))

  /**
   * A rejection code read for WHAT it means, not only for whether it
   * failed.
   *
   * 452 and 552 are a mailbox that cannot take it now or at all; 550,
   * 551 and 553 are the address itself; a 550 or 554 mentioning
   * relaying is the server refusing to CARRY it, which is a
   * configuration problem rather than a bad address. Telling a person
   * the wrong one of those sends them to fix something that is not
   * broken.
   */
  def rejection(step: String, address: Option[String], r: Reply): Rejection =
    val relaying = r.text.toLowerCase.contains("relay")
    (r.code, address) match
      case (c, _) if relaying && (c == 550 || c == 554) => Rejection.RelayRefused(c, r.text)
      case (c @ (452 | 552), Some(a)) => Rejection.MailboxFull(a, c, r.text)
      case (c @ (550 | 551 | 553), Some(a)) => Rejection.Recipient(a, c, r.text)
      case (c, _) if c == 535 || c == 534 || c == 454 => Rejection.Auth(c, r.text)
      case (c, _) => Rejection.Protocol(step, c, r.text)

  /** the server greets first, unprompted */
  def start: State = State.Greeting

  /**
   * The whole protocol. Every line the client writes comes from here.
   *
   * `stamp` is (Date, Message-ID), supplied rather than read from a
   * clock, so that a test of the wire is a test of the wire.
   */
  def next(cfg: Config, mail: Mail, stamp: (String, String))
          (state: State, r: Reply): Step =
    val (date, id) = stamp
    state match
      case State.Greeting =>
        if r.code == 220 then Step.Write(Vector(s"EHLO ${cfg.ehloAs}"), State.Ehlo)
        else Step.Done(Left(rejection("greeting", None, r)))

      case State.Ehlo =>
        if !r.ok then Step.Done(Left(rejection("EHLO", None, r)))
        else if advertises(r, "STARTTLS") then Step.Write(Vector("STARTTLS"), State.StartTls)
        else if cfg.requireTls then
          Step.Done(Left(Rejection.Connection(
            "the server does not offer STARTTLS and requireTls is set: " +
              "sending in the clear has to be a caller's stated choice")))
        else afterHandshake(cfg, r)

      case State.StartTls =>
        if r.code == 220 then Step.Upgrade(State.EhloAgain)
        else Step.Done(Left(rejection("STARTTLS", None, r)))

      // the capabilities are re-read after the upgrade, because a
      // server may advertise AUTH only once the channel is private
      case State.EhloAgain =>
        if r.ok then afterHandshake(cfg, r)
        else Step.Done(Left(rejection("EHLO", None, r)))

      case State.Auth =>
        if r.code == 334 then
          cfg.login match
            case Some(l) => Step.Write(Vector(b64(l.user)), State.AuthUser)
            case None => Step.Done(Left(Rejection.Auth(r.code,
              "the server asked for credentials and none were configured")))
        else if r.ok then mailFrom(cfg)
        else Step.Done(Left(rejection("AUTH", None, r)))

      case State.AuthUser =>
        if r.code == 334 then
          cfg.login match
            case Some(l) => Step.Write(Vector(b64(l.password)), State.AuthPassword)
            case None => Step.Done(Left(Rejection.Auth(r.code, "no password")))
        else Step.Done(Left(rejection("AUTH LOGIN", None, r)))

      case State.AuthPassword =>
        if r.ok then mailFrom(cfg) else Step.Done(Left(rejection("AUTH", None, r)))

      case State.MailFrom =>
        if r.ok then rcpt(mail.to.toList, Nil)
        else Step.Done(Left(rejection("MAIL FROM", Some(cfg.from), r)))

      case State.Rcpt(remaining, sent) =>
        // this reply answers the address at the head of `remaining`,
        // which is what the state was built with
        remaining match
          case a :: rest =>
            if r.ok then rcpt(rest, a :: sent)
            else Step.Done(Left(rejection("RCPT TO", Some(a.email), r)))
          case Nil =>
            if r.ok then Step.Write(Vector("DATA"), State.Data)
            else Step.Done(Left(rejection("RCPT TO", None, r)))

      case State.Data =>
        if r.code == 354 then Step.Write(Mail.data(mail, date, id) :+ ".", State.Body)
        else Step.Done(Left(rejection("DATA", None, r)))

      case State.Body =>
        if r.ok then Step.Write(Vector("QUIT"), State.Quit)
        else Step.Done(Left(rejection("body", None, r)))

      // whatever the server says to QUIT, the message was accepted at
      // the previous step and that is the answer
      case State.Quit => Step.Done(Right(Mail.Accepted("250 accepted")))

  private def afterHandshake(cfg: Config, r: Reply): Step =
    cfg.login match
      case Some(l) if advertises(r, "AUTH") && advertises(r, "PLAIN") =>
        Step.Write(Vector(s"AUTH PLAIN ${plain(l)}"), State.Auth)
      case Some(_) if advertises(r, "AUTH") => Step.Write(Vector("AUTH LOGIN"), State.Auth)
      case Some(_) => Step.Done(Left(Rejection.Auth(0,
        "a login was configured and the server advertises no AUTH")))
      case None => mailFrom(cfg)

  private def mailFrom(cfg: Config): Step =
    Step.Write(Vector(s"MAIL FROM:<${cfg.from}>"), State.MailFrom)

  /** one RCPT per recipient, so a refusal names the address */
  private def rcpt(remaining: List[Address], sent: List[Address]): Step =
    remaining match
      case a :: _ => Step.Write(Vector(s"RCPT TO:${a.envelope}"), State.Rcpt(remaining, sent))
      case Nil => Step.Write(Vector("DATA"), State.Data)
}
