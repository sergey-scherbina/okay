package okay.mail

import okay.*
import okay.given
import okay.conf.{Secret, Secrets}
import okay.mail.Mail.{Accepted, Rejection}
import okay.tls.{Tls, TlsConfig}
import java.io.{BufferedReader, InputStreamReader, OutputStreamWriter, Writer}
import java.net.Socket
import java.nio.charset.StandardCharsets.UTF_8
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter

/**
 * The socket half (specs/mail.md).
 *
 * Deliberately thin: it opens a socket, reads reply groups, writes the
 * lines `Session` decides on, and upgrades to TLS when told to. Every
 * decision about WHAT to write is in `Session`, where it is tested
 * against scripted replies with no network at all, so what is left
 * here is only the carrying of bytes.
 */
object Smtp {

  final case class Server(host: String,
                          port: Int = 587,
                          from: Address,
                          login: Option[Session.Login] = None,
                          requireTls: Boolean = true,
                          tls: TlsConfig = TlsConfig(),
                          ehloAs: String = "localhost",
                          timeoutMs: Int = 30000)

  /** the credentials, from `Secrets` rather than inline: the same rule
   * `okay-tls` holds for a private key, for the same reason */
  def login(user: String, password: Secret,
            secrets: Secrets = Secrets.env): Either[String, Session.Login] =
    secrets.get(password).map(Session.Login(user, _))

  /**
   * A `Send` that talks to a real server.
   *
   * The effect is `Async` because a socket blocks; a caller wires this
   * in `main` and a `Mail.Recorder` in a test, which is the shape
   * `Transport` already has.
   */
  def send(server: Server, secrets: Secrets = Secrets.env)
          (mail: Mail): Either[Rejection, Accepted] ! Async =
    okay.effect[Async, Either[Rejection, Accepted]](Async.Run(() => blocking(server, secrets, mail)))

  /** the same, without the effect, for a caller that already owns a
   * thread and wants the plain answer */
  def blocking(server: Server, secrets: Secrets, mail: Mail)
  : Either[Rejection, Accepted] =
    var sock: Socket = null
    try
      sock = new Socket(server.host, server.port)
      sock.setSoTimeout(server.timeoutMs)
      converse(server, secrets, mail, sock)
    catch
      case e: Throwable => Left(Rejection.Connection(s"${e.getClass.getSimpleName}: ${e.getMessage}"))
    finally
      if sock != null then try sock.close() catch case _: Throwable => ()

  private def converse(server: Server, secrets: Secrets, mail: Mail, first: Socket)
  : Either[Rejection, Accepted] =
    val cfg = Session.Config(server.host, server.from.email, server.login,
      server.requireTls, server.ehloAs)
    val step = Session.next(cfg, mail, stamp(server.host))

    var sock = first
    var in = reader(sock)
    var out = writer(sock)
    var state = Session.start
    var answer: Option[Either[Rejection, Accepted]] = None

    while answer.isEmpty do
      Session.reply(group(in)) match
        case None => answer = Some(Left(Rejection.Connection("the server closed the connection")))
        case Some(r) =>
          step(state, r) match
            case Session.Step.Done(result) => answer = Some(result)
            case Session.Step.Write(lines, next) =>
              lines.foreach { l => out.write(l); out.write("\r\n") }
              out.flush()
              state = next
            case Session.Step.Upgrade(next) =>
              Tls.client(sock, server.host, server.tls, secrets) match
                case Left(why) => answer = Some(Left(Rejection.Connection(s"STARTTLS: $why")))
                case Right(upgraded) =>
                  sock = upgraded
                  in = reader(sock)
                  out = writer(sock)
                  // the upgraded channel starts a new conversation:
                  // EHLO again, because capabilities may differ once
                  // the channel is private
                  out.write(s"EHLO ${server.ehloAs}"); out.write("\r\n"); out.flush()
                  state = next
    answer.getOrElse(Left(Rejection.Connection("no answer")))

  /** one reply GROUP: continuation lines carry a hyphen after the code */
  private def group(in: BufferedReader): Vector[String] =
    val lines = scala.collection.mutable.Buffer[String]()
    var done = false
    while !done do
      val line = in.readLine()
      if line == null then done = true
      else
        lines += line
        if Session.complete(line) then done = true
    lines.toVector

  private def reader(s: Socket) = new BufferedReader(new InputStreamReader(s.getInputStream, UTF_8))
  private def writer(s: Socket): Writer = new OutputStreamWriter(s.getOutputStream, UTF_8)

  /** the Date and Message-ID headers, which are the only two things
   * this module reads a clock for */
  private def stamp(host: String): (String, String) =
    val now = ZonedDateTime.now()
    (now.format(DateTimeFormatter.RFC_1123_DATE_TIME),
      s"<${java.util.UUID.randomUUID()}@$host>")
}
