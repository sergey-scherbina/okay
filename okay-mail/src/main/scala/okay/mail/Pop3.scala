package okay.mail

import okay.conf.{Secret, Secrets}
import okay.tls.{Tls, TlsConfig}
import java.io.{BufferedReader, InputStreamReader, OutputStreamWriter, Writer}
import java.net.Socket
import java.nio.charset.StandardCharsets.UTF_8

/**
 * Receiving (specs/mail.md, "Receiving — POP3").
 *
 * `Smtp`'s header says receiving "is IMAP or POP, a different and much
 * larger module". That is true of IMAP and not of POP3: eight
 * commands, `+OK`/`-ERR`, and a block terminated by a lone dot. What
 * the consumer asks for is *what arrived since last time*, which is
 * `UIDL`, `RETR` and `DELE` — folders, flags, search and push are
 * IMAP's reasons to exist and none of them was asked for.
 *
 * The split is the one this module already has, with the seam moved:
 * POP3's answer is one line or a dot-terminated block depending on the
 * command just sent, so the pure half is a conversation over a `Wire`
 * rather than a reply-driven machine. `talk` decides every line and
 * every outcome and is tested against a scripted wire with no socket
 * at all; `read` is that wire over a real one.
 */
object Pop3:

  /** what went wrong, as DATA — "wrong password" and "the host is
   * unreachable" are different answers to give an operator, and an
   * exception collapses them into "it did not work" */
  enum Failure:
    case Connection(why: String)
    case Auth(why: String)
    case Protocol(why: String)

    def message: String = this match
      case Connection(w) => s"connection: $w"
      case Auth(w) => s"auth: $w"
      case Protocol(w) => s"protocol: $w"

  /** how the channel is made private */
  enum Security:
    /** TLS from the first byte — port 995, what mailboxes offer today */
    case Implicit
    /** plain, then `STLS` — port 110 */
    case StartTls
    /** no TLS at all: a test, a loopback, and a choice a caller states
     * rather than one it inherits */
    case Plain

  final case class Server(host: String,
                          port: Int = 995,
                          user: String = "",
                          password: Secret = Secret(""),
                          security: Security = Security.Implicit,
                          tls: TlsConfig = TlsConfig(),
                          timeoutMs: Int = 30000)

  /** one message as it arrived. NO MIME here: a wire hands over bytes,
   * and what a subject or a body means is the consumer's question */
  final case class Message(number: Int, uid: String, raw: String)

  /**
   * One connection's line I/O, and the whole seam.
   *
   * A test scripts this and the conversation below never knows the
   * difference; `socket` is the only implementation that touches a
   * network.
   */
  trait Wire:
    /** the next line, or None when the far end closed */
    def line(): Option[String]
    /** a response block: every line up to a lone `.`, dot-unstuffed */
    def block(): Vector[String]
    def write(s: String): Unit
    /** make the channel private; `Left` when that failed */
    def upgrade(): Either[String, Unit]

  private def ok(l: Option[String]): Boolean = l.exists(_.startsWith("+OK"))
  private def why(l: Option[String]): String =
    l.map(_.dropWhile(_ != ' ').trim).filter(_.nonEmpty)
      .getOrElse(l.getOrElse("the server closed the connection"))

  /**
   * The whole session, over a `Wire`.
   *
   * `handle` runs INSIDE the session and its answer decides `DELE`:
   * **true means the caller has taken responsibility for that
   * message** — journalled it, in the consumer's case — and it may be
   * removed. False, or a throw, leaves it in the mailbox for the next
   * pass, because a caller that threw has not taken responsibility for
   * anything.
   *
   * Nothing is actually removed until `QUIT`: that is POP3's own rule
   * and it is the reason this is safe to interrupt. A session that
   * dies mid-pass deletes NOTHING, so a re-read is the repair.
   */
  def talk(wire: Wire, server: Server, secrets: Secrets, most: Int)
          (handle: Message => Boolean): Either[Failure, Vector[Message]] =
    def cmd(line: String): Option[String] =
      wire.write(line)
      wire.line()

    // the greeting comes before anything, and on an implicit-TLS
    // connection the socket is already private when it arrives
    if server.security == Security.StartTls then
      val greeting = wire.line()
      if !ok(greeting) then Left(Failure.Connection(why(greeting)))
      else if !ok(cmd("STLS")) then Left(Failure.Connection("the server refused STLS"))
      else wire.upgrade() match
        case Left(w) => Left(Failure.Connection(s"STLS: $w"))
        case Right(_) => authenticate(wire, server, secrets, most)(handle)
    else
      val greeting = wire.line()
      if !ok(greeting) then Left(Failure.Connection(why(greeting)))
      else authenticate(wire, server, secrets, most)(handle)

  private def authenticate(wire: Wire, server: Server, secrets: Secrets, most: Int)
                          (handle: Message => Boolean): Either[Failure, Vector[Message]] =
    def cmd(line: String): Option[String] =
      wire.write(line)
      wire.line()
    secrets.get(server.password) match
      case Left(missing) => Left(Failure.Auth(s"the password is not in Secrets: $missing"))
      case Right(pass) =>
        if !ok(cmd(s"USER ${server.user}")) then Left(Failure.Auth("USER refused"))
        else if !ok(cmd(s"PASS $pass")) then Left(Failure.Auth("PASS refused"))
        else collect(wire, most)(handle)

  /**
   * `UIDL` first, because a message needs an identity the caller can
   * recognise across sessions — a number is only its position in
   * today's mailbox. A server that refuses `UIDL` (they exist) leaves
   * the number as the only identity there is, and that is said in the
   * uid rather than pretended about.
   */
  private def collect(wire: Wire, most: Int)
                     (handle: Message => Boolean): Either[Failure, Vector[Message]] =
    wire.write("UIDL")
    val listed =
      if ok(wire.line()) then
        wire.block().flatMap { row =>
          row.trim.split("\\s+", 2) match
            case Array(n, uid) => n.toIntOption.map(_ -> uid)
            case _ => None
        }
      else Vector.empty

    val numbers =
      if listed.nonEmpty then listed
      else
        // no UIDL: LIST gives the numbers, and the identity is then
        // "message 3 in this mailbox today", which is honest and weak
        wire.write("LIST")
        if !ok(wire.line()) then Vector.empty
        else wire.block().flatMap(_.trim.split("\\s+").headOption.flatMap(_.toIntOption))
          .map(n => n -> s"n$n")

    var taken = Vector.empty[Message]
    var failed: Option[Failure] = None
    for (n, uid) <- numbers.take(most) if failed.isEmpty do
      wire.write(s"RETR $n")
      if !ok(wire.line()) then failed = Some(Failure.Protocol(s"RETR $n refused"))
      else
        val message = Message(n, uid, wire.block().mkString("\r\n"))
        val mine =
          try handle(message)
          catch case _: Throwable => false
        if mine then
          taken = taken :+ message
          wire.write(s"DELE $n")
          wire.line(): Unit   // a refused DELE leaves the message; the next pass sees it again
    wire.write("QUIT")
    wire.line(): Unit
    failed.toLeft(taken)

  /**
   * The socket half, and nothing else. `Smtp.blocking`'s shape: open,
   * converse, close whatever happened.
   */
  def read(server: Server, secrets: Secrets = Secrets.env, most: Int = 50)
          (handle: Message => Boolean): Either[Failure, Vector[Message]] =
    var sock: Socket = null
    try
      sock = Socket(server.host, server.port)
      sock.setSoTimeout(server.timeoutMs)
      if server.security == Security.Implicit then
        Tls.client(sock, server.host, server.tls, secrets) match
          case Left(w) => Left(Failure.Connection(s"TLS: $w"))
          case Right(upgraded) =>
            sock = upgraded
            talk(socket(() => sock, s => sock = s, server, secrets), server, secrets, most)(handle)
      else talk(socket(() => sock, s => sock = s, server, secrets), server, secrets, most)(handle)
    catch
      case e: Throwable => Left(Failure.Connection(s"${e.getClass.getSimpleName}: ${e.getMessage}"))
    finally
      if sock != null then try sock.close() catch case _: Throwable => ()

  /** a `Wire` over a socket that may be replaced under it by `STLS` */
  private def socket(get: () => Socket, set: Socket => Unit,
                     server: Server, secrets: Secrets): Wire = new Wire:
    private var in: BufferedReader = reader(get())
    private var out: Writer = writer(get())

    def line(): Option[String] = Option(in.readLine())

    def block(): Vector[String] =
      val lines = scala.collection.mutable.Buffer[String]()
      var done = false
      while !done do
        in.readLine() match
          case null => done = true
          case "." => done = true
          // DOT-UNSTUFFING, the mirror of what DATA does on the way
          // out: a body line beginning with a dot arrives doubled
          case l if l.startsWith("..") => lines += l.drop(1)
          case l => lines += l
      lines.toVector

    def write(s: String): Unit =
      out.write(s); out.write("\r\n"); out.flush()

    def upgrade(): Either[String, Unit] =
      Tls.client(get(), server.host, server.tls, secrets) match
        case Left(w) => Left(w)
        case Right(upgraded) =>
          set(upgraded)
          in = reader(upgraded)
          out = writer(upgraded)
          Right(())

  private def reader(s: Socket) = BufferedReader(InputStreamReader(s.getInputStream, UTF_8))
  private def writer(s: Socket): Writer = OutputStreamWriter(s.getOutputStream, UTF_8)
