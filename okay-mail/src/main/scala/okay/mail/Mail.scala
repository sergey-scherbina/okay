package okay.mail

import java.nio.charset.StandardCharsets.UTF_8
import java.util.Base64

/**
 * One message (specs/mail.md).
 *
 * A name and an address, a subject, a UTF-8 body, and whatever
 * headers a caller needs. No attachments, no multipart, no templates:
 * the consumer who asked for this argued for exactly that, and the
 * rule it follows is the one the frame work already uses — a feature
 * arrives with its reader or it does not arrive.
 */
final case class Address(email: String, name: Option[String] = None):
  /** the envelope form, which is the bare address and never the name */
  def envelope: String = s"<${email.trim}>"

  /** the header form, with the display name as an encoded word when it
   * is not plain ASCII */
  def header: String = name match
    case None => email.trim
    case Some(n) => s"${Mail.word(n)} <${email.trim}>"

final case class Mail(from: Address,
                      to: Seq[Address],
                      subject: String,
                      body: String,
                      headers: Vector[(String, String)] = Vector.empty)

object Mail {

  /** what a `Send` answers with when the server took the message */
  final case class Accepted(reply: String)

  /**
   * Why it did not go, AS DATA.
   *
   * The consumer's own argument, and it is right: a rejected
   * recipient, a full mailbox and a refused relay are three different
   * things, and a caller answering a person needs to tell them apart.
   * An exception collapses all three into "it did not work", which is
   * exactly the message a person cannot act on.
   */
  enum Rejection:
    /** this ADDRESS was refused — a mail to three people where one is
     * dead must not read as a total failure */
    case Recipient(address: String, code: Int, text: String)
    case MailboxFull(address: String, code: Int, text: String)
    case RelayRefused(code: Int, text: String)
    case Auth(code: Int, text: String)
    /** the socket, the handshake, the timeout — before any SMTP
     * conversation could happen */
    case Connection(what: String)
    /** the server answered something the protocol does not allow here,
     * and the step is named so a log says where */
    case Protocol(step: String, code: Int, text: String)

  /** a mail sender: an effect, so `main` wires a server and a test
   * wires a recorder — the shape `Transport` already has */
  type Send = Mail => Either[Rejection, Accepted]

  /** a `Send` that keeps what it was given and refuses nothing: the
   * test double, and the development stand-in a caller can print from */
  final class Recorder extends (Mail => Either[Rejection, Accepted]):
    private val kept = scala.collection.mutable.Buffer[Mail]()
    def apply(m: Mail): Either[Rejection, Accepted] =
      kept += m
      Right(Accepted("250 2.0.0 recorded"))
    def sent: Vector[Mail] = kept.toVector
    def last: Option[Mail] = kept.lastOption

  // ── rendering ─────────────────────────────────────────────────────

  /** an RFC 2047 encoded-word, for a header that is not plain ASCII.
   * The consumer asking for this writes Ukrainian and Polish, so a
   * subject line that survives is not a nicety. */
  def word(s: String): String =
    if s.forall(c => c >= 0x20 && c < 0x7f) then s
    else s"=?UTF-8?B?${Base64.getEncoder.encodeToString(s.getBytes(UTF_8))}?="

  /** base64 in 76-character lines, which is what a DATA body must be
   * when it carries anything but ASCII: 8-bit bytes through a server
   * that never advertised 8BITMIME is corruption nobody sees until a
   * person reads it */
  def base64(s: String): Vector[String] =
    Base64.getEncoder.encodeToString(s.getBytes(UTF_8)).grouped(76).toVector

  /**
   * The DATA payload, dot-stuffed.
   *
   * A body line that begins with `.` must be doubled or the server
   * reads it as the end of the message — the oldest bug in SMTP
   * clients. Base64 output cannot contain one, and this runs anyway
   * because the headers a caller supplies are not base64 and the rule
   * is about lines rather than about bodies.
   */
  def data(m: Mail, date: String, id: String): Vector[String] =
    val fixed = Vector(
      "From" -> m.from.header,
      "To" -> m.to.map(_.header).mkString(", "),
      "Subject" -> word(m.subject),
      "Date" -> date,
      "Message-ID" -> id,
      "MIME-Version" -> "1.0",
      "Content-Type" -> "text/plain; charset=utf-8",
      "Content-Transfer-Encoding" -> "base64")
    val extra = m.headers.filterNot((k, _) => fixed.exists(_._1.equalsIgnoreCase(k)))
    ((fixed ++ extra).map((k, v) => s"$k: $v") ++ Vector("") ++ base64(m.body))
      .map(l => if l.startsWith(".") then "." + l else l)
}
