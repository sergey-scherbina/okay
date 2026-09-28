package okay.telegram

import okay.*
import okay.given
import okay.ui.{Host, Telegram}
import okay.ui.Telegram.Act

/**
 * okay-ui's HOST, PER CHAT, OVER A BOT (specs/telegram-bot.md).
 *
 * `okay.ui.Telegram.host(perform)` is one chat's screen: its acts leave
 * as data and a consumer performs them. `Chats.perform` is that consumer
 * — the Bot API call for each act — and `Chats` keeps one host per chat,
 * opening the application the first time a chat speaks and handing that
 * chat's later presses and messages to its host. The application itself
 * is the consumer's `Ui.run(…)(host)`, with its own `Scheduler` and
 * `CanBlock`, so this module needs neither.
 */
object Chats:

  /** the performer of one chat's acts; a Send answers its message id. A
   * `Refused` reaches `refused` and the act answers `None` — one failed
   * call does not stop the host */
  def perform(bot: Bot, chat: Long, refused: Refused => Unit ! Async = _ => pure(())): Act => Option[Long] ! Async =
    def told[A](r: Either[Refused, A]): Option[A] ! Async = r match
      case Right(a) => pure(Some(a))
      case Left(x) => refused(x).map(_ => None)
    {
      case Act.Send(m) => bot.send(chat, m.text, m.keyboard).flatMap(told)
      case Act.Edit(id, m) => bot.edit(chat, id, m.text, m.keyboard).flatMap(told).map(_ => None)
      case Act.Answer(cb, notice) => bot.answerCallback(cb, notice).flatMap(told).map(_ => None)
      case Act.Ask(prompt) => bot.send(chat, prompt, forceReply = true).flatMap(told).map(_ => None)
    }

  /** an update as what a chat's host hears, and which chat: a press, a
   * message; a payment or a pre-checkout is the consumer's, not the host's */
  def heard(u: Update): Option[(Long, Telegram.Update)] = u match
    case Update.Callback(_, chat, _, _, data, cb) => Some(chat -> Telegram.Update.Pressed(data, cb))
    case Update.Message(_, chat, _, _, text) => Some(chat -> Telegram.Update.Said(text))
    case _ => None

final class Chats(bot: Bot, open: (Long, Host) => Unit ! Async,
                  refused: Refused => Unit ! Async = _ => pure(()))(using Scheduler):
  private var doors = Map.empty[Long, Telegram.Update => Unit ! Async]
  private val lock = new Object

  /** the chats with an application open */
  def opened: Set[Long] = lock.synchronized(doors.keySet)

  /** the chat's door, opening its application first when this is the
   * chat's first word — the one place a host is made */
  private def doorOf(chat: Long): (Telegram.Update => Unit ! Async) ! Async =
    async {
      lock.synchronized {
        doors.get(chat) match
          case Some(door) => (door, None)
          case None =>
            val (host, door) = Telegram.host(Chats.perform(bot, chat, refused))
            doors += chat -> door
            (door, Some(host))
      }
    }.map { (door, opened) =>
      opened.foreach(host => Async.spawn(open(chat, host)))
      door
    }

  /** the application of a chat, opened if it was not — for a consumer
   * that understood a message itself and wants the screen, not the
   * host's reading of the text (a sentence is not a field's value) */
  def open(chat: Long): Unit ! Async = doorOf(chat).map(_ => ())

  /** a press or a message: the chat's host hears it; the first from a
   * chat opens its application */
  def hear(u: Update): Unit ! Async = Chats.heard(u) match
    case None => pure(())
    case Some((chat, heard)) => doorOf(chat).flatMap(_(heard))
