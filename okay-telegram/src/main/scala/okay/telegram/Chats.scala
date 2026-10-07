package okay.telegram

import okay.{Async, Scheduler, Timer, async}
import okay.freer.*
import okay.ui.{Host, Telegram}
import okay.ui.Telegram.{Act, Message}

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
   * call does not stop the host.
   *
   * `asked` hears the one act that puts the chat in a waiting state: an
   * `Ask` is the screen requesting a typed value, and whoever owns the
   * next message needs to know (see `Chats.awaiting`). */
  def perform(bot: Bot, chat: Long, refused: Refused => Unit ! Async = _ => pure(()),
              asked: () => Unit = () => ()): Act => Option[Long] ! Async =
    def told[A](r: Either[Refused, A]): Option[A] ! Async = r match
      case Right(a) => pure(Some(a))
      case Left(x) => refused(x).map(_ => None)
    {
      case Act.Send(m) => bot.send(chat, m.text, m.keyboard).flatMap(told)
      case Act.Edit(id, m) => bot.edit(chat, id, m.text, m.keyboard).flatMap(told).map(_ => None)
      case Act.Answer(cb, notice) => bot.answerCallback(cb, notice).flatMap(told).map(_ => None)
      case Act.Ask(prompt) =>
        asked()
        bot.send(chat, prompt, forceReply = true).flatMap(told).map(_ => None)
    }

  /**
   * `perform`, WITH EDITS TO ONE MESSAGE COALESCED (specs/telegram-live.md).
   *
   * A screen that watches a running agent changes faster than the Bot API
   * lets a message be edited. The first edit of a message goes out at
   * once and opens a window of `everyMs`; edits within the window are
   * held and the LAST one is sent when it closes, which opens the next
   * window; a window that closes with nothing held simply closes, so a
   * quiet card is still instant. An edit equal to the last one sent is
   * dropped (the API refuses "message is not modified" as an error).
   *
   * Sends, answers and asks are never held: a new message is not an
   * edit, and a press must be answered now or the person's client spins.
   * Edits to different messages do not hold each other.
   */
  def performThrottled(bot: Bot, chat: Long, everyMs: Long = 2000,
                       refused: Refused => Unit ! Async = _ => pure(()),
                       asked: () => Unit = () => ())
                      (using T: Timer, S: Scheduler): Act => Option[Long] ! Async =
    val inner = perform(bot, chat, refused, asked)
    val lock = new Object
    var last = Map.empty[Long, Message]              // the newest text+keyboard sent, per message
    var held = Map.empty[Long, Option[Message]]      // an open window, and what it holds

    def send(id: Long, m: Message): Unit ! Async =
      inner(Act.Edit(id, m)).map(_ => ())

    def arm(id: Long): Unit =
      T.after(everyMs)(() => close(id)): Unit

    // the window closes: send what it held and open the next, or just close
    def close(id: Long): Unit =
      val next = lock.synchronized {
        held.get(id).flatten match
          case Some(m) => held += id -> None; last += id -> m; Some(m)
          case None => held -= id; None
      }
      next.foreach(m => Async.spawn(send(id, m).map(_ => arm(id))): Unit)

    {
      case Act.Edit(id, m) =>
        val now = lock.synchronized {
          if last.get(id).contains(m) && !held.get(id).exists(_.exists(_ != m)) then false   // nothing new
          else if held.contains(id) then { held += id -> Some(m); false }                     // held: last wins
          else { held += id -> None; last += id -> m; true }                                  // first: at once
        }
        if now then send(id, m).map { _ => arm(id); None } else pure(None)
      case other => inner(other)
    }

  /** an update as what a chat's host hears, and which chat: a press, a
   * message; a payment or a pre-checkout is the consumer's, not the host's */
  def heard(u: Update): Option[(Long, Telegram.Update)] = u match
    case Update.Callback(_, chat, _, _, data, cb) => Some(chat -> Telegram.Update.Pressed(data, cb))
    case Update.Message(_, chat, _, _, text) => Some(chat -> Telegram.Update.Said(text))
    case _ => None

final class Chats(bot: Bot, open: (Long, Host) => Unit ! Async,
                  refused: Refused => Unit ! Async = _ => pure(()),
                  everyMs: Long = 0)(using Scheduler, Timer):
  private var doors = Map.empty[Long, Telegram.Update => Unit ! Async]
  private var asked = Set.empty[Long]
  private val lock = new Object

  /** the chats with an application open */
  def opened: Set[Long] = lock.synchronized(doors.keySet)

  /**
   * IS THIS CHAT'S SCREEN WAITING FOR A TYPED VALUE? Its `Input` was
   * focused (the pencil pressed), the screen sent its prompt with
   * ForceReply, and the answer has not arrived.
   *
   * A consumer that also understands text itself must ask: the next
   * message is either the value the screen asked for or a sentence of
   * its own, and the two roads are not interchangeable. Without this the
   * choice is made blind — steal the value, or hand the screen a message
   * it has no focus for, which is no event, no act and no reply at all.
   */
  def awaiting(chat: Long): Boolean = lock.synchronized(asked.contains(chat))

  /** the chat's door, opening its application first when this is the
   * chat's first word — the one place a host is made */
  private def doorOf(chat: Long): (Telegram.Update => Unit ! Async) ! Async =
    async {
      lock.synchronized {
        doors.get(chat) match
          case Some(door) => (door, None)
          case None =>
            val tell = () => lock.synchronized { asked += chat }
            val (host, door) = Telegram.host(
              if everyMs > 0 then Chats.performThrottled(bot, chat, everyMs, refused, tell)
              else Chats.perform(bot, chat, refused, tell))
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
    case Some((chat, heard)) =>
      // the value the screen asked for has arrived (or the person pressed
      // instead of typing, which abandons the question either way)
      lock.synchronized { asked -= chat }
      doorOf(chat).flatMap(_(heard))
