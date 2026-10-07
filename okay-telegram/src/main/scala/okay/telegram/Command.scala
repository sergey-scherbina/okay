package okay.telegram

import okay.{Async}
import okay.freer.*
import okay.std.*
/**
 * A COMMAND THE CLIENT LISTS IN ITS MENU (specs/telegram-live.md): the
 * same table a program's screens are keyed by, so there is exactly one
 * place a command is named and the menu cannot say `/agents` while the
 * program has no such screen.
 *
 * Still not a framework: `dispatch` answers which screen a `/name`
 * opens and nothing more; what a screen is remains the program's.
 */
final case class Command(name: String, description: String, screen: String)

object Command:
  /** the Bot API's rule for a command name */
  private val Name = "[a-z0-9_]{1,32}".r

  /** `setMyCommands` from the table. A name the API would refuse is
   * refused HERE, by name, before the API refuses it by index — and
   * without a network call. */
  def install(bot: Bot, commands: Vector[Command]): Either[Refused, Unit] ! Async =
    commands.find(c => !Name.matches(c.name)) match
      case Some(bad) =>
        pure(Left(Refused("setMyCommands", 400, s"command '${bad.name}' is not [a-z0-9_]{1,32}")))
      case None => bot.setCommands(commands.map(c => c.name -> c.description))

  /** `/name`, `/name@bot`, `/name args` → the screen it opens; a text
   * that is not a command, or a command not in the table, is `None` */
  def dispatch(commands: Vector[Command], text: String): Option[String] =
    val t = text.trim
    if !t.startsWith("/") then None
    else
      val word = t.drop(1).takeWhile(ch => ch != ' ' && ch != '@')
      commands.find(_.name == word).map(_.screen)
