package okay.ui

import okay.!

/**
 * The composed dialogs (specs/ui-toolkit.md): the four questions
 * every scenario was about to hand-roll, as Dialog programs over the
 * same event contract Form.ask uses. Nothing here knows a backend.
 */
object Toolkit {

  /** ok/cancel as a Boolean */
  def confirm(text: String): Boolean ! Dialog =
    def loop: Boolean ! Dialog =
      Dialog.show(Ui.Column(Vector(Ui.Text(text),
        Ui.Row(Vector(Ui.Button("ok", "$ok"), Ui.Button("cancel", "$cancel")))))).flatMap {
        case Event.Pressed("$ok") => okay.pure(true)
        case Event.Pressed("$cancel") | Event.Closed => okay.pure(false)
        case _ => loop
      }
    loop

  /** a message, acknowledged */
  def alert(text: String): Unit ! Dialog =
    def loop: Unit ! Dialog =
      Dialog.show(Ui.Column(Vector(Ui.Text(text),
        Ui.Button("ok", "$ok")))).flatMap {
        case Event.Pressed("$ok") | Event.Closed => okay.pure(())
        case _ => loop
      }
    loop

  /** one line of text; Cancel answers None */
  def prompt(text: String): Option[String] ! Dialog =
    def loop(value: String): Option[String] ! Dialog =
      Dialog.show(Ui.Column(Vector(Ui.Text(text), Ui.Input(value, key = "$value"),
        Ui.Row(Vector(Ui.Button("ok", "$ok"), Ui.Button("cancel", "$cancel")))))).flatMap {
        case Event.Pressed("$ok") => okay.pure(Some(value))
        case Event.Pressed("$cancel") | Event.Closed => okay.pure(None)
        case Event.Edited("$value", v) => loop(v)
        case _ => loop(value)
      }
    loop("")

  /** one of the options, by index; Cancel answers None */
  def choice(text: String, options: Vector[String]): Option[Int] ! Dialog =
    def loop(sel: Int): Option[Int] ! Dialog =
      Dialog.show(Ui.Column(Vector(Ui.Text(text), Ui.Select(options, sel, key = "$choice"),
        Ui.Row(Vector(Ui.Button("ok", "$ok"), Ui.Button("cancel", "$cancel")))))).flatMap {
        case Event.Pressed("$ok") => okay.pure(Some(sel))
        case Event.Pressed("$cancel") | Event.Closed => okay.pure(None)
        case Event.Chosen("$choice", i) => loop(i)
        case _ => loop(sel)
      }
    loop(0)
}
