package okay.ui

import okay.!

/**
 * The composed dialogs (specs/ui-toolkit.md): the four questions
 * every scenario was about to hand-roll, as Dialog programs over the
 * same event contract Form.ask uses. Nothing here knows a backend.
 *
 * Each is one `!.loop` (specs/fold-until.md, stage 2): the state is
 * what the dialog remembers between showings, `Right` is its answer,
 * `Left` is "show again with this". They were each a local `def loop`
 * with a trailing `loop(z)` before loop-on-bang — the recursion and
 * the seed written by hand four times.
 */
object Toolkit {

  /** ok/cancel as a Boolean */
  def confirm(text: String): Boolean ! Dialog =
    !.loop(()) { _ =>
      Dialog.show(Ui.Column(Vector(Ui.Text(text),
        Ui.Row(Vector(Ui.Button("ok", "$ok"), Ui.Button("cancel", "$cancel")))))).map {
        case Event.Pressed("$ok") => Right(true)
        case Event.Pressed("$cancel") | Event.Closed => Right(false)
        case _ => Left(())
      }
    }

  /** a message, acknowledged */
  def alert(text: String): Unit ! Dialog =
    !.loop(()) { _ =>
      Dialog.show(Ui.Column(Vector(Ui.Text(text), Ui.Button("ok", "$ok")))).map {
        case Event.Pressed("$ok") | Event.Closed => Right(())
        case _ => Left(())
      }
    }

  /** one line of text; Cancel answers None */
  def prompt(text: String): Option[String] ! Dialog =
    !.loop("") { value =>
      Dialog.show(Ui.Column(Vector(Ui.Text(text), Ui.Input(value, key = "$value"),
        Ui.Row(Vector(Ui.Button("ok", "$ok"), Ui.Button("cancel", "$cancel")))))).map {
        case Event.Pressed("$ok") => Right(Some(value))
        case Event.Pressed("$cancel") | Event.Closed => Right(None)
        case Event.Edited("$value", v) => Left(v)
        case _ => Left(value)
      }
    }

  /** one of the options, by index; Cancel answers None */
  def choice(text: String, options: Vector[String]): Option[Int] ! Dialog =
    !.loop(0) { sel =>
      Dialog.show(Ui.Column(Vector(Ui.Text(text), Ui.Select(options, sel, key = "$choice"),
        Ui.Row(Vector(Ui.Button("ok", "$ok"), Ui.Button("cancel", "$cancel")))))).map {
        case Event.Pressed("$ok") => Right(Some(sel))
        case Event.Pressed("$cancel") | Event.Closed => Right(None)
        case Event.Chosen("$choice", i) => Left(i)
        case _ => Left(sel)
      }
    }
}
