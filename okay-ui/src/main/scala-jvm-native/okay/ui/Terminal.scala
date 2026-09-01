package okay.ui

import okay.*
import okay.given

/**
 * The terminal as a Host, and it is thin because everything that can
 * be a value already is (Frame): this file owns the tty — stty raw
 * mode, painting frames, reading bytes — and the focus, which is the
 * host's state exactly like a cursor is the terminal's.
 *
 * POSIX only in v1 (raw mode is `stty`); the same file serves the JVM
 * and Scala Native, which both have ProcessBuilder.
 */
object Terminal {

  def host(): Host = new Host:
    @volatile private var tree: Ui = Ui.Text("")
    @volatile private var focus = 0
    private val out = System.out

    private def paint(): Unit =
      val f = Ui.focusable(tree).lift(focus)
      val lines = Frame.render(tree, f)
      out.print("\u001b[2J\u001b[H")          // clear, home
      lines.foreach(l => out.print(l + "\r\n"))
      out.flush()

    def render(ui: Ui): Unit ! Async = async {
      tree = ui
      paint()
    }

    def events: Source[Event] =
      def go: Source[Event] =
        effect[Writer % Event + Async, Int](Async.Run(() => System.in.read()))
          .flatMap { b =>
            if b < 0 then pure(())                   // stdin ended
            else if b == 3 || b == 17 then           // Ctrl-C, Ctrl-Q
              effect[Writer % Event + Async, Unit](Writer(Event.Closed))
            else
              val (nf, ev) = Frame.interpret(tree, focus, b.toChar)
              val moved = nf != focus
              focus = nf
              val emit: Unit ! (Writer % Event + Async) = ev match
                case Some(e) => effect[Writer % Event + Async, Unit](Writer(e))
                case None =>
                  // a focus move re-renders the SAME tree, which the
                  // loop would skip — so the host repaints itself
                  if moved then effect[Writer % Event + Async, Unit](Async.Run(() => paint()))
                  else pure(())
              emit.flatMap(_ => go)
          }
      go

  /** raw mode on, run, raw mode off — a bracket, like any resource */
  def raw[A](body: => A): A =
    def stty(args: String*): Unit =
      val _ = ProcessBuilder(("stty" +: args)*)
        .redirectInput(ProcessBuilder.Redirect.INHERIT).start().waitFor()
    stty("raw", "-echo")
    try body finally stty("sane")
}
