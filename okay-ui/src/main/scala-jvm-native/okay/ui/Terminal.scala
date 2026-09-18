package okay.ui

import okay.*

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

    // an arrow arrives as `ESC [ A`, one byte per read, so the bytes
    // are decoded into KEYS before the tree is asked about them
    // (ui-terminal-keys). The decoder is pure and lives in `Frame`;
    // what is here is the one thing that cannot be a value: the state
    // BETWEEN two reads.
    @volatile private var keyState: Frame.KeyState = Frame.KeyState.Plain

    def events: Source[Event] =
      def go: Source[Event] =
        effect[Writer % Event + Async, Int](Async.Run(() => System.in.read()))
          .flatMap { b =>
            if b < 0 then pure(())                   // stdin ended
            else if b == 3 || b == 17 then           // Ctrl-C, Ctrl-Q
              effect[Writer % Event + Async, Unit](Writer(Event.Closed))
            else
              val (st, keys) = Frame.feed(keyState, b)
              keyState = st
              // one byte can complete no key (mid-sequence) or two (a
              // lone ESC and the byte after it), so this folds
              val emit = keys.foldLeft(pure(()): Unit ! (Writer % Event + Async)) { (acc, key) =>
                acc.flatMap { _ =>
                  val (nf, ev) = Frame.interpret(tree, focus, key)
                  val moved = nf != focus
                  focus = nf
                  ev match
                    case Some(e) => effect[Writer % Event + Async, Unit](Writer(e))
                    case None =>
                      // a focus move re-renders the SAME tree, which the
                      // loop would skip — so the host repaints itself
                      if moved then effect[Writer % Event + Async, Unit](Async.Run(() => paint()))
                      else pure(())
                }
              }
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
