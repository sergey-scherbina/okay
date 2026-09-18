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

  /**
   * THE TERMINAL'S SIZE, asked of the terminal (ui-terminal-width).
   * `stty size` prints "rows cols"; a terminal that will not say
   * answers None and the host renders unbudgeted, which is v1's
   * layout. Measured ONCE, when the host is made: re-measuring per
   * paint is a process spawn per frame, and SIGWINCH is not something
   * this file can hear without a signal handler — so a resize needs a
   * new host today, and that is stated rather than pretended.
   */
  def size(): Option[(Int, Int)] =
    try
      val p = ProcessBuilder("stty", "size").redirectInput(ProcessBuilder.Redirect.INHERIT).start()
      val said = String(p.getInputStream.readAllBytes, "UTF-8").trim
      if p.waitFor() != 0 then None
      else said.split("\\s+") match
        case Array(r, c) => (r.toIntOption, c.toIntOption) match
          case (Some(rows), Some(cols)) if rows > 0 && cols > 0 => Some((cols, rows))
          case _ => None
        case _ => None
    catch case _: Exception => None

  def host(): Host = new Host:
    @volatile private var tree: Ui = Ui.Text("")
    @volatile private var focus = 0
    private val out = System.out
    /** the width the frames are laid out in — 0 is "unbudgeted", which
     * is what a terminal that would not say its size gets */
    private val measured: Option[(Int, Int)] = size()
    private val cols = measured.map(_._1).getOrElse(0)
    /** the screen's height, and the first line of it that is shown:
     * a frame taller than the screen is CLIPPED, and the view follows
     * the focus rather than losing it off the bottom
     * (ui-terminal-scroll) */
    private val rows = measured.map(_._2).getOrElse(0)
    @volatile private var top = 0

    private def paint(): Unit =
      val f = Ui.focusable(tree).lift(focus)
      val lines = Frame.render(tree, f, cols)
      Frame.focusLine(tree, f, cols).foreach(l => top = Frame.follow(top, l, rows))
      out.print("\u001b[2J\u001b[H")          // clear, home
      Frame.clip(lines, top, rows).foreach(l => out.print(l + "\r\n"))
      out.flush()

    /** a page of the view, bounded by what there is to show */
    private def page(d: Int): Unit =
      val height = Frame.render(tree, Ui.focusable(tree).lift(focus), cols).length
      val step = math.max(rows - 1, 1)
      top = math.max(0, math.min(top + d * step, math.max(height - rows, 0)))
      paint()

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
      // the size is the FIRST thing the application hears, so a view
      // that wants to lay itself out differently on a narrow terminal
      // can — `Resized` had been an event no host ever sent
      def first: Source[Event] = measured match
        case Some((w, h)) => effect[Writer % Event + Async, Unit](Writer(Event.Resized(w, h)))
        case None => pure(())

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
                  // the view keys are the HOST's: they move no focus
                  // and say nothing to the application
                  if key == Frame.Key.PageUp || key == Frame.Key.PageDown then
                    effect[Writer % Event + Async, Unit](Async.Run(() =>
                      page(if key == Frame.Key.PageUp then -1 else 1)))
                  else
                    val (nf, ev) = Frame.interpret(tree, focus, key)
                    val moved = nf != focus
                    focus = nf
                    ev match
                      case Some(e) => effect[Writer % Event + Async, Unit](Writer(e))
                      case None =>
                        // a focus move re-renders the SAME tree, which
                        // the loop would skip — so the host repaints
                        // itself, and the view follows the focus
                        if moved then effect[Writer % Event + Async, Unit](Async.Run(() => paint()))
                        else pure(())
                }
              }
              emit.flatMap(_ => go)
          }
      first.flatMap(_ => go)

  /** raw mode on, run, raw mode off — a bracket, like any resource */
  def raw[A](body: => A): A =
    def stty(args: String*): Unit =
      val _ = ProcessBuilder(("stty" +: args)*)
        .redirectInput(ProcessBuilder.Redirect.INHERIT).start().waitFor()
    stty("raw", "-echo")
    try body finally stty("sane")
}
