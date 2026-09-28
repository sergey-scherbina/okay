package okay.desktop

import java.nio.file.{Files, Path}

/** the window's size and place, remembered in `<state>/window.txt`;
 * a window smaller than the floor comes back as the default */
final case class WindowState(x: Double, y: Double, w: Double, h: Double):
  def write(state: Path): Unit = scala.util.Try(Files.writeString(state.resolve(WindowState.File), s"$x $y $w $h\n")): Unit

object WindowState:
  val File = "window.txt"
  val Default: WindowState = WindowState(-1, -1, 1180, 820)
  def read(state: Path): WindowState =
    scala.util.Try(Files.readString(state.resolve(File)).trim.split(' ').map(_.toDouble)).toOption.collect {
      case Array(x, y, w, h) if w >= 600 && h >= 400 => WindowState(x, y, w, h)
    }.getOrElse(Default)
