package okay.desktop

import java.nio.file.{Files, Path}

/**
 * THE INSTALLED PROGRAM'S LAUNCH (specs/app-host.md; first okay-watch's
 * specs/native.md): the same service as the server's, on this computer
 * only, with its data where the system keeps an application's; one copy
 * running — a second start brings the first to the front; the app's own
 * window when this Java carries JavaFX and there is a screen, else a
 * small "running" window and the browser. Nothing here names JavaFX:
 * `Window` is asked for by name, so a Java without it never loads it.
 */
object Desktop:
  /** where the system keeps an application's data, by the app's name:
   * `~/Library/Application Support/<name>` on a Mac, `%APPDATA%\<name>`
   * on Windows, `$XDG_DATA_HOME/<name>` (or `~/.local/share/<name>`)
   * elsewhere */
  def dataDir(name: String, os: String = System.getProperty("os.name", ""),
              home: Path = Path.of(System.getProperty("user.home")),
              env: String => Option[String] = k => Option(System.getenv(k))): Path =
    val o = os.toLowerCase
    if o.contains("mac") then home.resolve("Library").resolve("Application Support").resolve(name)
    else if o.contains("win") then env("APPDATA").map(Path.of(_)).getOrElse(home.resolve("AppData").resolve("Roaming")).resolve(name)
    else env("XDG_DATA_HOME").map(Path.of(_)).getOrElse(home.resolve(".local").resolve("share")).resolve(name)

  /** the service answers on this computer: a GET of `path` on `port`
   * whose body carries `marker` */
  def running(port: Int, path: String = "/healthz", marker: String = ""): Boolean =
    scala.util.Try {
      val c = java.net.URI.create(s"http://127.0.0.1:$port$path").toURL.openConnection()
      c.setConnectTimeout(800); c.setReadTimeout(800)
      val in = c.getInputStream
      try new String(in.readAllBytes(), "UTF-8").contains(marker) finally in.close()
    }.getOrElse(false)

  /** THE APP'S OWN WINDOW can open: JavaFX is in this Java, and there is
   * a screen. Asked by name, so a Java without it never loads `Window` */
  def windowed: Boolean =
    !java.awt.GraphicsEnvironment.isHeadless &&
      scala.util.Try(Class.forName("javafx.application.Platform")).isSuccess

  /** the running copy's window to the front: a POST the service answers
   * by calling `Window.focus` (a second double-click) */
  def front(port: Int, path: String = "/ui/app/focus"): Unit =
    scala.util.Try {
      val c = java.net.URI.create(s"http://127.0.0.1:$port$path").toURL.openConnection()
        .asInstanceOf[java.net.HttpURLConnection]
      c.setRequestMethod("POST"); c.setConnectTimeout(800); c.setReadTimeout(800); c.getResponseCode
    }: Unit

  /** in the person's own browser, by the system's own opener */
  def browse(url: String): Unit =
    val os = System.getProperty("os.name", "").toLowerCase
    val cmd =
      if os.contains("mac") then Seq("open", url)
      else if os.contains("win") then Seq("rundll32", "url.dll,FileProtocolHandler", url)
      else Seq("xdg-open", url)
    scala.util.Try(ProcessBuilder(cmd*).start()): Unit

  /**
   * THE LAUNCH: if a copy is running, bring it forward (its window, or
   * the browser at it) and return; else start `serve` on its own thread
   * and, once `running` answers, open the app's window (closing it ends
   * the program) — or, without JavaFX or a screen, the small "running"
   * window and the browser (unless `quiet`, a restart into a new version
   * whose browser tab comes back by itself), and wait for the service.
   *
   * @param window  whether the app's own window is wanted; `Desktop.windowed`
   * @param prepare what the product does once it knows it is the one copy
   *                and before its service opens the data (a staged restore)
   */
  def launch(app: App, port: Int, data: Path, serve: () => Unit, running: () => Boolean,
             window: Boolean = windowed, quiet: Boolean = false, wait: Long = 120_000,
             prepare: () => Unit = () => ()): Unit =
    if running() then
      if window then front(port) else browse(app.base)
      return
    Files.createDirectories(data)
    prepare()
    val service = Thread.ofPlatform().name(app.name).start(() => serve())
    val until = System.currentTimeMillis + wait
    while !running() && service.isAlive && System.currentTimeMillis < until do Thread.sleep(200)
    if window then
      if running() then
        Window.systemAbout(app)
        Window.open(app, data)
      // closing the window quits
      System.exit(0)
    val headless = java.awt.GraphicsEnvironment.isHeadless
    if !headless then status(app, data)
    if running() && !headless && !quiet then browse(app.base)
    service.join()

  /** "<name> is running" — Open, Quit; closing it quits */
  private def status(app: App, data: Path): Unit =
    javax.swing.SwingUtilities.invokeLater { () =>
      import javax.swing.*
      val f = JFrame(app.name)
      val panel = JPanel()
      panel.setLayout(BoxLayout(panel, BoxLayout.Y_AXIS))
      panel.setBorder(BorderFactory.createEmptyBorder(18, 22, 18, 22))
      val title = JLabel(s"${app.name} is running${if app.version.nonEmpty then s" — ${app.version}" else ""}")
      title.setFont(title.getFont.deriveFont(java.awt.Font.BOLD, 15f))
      val where = JLabel(s"${app.base} — your data: $data")
      val buttons = JPanel()
      val openB = JButton(s"Open ${app.name}")
      openB.addActionListener(_ => browse(app.base))
      val quit = JButton("Quit")
      quit.addActionListener(_ => System.exit(0))
      buttons.add(openB); buttons.add(quit)
      Vector[javax.swing.JComponent](title, Box.createVerticalStrut(6).asInstanceOf[JComponent], where,
        Box.createVerticalStrut(12).asInstanceOf[JComponent], buttons).foreach { c =>
        c.setAlignmentX(java.awt.Component.LEFT_ALIGNMENT); panel.add(c)
      }
      f.setContentPane(panel)
      f.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
      f.pack()
      f.setLocationRelativeTo(null)
      f.setVisible(true)
    }
