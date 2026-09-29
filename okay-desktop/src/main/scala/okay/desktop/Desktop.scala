package okay.desktop

import java.nio.file.{Files, Path}

/**
 * THE INSTALLED PROGRAM'S LAUNCH (specs/app-host.md, specs/app-in-process.md;
 * first okay-watch's specs/native.md): the same service as the server's,
 * with its data where the system keeps an application's; one copy
 * running, known by its data folder — a second start brings the first to
 * the front; the app's own window when this Java carries JavaFX and there
 * is a screen, the service reached IN THE PROCESS with no port at all;
 * else (the browser road) a small "running" window and the browser, on a
 * port that is free. Nothing here names JavaFX: `Window` is asked for by
 * name, so a Java without it never loads it.
 */
object Desktop:
  /** how the product's service is to be reached */
  enum Mode:
    /** the app's window: hand the routes over, listen on nothing */
    case InWindow(hand: Routes => Unit)
    /** the browser road: listen on this port, on this computer */
    case OnPort(port: Int)

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

  /** something listens on this port of this computer */
  def listening(port: Int): Boolean =
    scala.util.Try {
      val s = java.net.Socket()
      try s.connect(java.net.InetSocketAddress("127.0.0.1", port), 800) finally s.close()
    }.isSuccess

  /** NOTHING LISTENS ON `port`, on every address nor on the loopback alone —
   * a Docker copy publishes `127.0.0.1:8099`, and a bind with the JVM's
   * default `SO_REUSEADDR` succeeds beside another process's, so both are
   * tried without it. A bind refused by nothing but the last process's
   * closed connections (TIME_WAIT, after a restart into a new version) is
   * not a holder: nothing answers a connect there (okay-watch's
   * `Desktop.free`, 2026-09-29, with its TIME_WAIT fix the same day) */
  def free(port: Int): Boolean =
    val bindable = Vector(java.net.InetSocketAddress(port), java.net.InetSocketAddress(java.net.InetAddress.getLoopbackAddress, port))
      .forall { at =>
        val s = java.net.ServerSocket()
        try { s.setReuseAddress(false); s.bind(at, 1); true }
        catch case _: java.io.IOException => false
        finally s.close()
      }
    bindable || !listening(port)

  /** the preferred port when nothing holds it, else one the system gives */
  def freePort(preferred: Int): Int =
    if free(preferred) then preferred
    else scala.util.Try { val s = java.net.ServerSocket(0); try s.getLocalPort finally s.close() }.getOrElse(preferred)

  /** THE APP'S OWN WINDOW can open: JavaFX is in this Java, and there is
   * a screen. Asked by name, so a Java without it never loads `Window` */
  def windowed: Boolean =
    !java.awt.GraphicsEnvironment.isHeadless &&
      scala.util.Try(Class.forName("javafx.application.Platform")).isSuccess

  /** in the person's own browser, by the system's own opener */
  def browse(url: String): Unit =
    val os = System.getProperty("os.name", "").toLowerCase
    val cmd =
      if os.contains("mac") then Seq("open", url)
      else if os.contains("win") then Seq("rundll32", "url.dll,FileProtocolHandler", url)
      else Seq("xdg-open", url)
    val _ = scala.util.Try(ProcessBuilder(cmd*).start())

  /** where the browser road's page is, for a second start: `<data>/url.txt` */
  val UrlFile = "url.txt"

  /**
   * THE LAUNCH. One copy per data folder (`Instance`): a second start
   * says `front` to the first and ends. The first runs `prepare` (what the
   * product does before its service opens the data) and then:
   *
   * - with the window: `serve(Mode.InWindow(hand))` on its own thread;
   *   the routes it hands over are served in the process (`InProcess`),
   *   the pages are `app://<host>/…`, and closing the window ends the
   *   program — no port was ever opened;
   * - without (no JavaFX, no screen): `serve(Mode.OnPort(port))` on
   *   `port` when it is free, else on one that is, written to
   *   `<data>/url.txt`; a small "running" window and the browser (unless
   *   `quiet`, a restart whose browser tab comes back by itself).
   */
  def launch(app: App, data: Path, serve: Mode => Unit, port: Int = 8099,
             window: Boolean = windowed, quiet: Boolean = false, wait: Long = 120_000,
             prepare: () => Unit = () => (),
             /** a start that failed, said: the product's dialog and log */
             failed: String => Unit = why => System.err.println(s"could not start: $why")): Unit =
    Files.createDirectories(data)
    Instance.claim(data) match
      case None =>
        // ANOTHER COPY RUNS for this data folder: it comes to the front
        if !Instance.front(data) && !window then
          scala.util.Try(Files.readString(data.resolve(UrlFile)).trim).toOption.filter(_.nonEmpty).foreach(browse)
      case Some(one) =>
        prepare()
        if window then inWindow(app, data, serve, one, wait, failed) else onPort(app, data, serve, one, port, quiet, wait, failed)

  private def inWindow(app: App, data: Path, serve: Mode => Unit, one: Instance, wait: Long, failed: String => Unit): Unit =
    val handed = java.util.concurrent.CompletableFuture[Routes]()
    Thread.ofPlatform().name(app.name).start(() =>
      try
        serve(Mode.InWindow(r => { val _ = handed.complete(r) }))
        // it ended without handing its routes over: that start failed
        val _ = handed.completeExceptionally(IllegalStateException("the service ended before it was ready"))
      catch case e: Throwable => { val _ = handed.completeExceptionally(e) }): Unit
    val routes = scala.util.Try(handed.get(wait, java.util.concurrent.TimeUnit.MILLISECONDS))
    routes match
      case scala.util.Success(r) =>
        val server = InProcess.Server(InProcess.host(app.name), r)
        InProcess.install(server)
        val here = app.copy(base = server.base)
        val _ = one.listen(() => Window.focus())
        Window.systemAbout(here)
        Window.open(here, data, Transport.inProcess(server))
      case scala.util.Failure(e) =>
        val why = e match
          case _: java.util.concurrent.TimeoutException => s"the service was not ready within ${wait / 1000} seconds."
          case x: java.util.concurrent.ExecutionException => Option(x.getCause).fold(x.toString)(c => Option(c.getMessage).filter(_.nonEmpty).getOrElse(c.toString))
          case x => x.toString
        one.release()
        failed(why)
    one.release()
    // closing the window quits
    System.exit(0)

  private def onPort(app: App, data: Path, serve: Mode => Unit, one: Instance, preferred: Int, quiet: Boolean, wait: Long,
                     failed: String => Unit): Unit =
    val port = freePort(preferred)
    val url = s"http://127.0.0.1:$port"
    Files.writeString(data.resolve(UrlFile), url + "\n"): Unit
    val here = app.copy(base = url)
    val _ = one.listen(() => browse(url))
    val service = Thread.ofPlatform().name(app.name).start(() =>
      try serve(Mode.OnPort(port)) catch case e: Throwable => failed(Option(e.getMessage).filter(_.nonEmpty).getOrElse(e.toString)))
    val until = System.currentTimeMillis + wait
    while !listening(port) && service.isAlive && System.currentTimeMillis < until do Thread.sleep(200)
    if !listening(port) then failed(s"the service did not answer on port $port.")
    val headless = java.awt.GraphicsEnvironment.isHeadless
    if !headless then status(here, data)
    if listening(port) && !headless && !quiet then browse(url)
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
      buttons.add(openB): Unit
      buttons.add(quit): Unit
      Vector[javax.swing.JComponent](title, Box.createVerticalStrut(6).asInstanceOf[JComponent], where,
        Box.createVerticalStrut(12).asInstanceOf[JComponent], buttons).foreach { c =>
        c.setAlignmentX(java.awt.Component.LEFT_ALIGNMENT); panel.add(c): Unit
      }
      f.setContentPane(panel)
      f.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
      f.pack()
      f.setLocationRelativeTo(null)
      f.setVisible(true)
    }
