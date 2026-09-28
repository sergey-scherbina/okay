package okay.desktop

import java.nio.file.{Files, Path}
import javafx.application.Platform
import javafx.concurrent.Worker
import javafx.scene.Scene
import javafx.scene.control.{Alert, ButtonType, Menu, MenuBar, MenuItem, SeparatorMenuItem}
import javafx.scene.input.KeyCombination
import javafx.scene.layout.BorderPane
import javafx.scene.web.WebView
import javafx.stage.{FileChooser, Stage}

/**
 * THE APP'S OWN WINDOW (specs/app-host.md; first okay-watch's
 * specs/app-window.md): the pages inside the system's web engine, no
 * address bar and no tabs; the menus; a Save dialog for every download;
 * the system browser for every outside link; its size and place
 * remembered. Loaded only by an installed app whose Java carries JavaFX
 * — a server never touches this class, which is why it is asked for by
 * name (`Desktop.windowed`) and why nothing else in this module
 * mentions JavaFX.
 *
 * What is the PRODUCT's is in its `App`: the name, the icon, the
 * menus, the picks, the About words. What is here is the same for
 * every product.
 */
object Window:
  @volatile private var stage: Option[Stage] = None
  /** kept strongly: the page's bridge is held weakly by the engine */
  @volatile private var bridge: Bridge = null

  /** a second double-click: this window to the front */
  def focus(): Unit = stage.foreach(s => Platform.runLater(() => { s.setIconified(false); s.show(); s.toFront() }))

  /** the window, at the app's `base`, until it is closed; `state` is
   * where its size and place are kept */
  def open(app: App, state: Path): Unit =
    val closed = java.util.concurrent.CountDownLatch(1)
    Platform.setImplicitExit(true)
    Platform.startup { () =>
      val s = Stage()
      val view = WebView()
      // the sharpest text the engine draws (LCD where the screen allows it)
      view.setFontSmoothingType(javafx.scene.text.FontSmoothingType.LCD)
      val engine = view.getEngine
      // THE ENGINE'S OWN COOKIES (it installs them as the default on its
      // first view): the window's own requests share the session
      val cookies = Option(java.net.CookieHandler.getDefault).getOrElse {
        val c = java.net.CookieManager(null, java.net.CookiePolicy.ACCEPT_ALL); java.net.CookieHandler.setDefault(c); c }
      val http = java.net.http.HttpClient.newBuilder().cookieHandler(cookies)
        .followRedirects(java.net.http.HttpClient.Redirect.NORMAL).build()
      engine.setUserAgent(s"${engine.getUserAgent} ${app.name}-app")
      val here = WindowState.read(state)
      s.setTitle(app.name)
      scala.util.Try(app.icon().foreach(in => try s.getIcons.add(javafx.scene.image.Image(in)) finally in.close()))
      bridge = Bridge(app, s, http)
      bridge.engine = Some(engine)

      // OUTSIDE LINKS go to the system browser; the page stays
      engine.locationProperty.addListener { (_, before, now) =>
        if now != null && !now.startsWith(app.base) && !now.startsWith("about:") && !now.startsWith("data:") then
          Platform.runLater { () =>
            engine.getLoadWorker.cancel()
            external(now)
            if before != null && before.startsWith(app.base) then engine.load(before)
          }
      }
      engine.setCreatePopupHandler { _ =>
        val popup = javafx.scene.web.WebEngine()
        popup.locationProperty.addListener((_, _, u) => if u != null && u.nonEmpty then external(u))
        popup
      }
      engine.titleProperty.addListener((_, _, t) => s.setTitle(Option(t).filter(_.nonEmpty).fold(app.name)(x =>
        if x == app.name then x else s"$x — ${app.name}")))
      // SAVING IS A SAVE DIALOG: every `a[download]`, and what the app names
      engine.getLoadWorker.stateProperty.addListener { (_, _, st) =>
        if st == Worker.State.SUCCEEDED then
          scala.util.Try {
            engine.executeScript("window").asInstanceOf[netscape.javascript.JSObject].setMember("okayApp", bridge)
            engine.executeScript(App.script(app))
          }
          Tour.next(app, s, engine)
        else if st == Worker.State.FAILED then
          System.err.println(s"${app.name}: the page did not load: ${engine.getLocation} ${Option(engine.getLoadWorker.getException).getOrElse("")}")
      }

      val root = BorderPane(view)
      val bar = menus(app, s, view, http, state)
      bar.setUseSystemMenuBar(true)
      root.setTop(bar)
      s.setScene(Scene(root, here.w, here.h))
      if here.x >= 0 then { s.setX(here.x); s.setY(here.y) }
      s.setOnCloseRequest { e => if !mayClose(app, s, http, state) then e.consume() }
      s.setOnHidden(_ => closed.countDown())
      // ⌘Q from the system's app menu ends JavaFX without a close request
      Thread.ofPlatform().daemon(true).start { () =>
        while closed.getCount > 0 do
          Thread.sleep(1000)
          if !s.isShowing then closed.countDown()
      }
      engine.load(app.base + app.start)
      stage = Some(s)
      s.show()
    }
    closed.await()

  /** a page the window does not show, loaded apart and printed whole */
  private def printPage(owner: Stage, url: String): Unit =
    val off = javafx.scene.web.WebEngine()
    off.getLoadWorker.stateProperty.addListener { (_, _, st) =>
      if st == Worker.State.SUCCEEDED then
        val job = javafx.print.PrinterJob.createPrinterJob()
        if job != null && job.showPrintDialog(owner) then { off.print(job); job.endJob() }
      else if st == Worker.State.FAILED then
        note(owner, "It could not be printed", "The page to print did not open.")
    }
    off.load(url)

  /** closing quits — after the app's question when it says it is busy */
  private def mayClose(app: App, s: Stage, http: java.net.http.HttpClient, state: Path): Boolean =
    val go = !busy(app, http) || ask(app.quit._1, app.quit._2)
    if go then WindowState(s.getX, s.getY, s.getWidth, s.getHeight).write(state)
    go

  /** THE MENUS: the product's, as data, between the window's own —
   * File gets Print… and Close Window at its end, Edit and View are the
   * window's unless the product gave its own, Help ends with About */
  private def menus(app: App, s: Stage, view: WebView, http: java.net.http.HttpClient, state: Path): MenuBar =
    val engine = view.getEngine
    def item(label: String, keys: String, act: => Unit): MenuItem =
      val m = MenuItem(label)
      if keys.nonEmpty then m.setAccelerator(KeyCombination.keyCombination(keys))
      m.setOnAction(_ => act)
      m
    def go(path: String) = engine.load(app.base + path)
    def js(code: String) = scala.util.Try(engine.executeScript(code)): Unit
    def location = Option(engine.getLocation).getOrElse("")
    def perform(a: App.Act): Unit = a match
      case App.Act.Go(path) => go(path)
      case App.Act.External(url) => external(url)
      case App.Act.Js(code) => js(code)
      case App.Act.Save(url, orSay) => url(location) match
        case Some(u) => bridge.save(u)
        case None => note(s, orSay._1, orSay._2)
      case App.Act.Post(path, next) =>
        scala.util.Try(http.send(java.net.http.HttpRequest.newBuilder(java.net.URI.create(app.base + path))
          .POST(java.net.http.HttpRequest.BodyPublishers.noBody()).build(), java.net.http.HttpResponse.BodyHandlers.discarding()))
        go(next)
      case App.Act.Pick(p) => bridge.pick(p)
      case App.Act.Run(run) => run()
    def entries(es: Vector[App.Entry]): Vector[MenuItem] = es.map {
      case App.Item(label, act, keys) => item(label, keys, perform(act))
      case App.Separator => SeparatorMenuItem()
    }
    def menu(title: String, es: Vector[MenuItem]): Menu =
      val m = Menu(title)
      m.getItems.addAll(es*)
      m
    val own = app.menus.map(m => m.title -> m.entries).toMap
    // PRINT is what the app names for the shown page (a report rather
    // than the page around it), else the page itself
    def print(): Unit = app.printing(location) match
      case Some(url) => printPage(s, url)
      case None =>
        val job = javafx.print.PrinterJob.createPrinterJob()
        if job != null && job.showPrintDialog(s) then { engine.print(job); job.endJob() }
    val file = menu("File", entries(own.getOrElse("File", Vector.empty)) ++
      (if own.contains("File") then Vector(SeparatorMenuItem()) else Vector.empty) ++ Vector(
      item("Print…", "Shortcut+P", print()),
      SeparatorMenuItem(),
      item("Close Window", "Shortcut+W", if mayClose(app, s, http, state) then s.close())))
    val edit = own.get("Edit").map(es => menu("Edit", entries(es))).getOrElse(menu("Edit", Vector(
      item("Cut", "Shortcut+X", js("document.execCommand('cut')")),
      item("Copy", "Shortcut+C", js("document.execCommand('copy')")),
      item("Paste", "Shortcut+V", paste(engine)),
      item("Select All", "Shortcut+A", js("document.execCommand('selectAll')")))))
    val viewM = own.get("View").map(es => menu("View", entries(es))).getOrElse(menu("View", Vector(
      item("Back", "Shortcut+OPEN_BRACKET", js("history.back()")),
      item("Forward", "Shortcut+CLOSE_BRACKET", js("history.forward()")),
      item("Reload", "Shortcut+R", engine.reload()),
      SeparatorMenuItem(),
      item("Zoom In", "Shortcut+EQUALS", view.setZoom(math.min(2.0, view.getZoom + 0.1))),
      item("Zoom Out", "Shortcut+MINUS", view.setZoom(math.max(0.5, view.getZoom - 0.1))),
      item("Actual Size", "Shortcut+DIGIT0", view.setZoom(1.0)))))
    val others = app.menus.filterNot(m => Set("File", "Edit", "View", "Help")(m.title)).map(m => menu(m.title, entries(m.entries)))
    val help = menu("Help", entries(own.getOrElse("Help", Vector.empty)) ++
      (if own.contains("Help") then Vector(SeparatorMenuItem()) else Vector.empty) ++
      Vector(item(s"About ${app.name}", "", about(app, s))))
    MenuBar((Vector(file, edit, viewM) ++ others :+ help)*)

  /**
   * THE TRADITIONAL ABOUT WINDOW: the icon, the name, the version, what
   * it is for, the copyright — and at the foot, the library it is built
   * with, with a link that opens in the person's own browser. Modal to
   * the app's window; OK or Escape closes it.
   */
  def about(app: App, owner: Stage): Unit =
    import javafx.geometry.{Insets, Pos}
    import javafx.scene.control.{Button, Hyperlink, Label, Separator}
    import javafx.scene.image.{Image, ImageView}
    import javafx.scene.layout.VBox
    val w = Stage()
    w.initOwner(owner)
    w.initModality(javafx.stage.Modality.WINDOW_MODAL)
    w.setTitle(s"About ${app.name}")
    w.setResizable(false)
    val icon = app.icon().map { in => val v = ImageView(Image(in, 96, 96, true, true)); in.close(); v }
    val name = Label(app.name)
    name.setStyle("-fx-font-size: 22px; -fx-font-weight: bold;")
    val version = Label(app.version)
    version.setStyle("-fx-text-fill: #555;")
    val what = Label(app.about.what)
    what.setWrapText(true)
    what.setMaxWidth(340)
    what.setStyle("-fx-text-alignment: center;")
    val copyright = Label(app.about.copyright)
    copyright.setStyle("-fx-text-fill: #555; -fx-font-size: 11px;")
    val ok = Button("OK")
    ok.setDefaultButton(true)
    ok.setCancelButton(true)
    ok.setOnAction(_ => w.close())
    val box = VBox(8)
    box.setAlignment(Pos.CENTER)
    box.setPadding(Insets(24, 28, 18, 28))
    icon.foreach(box.getChildren.add(_))
    box.getChildren.addAll(name, version, what, copyright)
    app.about.library.foreach { (line, url) =>
      val built = Label(line)
      built.setStyle("-fx-font-size: 12px;")
      val link = Hyperlink(url)
      link.setOnAction(_ => { external(url); link.setVisited(false) })
      box.getChildren.addAll(Separator(), built, link)
    }
    box.getChildren.add(ok)
    VBox.setMargin(ok, Insets(10, 0, 0, 0))
    w.setScene(Scene(box))
    w.showAndWait()

  /**
   * ABOUT IN THE SYSTEM'S APPLICATION MENU (<name> → About), where a Mac
   * person looks for it — through AWT's desktop integration, which
   * JavaFX lacks. Called BEFORE the window opens: AWT set up after JavaFX
   * answers "supported" and adds nothing, since JavaFX already owns the
   * application menu; set up first, the menu is AWT's and has About.
   * Where it is not supported (Windows, Linux) nothing happens and
   * Help → About is the road.
   */
  def systemAbout(app: App): Unit =
    scala.util.Try {
      if java.awt.Desktop.isDesktopSupported then
        val d = java.awt.Desktop.getDesktop
        if d.isSupported(java.awt.Desktop.Action.APP_ABOUT) then
          d.setAboutHandler(_ => Platform.runLater(() => stage.foreach(about(app, _))))
    }: Unit

  /** the system clipboard's text, typed where the cursor is */
  private def paste(engine: javafx.scene.web.WebEngine): Unit =
    Option(javafx.scene.input.Clipboard.getSystemClipboard.getString).foreach { t =>
      val q = t.replace("\\", "\\\\").replace("'", "\\'").replace("\n", "\\n").replace("\r", "")
      scala.util.Try(engine.executeScript(s"document.execCommand('insertText', false, '$q')"))
    }

  /** in the person's own browser */
  def external(url: String): Unit = Desktop.browse(url)

  private def busy(app: App, http: java.net.http.HttpClient): Boolean = app.busy.exists { path =>
    scala.util.Try(http.send(java.net.http.HttpRequest.newBuilder(java.net.URI.create(app.base + path)).build(),
      java.net.http.HttpResponse.BodyHandlers.ofString()).body.trim).toOption.exists(b => b.nonEmpty && b != "0")
  }

  private def ask(title: String, text: String): Boolean =
    val a = Alert(Alert.AlertType.CONFIRMATION, text, ButtonType.CANCEL, ButtonType.OK)
    a.setHeaderText(title)
    a.showAndWait().filter(_ == ButtonType.OK).isPresent

  private def note(s: Stage, title: String, text: String): Unit =
    val a = Alert(Alert.AlertType.INFORMATION, text, ButtonType.OK)
    a.initOwner(s)
    a.setHeaderText(title)
    a.showAndWait(): Unit

  /** what the page may ask of the window (`window.okayApp`, `App.script`):
   * to save a file, to open one */
  final class Bridge(app: App, s: Stage, http: java.net.http.HttpClient):
    /** the page's engine, to show what the service answered */
    @volatile var engine: Option[javafx.scene.web.WebEngine] = None

    /** a GET, saved where the person says */
    def save(url: String): Unit =
      fetch(java.net.http.HttpRequest.newBuilder(java.net.URI.create(url)).build(), url)

    /** a form's POST (a table as CSV), saved where the person says */
    def savePost(url: String, form: String): Unit =
      fetch(java.net.http.HttpRequest.newBuilder(java.net.URI.create(url))
        .header("content-type", "application/x-www-form-urlencoded")
        .POST(java.net.http.HttpRequest.BodyPublishers.ofString(form)).build(), url)

    /** the i-th pick's dialog (the page's script names it by index) */
    def pick(i: Int): Unit = app.picks.lift(i).foreach(pick)

    /** THE OPEN DIALOG, the file to this computer's service, and the page
     * it answers with (a redirect under `base`) shown */
    def pick(p: App.Pick): Unit =
      val chooser = FileChooser()
      chooser.setTitle(p.title)
      chooser.getExtensionFilters.add(FileChooser.ExtensionFilter(p.filter._1, p.filter._2))
      Option(chooser.showOpenDialog(s)).foreach { f =>
        Thread.ofVirtual().start { () =>
          val sent = scala.util.Try(http.send(java.net.http.HttpRequest.newBuilder(java.net.URI.create(app.base + p.post))
            .header("content-type", p.media)
            .POST(java.net.http.HttpRequest.BodyPublishers.ofFile(f.toPath)).build(),
            java.net.http.HttpResponse.BodyHandlers.discarding()))
          Platform.runLater { () =>
            sent.toOption.map(_.uri.toString).filter(_.startsWith(app.base)) match
              case Some(to) => engine.foreach(_.load(to))
              case None => note(s, p.failed._1, p.failed._2)
          }
        }: Unit
      }

    private def fetch(req: java.net.http.HttpRequest, url: String): Unit =
      Thread.ofVirtual().start { () =>
        val got = scala.util.Try(http.send(req, java.net.http.HttpResponse.BodyHandlers.ofByteArray()))
        Platform.runLater { () =>
          got.toOption.filter(_.statusCode == 200) match
            case None => note(s, "It could not be saved", s"${app.name} did not give the file: $url")
            case Some(res) =>
              val name = res.headers.firstValue("content-disposition").orElse("")
                .split("filename=").drop(1).headOption.map(_.trim.stripPrefix("\"").takeWhile(_ != '"'))
                .filter(_.nonEmpty).getOrElse(url.takeWhile(_ != '?').split('/').last)
              val chooser = FileChooser()
              chooser.setInitialFileName(name)
              val downloads = Path.of(System.getProperty("user.home"), "Downloads").toFile
              if downloads.isDirectory then chooser.setInitialDirectory(downloads)
              Option(chooser.showSaveDialog(s)).foreach { f =>
                scala.util.Try(Files.write(f.toPath, res.body)) match
                  case scala.util.Success(_) => ()
                  case scala.util.Failure(e) => note(s, "It could not be saved", e.getMessage)
              }
        }
      }: Unit

  /**
   * A TOUR, for the one who builds it (`-D<app.tour>=<dir>:<step>,…`):
   * after each page, a picture of the window into `<dir>`, then the next
   * step — a path, `submit` (the page's first form), `pick:<path>` (the
   * first dropdown opened and pictured, then the path), or `quit` (a
   * build's training run for an AOT cache ends here). Nothing without
   * the property.
   */
  private object Tour:
    @volatile private var i = 0
    def next(app: App, s: Stage, engine: javafx.scene.web.WebEngine): Unit =
      sys.props.get(app.tour).map(_.split(":", 2)).collect { case Array(d, st) => (Path.of(d), st.split(',').toVector) }.foreach { (dir, steps) =>
        val n = i
        i += 1
        val pause = javafx.animation.PauseTransition(javafx.util.Duration.millis(900))
        pause.setOnFinished { _ =>
          shot(s, dir.resolve(f"$n%02d.png"))
          steps.lift(n) match
            case Some("submit") => scala.util.Try(engine.executeScript("document.forms[0].submit()"))
            case Some("quit") =>
              javafx.application.Platform.exit()
              System.exit(0)
            case Some(p) if p.startsWith("pick:") =>
              scala.util.Try(engine.executeScript("document.querySelector('select').dispatchEvent(" +
                "new MouseEvent('mousedown',{bubbles:true,cancelable:true}))"))
              val after = javafx.animation.PauseTransition(javafx.util.Duration.millis(500))
              after.setOnFinished { _ =>
                shot(s, dir.resolve(f"$n%02d-open.png"))
                engine.load(app.base + p.stripPrefix("pick:"))
              }
              after.play()
            case Some(p) => engine.load(app.base + p)
            case None => ()
        }
        pause.play()
      }

    private def shot(s: Stage, to: Path): Unit =
      val img = s.getScene.snapshot(null)
      val (w, h) = (img.getWidth.toInt, img.getHeight.toInt)
      val out = java.awt.image.BufferedImage(w, h, java.awt.image.BufferedImage.TYPE_INT_ARGB)
      val px = img.getPixelReader
      for y <- 0 until h; x <- 0 until w do out.setRGB(x, y, px.getArgb(x, y))
      Files.createDirectories(to.getParent)
      javax.imageio.ImageIO.write(out, "png", to.toFile): Unit
