package okay.desktop

import java.io.InputStream

/**
 * WHAT A PRODUCT SAYS ABOUT ITSELF, and nothing else (specs/app-host.md):
 * the window (`Window`) and the launch (`Desktop`) are the platform's,
 * this is the product's part — its name, its icon, where its own
 * service answers on this computer and the first page, its version as
 * a person reads it, the words of the About box, its own menus, the
 * links on its pages that open a file dialog, which downloads go
 * through a Save dialog, and whether quitting should ask.
 *
 * @param name     the window's title, the user agent's suffix, About
 * @param base     this computer's service, `http://127.0.0.1:8099`
 * @param start    the first page, `/ui/trace`
 * @param version  `Version 1.0.412 (abc1234)` — a line under the name
 * @param icon     the icon, as a stream when there is one (a resource)
 * @param about    what it is, whose it is, its site
 * @param menus    the product's menus, as data: the window draws them
 *                 between its own File items and Edit / View / Help
 * @param picks    links on a page the window answers with an Open dialog
 * @param saves    a JavaScript pattern over a link's path: matching links
 *                 are saved through a dialog, as every `a[download]` is
 * @param printing the page to print APART from the one shown, from the
 *                 shown page's location — a report rather than the page
 *                 around it; `None` prints the page
 * @param busy     a path that answers `0` when nothing is running; any
 *                 other answer makes closing ask `quit` first
 * @param quit     the question, and its second line
 * @param tour     the system property naming a tour (`Window`'s Tour)
 */
final case class App(name: String, base: String, start: String, version: String = "",
                     icon: () => Option[InputStream] = () => None,
                     about: App.About = App.About(),
                     menus: Vector[App.Menu] = Vector.empty,
                     picks: Vector[App.Pick] = Vector.empty,
                     saves: String = "\\.(csv|json|txt|zip)$",
                     printing: String => Option[String] = _ => None,
                     busy: Option[String] = None,
                     quit: (String, String) = ("Something is still running — quit anyway?", "It stops now."),
                     tour: String = "okay.desktop.tour")

object App:
  /** the About box's words: what it is (one line), the copyright, the
   * product's site, and the library it is built with (a line and a link) */
  final case class About(what: String = "", copyright: String = "", site: String = "",
                         library: Option[(String, String)] = Some(Library))

  val Library: (String, String) = ("Developed with the Okay! library", "https://github.com/sergey-scherbina/okay")

  /** what a menu item does */
  enum Act:
    /** a page of the service */
    case Go(path: String)
    /** in the person's own browser */
    case External(url: String)
    /** a script in the page */
    case Js(code: String)
    /** a download saved through a dialog: the url from the shown page's
     * location, or a note saying why not (its title and its line) */
    case Save(url: String => Option[String], orSay: (String, String))
    /** a POST to the service, then a page */
    case Post(path: String, next: String)
    /** the Open dialog and the file posted, as a page's link would */
    case Pick(pick: App.Pick)
    /** anything else */
    case Run(run: () => Unit)

  sealed trait Entry
  /** `keys` as JavaFX reads them: `Shortcut+N`, `Shortcut+Shift+C` */
  final case class Item(label: String, act: Act, keys: String = "") extends Entry
  case object Separator extends Entry
  final case class Menu(title: String, entries: Vector[Entry])

  /** A LINK ON A PAGE that the window answers with an Open dialog — the
   * page's own road is a form with a path in it, the window's is the
   * dialog: `link` is the path of the link, `title` the dialog's,
   * `filter` its description and pattern (`"backup" -> "*.zip"`), `post`
   * where the file goes (the service answers with where to go next),
   * `failed` the note when it could not be sent, `media` its content type */
  final case class Pick(link: String, title: String, filter: (String, String), post: String, failed: (String, String),
                        /** the content type the file is posted as */
                        media: String = "application/octet-stream")

  /** THE BRIDGE the window installs on every page: clicks on
   * `a[download]` and on links whose path matches `saves` go to a Save
   * dialog; a form sent by a button `as=csv` (or `__press=csv`) too; a
   * pick's link opens its dialog. `window.okayApp` is the window's
   * `Bridge`. Idempotent: a page that already has it keeps it. */
  def script(app: App): String =
    val picks = app.picks.zipWithIndex.map { (p, i) =>
      s" if(a&&a.pathname==='${js(p.link)}'){e.preventDefault();window.okayApp.pick($i);return;}\n"
    }.mkString
    "(function(){if(window.__okayApp)return;window.__okayApp=1;\n" +
      "document.addEventListener('click',function(e){var a=e.target.closest&&e.target.closest('a[href]');\n" +
      picks +
      s" if(!a||!(a.hasAttribute('download')||new RegExp('${js(app.saves)}').test(a.pathname)))return;\n" +
      " e.preventDefault();window.okayApp.save(a.href);},true);\n" +
      "document.addEventListener('submit',function(e){var b=e.submitter;if(!b||b.value!=='csv'||(b.name!=='as'&&b.name!=='__press'))return;\n" +
      " e.preventDefault();var d=new URLSearchParams(new FormData(e.target));d.set(b.name,'csv');\n" +
      " window.okayApp.savePost(e.target.action,d.toString());},true);})();"

  /** a string inside a single-quoted JavaScript literal */
  private def js(s: String): String =
    s.replace("\\", "\\\\").replace("'", "\\'").replace("\n", "\\n").replace("\r", "")
