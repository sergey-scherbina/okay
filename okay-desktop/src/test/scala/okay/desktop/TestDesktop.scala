package okay.desktop

import java.nio.file.{Files, Path}

/**
 * app-host (specs/app-host.md): the pure parts of the app's own window
 * and launch — the state file, the data directory, the bridge script —
 * on a headless box, and nothing here loads a JavaFX class.
 */
class TestDesktop extends munit.FunSuite:

  test("the window's size and place round-trip; a window under the floor comes back as the default") {
    val dir = Files.createTempDirectory("okay-desktop")
    assertEquals(WindowState.read(dir), WindowState.Default, "nothing written yet")
    WindowState(10, 20, 1000, 700).write(dir)
    assertEquals(WindowState.read(dir), WindowState(10, 20, 1000, 700))
    WindowState(10, 20, 300, 200).write(dir)
    assertEquals(WindowState.read(dir), WindowState.Default, "under the floor")
    Files.writeString(dir.resolve(WindowState.File), "garbage")
    assertEquals(WindowState.read(dir), WindowState.Default)
  }

  test("the data directory is where each system keeps an application's data") {
    val home = Path.of("/Users/anna")
    assertEquals(Desktop.dataDir("okay-watch", "Mac OS X", home, _ => None), Path.of("/Users/anna/Library/Application Support/okay-watch"))
    assertEquals(Desktop.dataDir("okay-watch", "Windows 11", Path.of("C:/Users/anna"), Map("APPDATA" -> "C:/Users/anna/AppData/Roaming").get),
      Path.of("C:/Users/anna/AppData/Roaming/okay-watch"))
    assertEquals(Desktop.dataDir("okay-watch", "Windows 11", Path.of("C:/Users/anna"), _ => None), Path.of("C:/Users/anna/AppData/Roaming/okay-watch"))
    assertEquals(Desktop.dataDir("okay-watch", "Linux", Path.of("/home/anna"), _ => None), Path.of("/home/anna/.local/share/okay-watch"))
    assertEquals(Desktop.dataDir("okay-watch", "Linux", Path.of("/home/anna"), Map("XDG_DATA_HOME" -> "/data").get), Path.of("/data/okay-watch"))
  }

  test("the bridge script names every pick's link and the saves pattern; a product with no picks saves downloads only") {
    val app = App("okay-watch", "http://127.0.0.1:8099", "/ui/trace",
      picks = Vector(
        App.Pick("/ui/backup/restore", "Restore from a backup", "backup" -> "*.zip", "/ui/backup/restore", "It could not be restored" -> "not read"),
        App.Pick("/ui/received/open", "Open a case file", "case file" -> "*.zip", "/ui/received/file", "It could not be opened" -> "not read")))
    val s = App.script(app)
    assert(s.contains("a.pathname==='/ui/backup/restore'") && s.contains("window.okayApp.pick(0)"), s)
    assert(s.contains("a.pathname==='/ui/received/open'") && s.contains("window.okayApp.pick(1)"), s)
    assert(s.contains("new RegExp('\\\\.(csv|json|txt|zip)$')"), s)
    assert(s.contains("window.okayApp.save(a.href)") && s.contains("window.okayApp.savePost("))
    val plain = App.script(App("x", "http://127.0.0.1:1", "/", saves = "\\.pdf$"))
    assert(!plain.contains("pick(") && plain.contains("new RegExp('\\\\.pdf$')"), plain)
    assert(plain.startsWith("(function(){if(window.__okayApp)return;"), "idempotent on a page that has it")
  }

  test("on the app's own pages every POST form goes through the bridge; a form Enhance took is left to it") {
    val s = App.script(App("x", "app://x", "/"))
    assert(s.contains("location.protocol!=='app:'") && s.contains("e.defaultPrevented"), s)
    assert(s.contains("window.okayApp.send('POST',f.action") && s.contains("window.okayApp.open(r.url)"), s)
  }

  test("a quote in a pick's link is escaped in the script") {
    val s = App.script(App("x", "http://127.0.0.1:1", "/", picks = Vector(App.Pick("/it's", "t", "a" -> "*", "/p", "f" -> "g"))))
    assert(s.contains("a.pathname==='/it\\'s'"), s)
  }
