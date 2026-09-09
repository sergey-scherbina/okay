package okay.script

/**
 * The mobile web leg of specs/frontend.md ("Mobile", M1): what turns
 * a Live page into an installable, mobile-first application with no
 * build step — three files the container serves under `/__okay/`,
 * and the head fragment `api.installable` that points a page at them.
 *
 * - `app.css`: level L drawn for a phone — flex rows and columns,
 *   tap-sized controls (44px, Apple's and Google's floor), 16px inputs
 *   (below that iOS zooms the page on focus), tokens as classes.
 * - `sw.js`: a service worker that keeps the SHELL — the page the
 *   browser last fetched, `live.js`, `app.css` — so the page opens
 *   offline (the SSR'd tree is whole without a socket) and reconnects
 *   when the network is back. Network first, cache on failure: the
 *   server stays the truth here too.
 * - `manifest.webmanifest`: name, start URL, standalone display, an
 *   SVG icon — what "Add to Home Screen" reads.
 */
object Mobile:
  def paths(p: String): Boolean = p == CssPath || p == SwPath || p == ManifestPath || p == IconPath
  val CssPath = "/__okay/app.css"
  val SwPath = "/__okay/sw.js"
  val ManifestPath = "/__okay/manifest.webmanifest"
  val IconPath = "/__okay/icon.svg"

  val css: String =
    """:root { color-scheme: light dark; --okay-gap: 8px; --okay-tap: 44px; }
      |body { margin: 0; padding: 12px; font: 16px/1.4 -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif; }
      |.okay-col, .okay-box.okay-v, .okay-form { display: flex; flex-direction: column; gap: var(--okay-gap); }
      |.okay-row, .okay-box.okay-h { display: flex; flex-direction: row; flex-wrap: wrap; gap: var(--okay-gap); align-items: center; }
      |.okay-scroll { overflow: auto; -webkit-overflow-scrolling: touch; max-height: 70vh; }
      |button, input, select, textarea { min-height: var(--okay-tap); font-size: 16px; border-radius: 8px; border: 1px solid #8884; padding: 0 12px; box-sizing: border-box; }
      |button { background: #eee; color: inherit; }
      |button.okay-primary { background: #2563eb; color: #fff; border-color: #2563eb; }
      |button.okay-danger { background: #dc2626; color: #fff; border-color: #dc2626; }
      |button.okay-active { font-weight: bold; text-decoration: underline; }
      |input, select, textarea { width: 100%; }
      |label { display: flex; flex-direction: column; gap: 4px; }
      |label:has(input[type=checkbox]) { flex-direction: row; align-items: center; }
      |input[type=checkbox] { width: var(--okay-tap); height: var(--okay-tap); min-height: 0; }
      |textarea { min-height: calc(var(--okay-tap) * 2); padding: 8px 12px; }
      |img { max-width: 100%; height: auto; }
      |.okay-bold, .okay-tone-emphasis { font-weight: bold; }
      |.okay-dim, .okay-tone-muted { opacity: 0.6; }
      |.okay-tone-danger { color: #dc2626; }
      |.okay-size-small { font-size: 0.85em; }
      |.okay-size-large { font-size: 1.4em; }
      |@media (min-width: 720px) { body { max-width: 680px; margin: 0 auto; } }
      |""".stripMargin

  /** the shell: network first, cache on failure — offline opens the
   * page as it was last seen, and the socket reconnects when it can */
  val serviceWorker: String =
    """var CACHE = "okay-shell-v1";
      |self.addEventListener("install", function (e) {
      |  e.waitUntil(caches.open(CACHE).then(function (c) { return c.addAll(["/__okay/live.js", "/__okay/app.css"]); }).then(function () { return self.skipWaiting(); }));
      |});
      |self.addEventListener("activate", function (e) { e.waitUntil(self.clients.claim()); });
      |self.addEventListener("fetch", function (e) {
      |  var req = e.request;
      |  if (req.method !== "GET" || new URL(req.url).origin !== self.location.origin) return;
      |  e.respondWith(
      |    fetch(req).then(function (res) {
      |      var copy = res.clone();
      |      caches.open(CACHE).then(function (c) { c.put(req, copy); });
      |      return res;
      |    }).catch(function () {
      |      return caches.match(req).then(function (hit) { return hit || caches.match(req, { ignoreSearch: true }); });
      |    })
      |  );
      |});
      |""".stripMargin

  val icon: String =
    """<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 64 64"><rect width="64" height="64" rx="14" fill="#2563eb"/><text x="32" y="42" font-size="30" text-anchor="middle" fill="#fff" font-family="sans-serif" font-weight="bold">ok</text></svg>
      |""".stripMargin

  /** the manifest for one page: `name` and the page's own path as `start_url` */
  def manifest(name: String, start: String): String =
    def q(s: String) = "\"" + s.replace("\\", "\\\\").replace("\"", "\\\"") + "\""
    s"""{"name": ${q(name)}, "short_name": ${q(name.take(12))}, "start_url": ${q(start)}, "scope": "/", "display": "standalone",
       |"background_color": "#ffffff", "theme_color": "#2563eb",
       |"icons": [{"src": "$IconPath", "sizes": "any", "type": "image/svg+xml", "purpose": "any"}]}
       |""".stripMargin

  /** the head fragment a page puts before its content: viewport,
   * stylesheet, manifest, theme colour, and the worker's registration.
   * The worker lives under `/__okay/` but must control the PAGE, so
   * it registers with scope `/` — which the `Service-Worker-Allowed`
   * header the container sends permits (found by a `ready` that never
   * resolved: a scope of `/__okay/` covers no page) */
  def head(name: String, start: String): String =
    val enc = java.net.URLEncoder.encode(name, "UTF-8").replace("+", "%20")
    val startEnc = java.net.URLEncoder.encode(start, "UTF-8").replace("+", "%20")
    s"""<meta name="viewport" content="width=device-width, initial-scale=1, viewport-fit=cover">
       |<meta name="theme-color" content="#2563eb">
       |<meta name="apple-mobile-web-app-capable" content="yes">
       |<link rel="stylesheet" href="$CssPath">
       |<link rel="manifest" href="$ManifestPath?name=$enc&start=$startEnc">
       |<link rel="apple-touch-icon" href="$IconPath">
       |<script>if ("serviceWorker" in navigator) navigator.serviceWorker.register("$SwPath", { scope: "/" }).then(function () {
       |  // the first load happened before the worker controlled it: keep THIS page too
       |  if (window.caches) caches.open("okay-shell-v1").then(function (c) { c.add(location.pathname + location.search); });
       |});</script>
       |""".stripMargin
