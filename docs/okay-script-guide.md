# A site out of markdown, from an empty directory

A shop — pages, a cart, a live counter, two languages — where every
page is a `.md` file compiled by the real Scala compiler when it is
first asked for. No build step for the site, no artifact: the
directory IS the application.

Every command below was executed in the order it appears and the
outputs quoted are the ones that came back; the store built here is
the one you will see in the transcripts.

What you end with: ten files, a running server, and a container image
if you want one.

**Prerequisites**: JDK 21+, sbt, and this repository cloned. Nothing
else — no Node, no template language, no framework configuration.

The decisions behind all of this live in
[`specs/okay-script.md`](../specs/okay-script.md); what a page costs
is measured in [benchmarks §19](benchmarks.md). This page is neither:
it is the road from an empty directory to a site.

---

## 1. A page

Make a directory and put one file in it.

```
mkdir store
```

`store/index.md`:

```markdown
# The Corner Shop

Open every day.
```

Serve it:

```
sbt "okayScript/runMain okay.script.Serve store 8080"
```

It prints what it did and where it is:

```
okay-script: compiled 1 page(s) in 220 ms
okay-script: serving /…/store at http://127.0.0.1:8080/
```

The compile happens at START, not at the first visitor: a page that
does not compile is named on stderr right there, and the rest of the
site serves anyway. `curl http://127.0.0.1:8080/` gives back the
markdown as it stands. Nothing has been converted to HTML for you —
what you write is what is sent, so if you want HTML, write HTML.
Everything below is about the parts you cannot write by hand.

## 2. Scala in the page

A fenced ` ```scala ` block runs where it stands, and `${…}` in the
prose is an expression:

```markdown
```scala
val stock = Vector("mug" -> 12, "tee" -> 25)
```
We sell ${stock.size} things.
```

One page is one compilation unit: a later block sees what an earlier
block defined, exactly as one Scala file would.

A ` ```scala declare ` block is different — it goes at OBJECT level,
outside the per-request body (JSP's `<%! %>`). A `val` there is built
once per compile and shared by every request; a `def` there is
callable from anywhere in the page, including from a `${…}` ABOVE
it. That is where a catalog, a helper, a `Schema` belongs.

## 3. The request, the response, the session

A page reaches the container through `okay.script.api`:

```markdown
```scala
import okay.script.api.*
```
You asked for ${Web.current.path} with ${Web.current.method}.
```

`Web.current` carries method, path, query, headers, cookies, the
posted form, uploaded parts and route params. `Response.current` sets
the status, headers, content type, cookies, or redirects.
`Session.current` is a cookie-backed session, created only when a
page actually puts something in it.

The store's header, `store/parts/header.md`, uses two of them:

```markdown
```scala
import okay.script.api.*
```
<nav><a href="/">shop</a> · <a href="/cart">cart (${Session.current.get("cart").map(_.split(",").length).getOrElse(0)})</a></nav>
```

and every page pulls it in with `include("parts/header.md")` — the
included page renders with the SAME request, response and session.

## 4. Routing is the directory

| the URL | the file |
|---|---|
| `/` | `index.md` |
| `/cart` | `cart.md` |
| `/shop/` | `shop/index.md` |
| `/product/mug` | `product/[sku].md`, with `Web.current.params("sku") == "mug"` |
| `/style.css` | the file itself, with an ETag |

A literal file always wins over a `[param]` one. Nothing else routes:
there is no route table to keep in step with the directory, because
the directory is the route table.

`store/product/[sku].md`:

```markdown
```scala
import okay.script.api.*
include("../parts/header.md")
val sku = Web.current.params("sku")
val price = Map("mug" -> 12, "tee" -> 25).get(sku)
```
```scala
price match
  case None => Response.current.status = 404; println(s"<p>no such thing: $sku</p>")
  case Some(p) =>
    println(s"<h1>$sku</h1><p>$$$p</p>")
    println(s"""<form method="post" action="/cart"><input type="hidden" name="sku" value="$sku"><button>add</button></form>""")
```
```

```
$ curl -s http://127.0.0.1:8080/product/mug
<nav>…</nav><h1>mug</h1><p>$12</p>
<form method="post" action="/cart"><input type="hidden" name="sku" value="mug"><button>add</button></form>

$ curl -s -o /dev/null -w "%{http_code}\n" http://127.0.0.1:8080/product/nope
404
```

## 5. A form, a session, a redirect

`store/cart.md` handles its own POST and redirects after it — the
POST-redirect-GET every form wants:

```markdown
```scala
import okay.script.api.*
Web.current.form.get("sku").foreach { s =>
  val had = Session.current.get("cart").map(_.split(",").toVector).getOrElse(Vector.empty)
  Session.current.set("cart", (had :+ s).mkString(","))
  Response.current.redirect("/cart")
}
include("parts/header.md")
```
# Cart
```scala
Session.current.get("cart") match
  case None => println("<p>empty</p>")
  case Some(csv) => println(csv.split(",").map(s => s"<li>$s</li>").mkString("<ul>", "", "</ul>"))
```
```

The session cookie appears exactly when the session is first written:

```
$ curl -s -i -X POST -d "sku=mug" -c jar.txt http://127.0.0.1:8080/cart | head -6
HTTP/1.1 302 Found
Location: /cart
Set-Cookie: OKAYSESSID=OlvUbLDcm_9TCaQ7oqdhbg; Path=/; HttpOnly

$ curl -s -b jar.txt http://127.0.0.1:8080/cart
<nav><a href="/">shop</a> · <a href="/cart">cart (1)</a> …</nav># Cart<ul><li>mug</li></ul>
```

**Typed forms.** For a form with more than one field, do not parse
`Web.current.form` by hand — derive both halves from a `Schema`:

```markdown
```scala declare
import okay.script.api.*
import okay.codec.Schema
final case class Order(name: String, qty: Int)
given Schema[Order] = Schema.derived
```
```scala
import okay.script.api.*
val posted = if Web.current.method == "POST" then Forms.read[Order](Web.current.form) else Left(Forms.Draft.empty)
posted match
  case Right(o) => println(s"<p>thanks ${o.name}, ${o.qty} on the way</p>")
  case Left(draft) => println(Forms.html[Order]("/checkout", draft, "Order"))
```
```

The form is rendered from the same `Schema` that decodes it, so it
cannot drift from its parser; a failed read hands back the value AND
the per-field errors to render again with.

## 6. A live page

For a page that changes without a reload, declare an okay-ui app and
mount it. The state and the update run on the SERVER; the browser
gets patches over the page's own WebSocket.

`store/counter.md`:

```markdown
```scala declare
import okay.script.api.*
import okay.ui.*
val clicks = Live(0)(n => Ui.Column(Vector(Ui.Text(s"clicks: $n"), Ui.Button("+1", "go"))))(
  (n, e) => e match { case Event.Pressed("go") => n + 1; case _ => n })
```
```scala
import okay.script.api.*
include("parts/header.md")
```
# A live counter
${mount("clicks", clicks)}
```

What is served first is the tree as HTML, so the page is whole before
any JavaScript runs:

```
$ curl -s http://127.0.0.1:8080/counter
…# A live counter
<div id="okay-live-clicks" data-okay-live="clicks"><div class="okay-col"><span>clicks: 0</span><button data-key="go">+1</button></div></div><script src="/__okay/live.js"></script><script>okayLive("clicks")</script>
```

The socket needs the WebSocket half of the server, which the stock
entry point already wires: `Jetty.serve(port)(site.routes)(site.ws,
site.push)`. A `Live.form[A](submit)` is the same machinery with a
typed form inside it, and `Live(..., push = source)` lets the SERVER
push events (a clock, a feed) into every session.

## 7. Two languages

Name the languages the site speaks, and a page can have variants:

```
OKAY_LANGS=en,uk sbt "okayScript/runMain okay.script.Serve store 8080"
```

`store/index.uk.md` is what `/` renders when the language is `uk`;
`store/i18n/uk.yaml` holds the strings `t("hello")` looks up. The
language is `?lang=` (remembered in a cookie), then the cookie, then
`Accept-Language`, then the first language.

```
$ curl -s -i "http://127.0.0.1:8080/?lang=uk" | grep -i set-cookie
Set-Cookie: OKAYLANG=uk; Path=/

$ curl -s "http://127.0.0.1:8080/?lang=uk"
…# Крамниця на розі
<p>Вітаємо</p>
```

A variant inherits its base page's front-matter, so a `secure:` rule
holds for a translation whether or not the translator repeated it.

## 8. Locking a page, and signing in

Put the rule in the page:

```markdown
---
secure: admin
---
```

and give the Site a verifier — which is the point where you write a
`main` of your own, because a deployment's identity is not something
a directory of pages can hold:

```scala
import okay.*, okay.given, okay.script.*, okay.security.SessionIssuer

@main def store(): Unit =
  val issuer = SessionIssuer()
  val site = Site(java.nio.file.Paths.get("store"),
    verify = Some(issuer.verify(_)),
    issue  = Some((subject, scopes) => issuer.issue(subject, scopes)))
  Resource.run[Unit, Pure](site.serve(8080).map(_ => Thread.sleep(Long.MaxValue))).runWith
```

A login page then checks whatever it checks and says
`signIn("admin", Set("admin"))`; the container mints the token and
stores it in the session. Without a token the visitor is sent to
`login.md` with `?next=`; with the wrong scope they get a 403; a
`secure:` page on a Site with no verifier is a 500, never an open
door. `Principal.current` tells a page who came in.

The example store does exactly this —
[`okay-script/examples/site`](../okay-script/examples/site) has
`login.md`, `admin.md` (a `Live.form` and a plain form both editing
the catalog) and `logout.md`, and its tests drive the whole flow.

## 9. What a page costs

Worth knowing before you decide anything else (benchmarks §19):

| | |
|---|---:|
| compile a page (once) | ~150 ms |
| answer a request (after that) | 0.062 ms |
| a static file, validated (304) | 0.021 ms |
| memory per compiled page | 87 KiB |
| renders per second, 4 threads | ~100 000 |

A page is compiled once and then it is free — the ratio is about
2500. The first page of a process costs 870 ms because the compiler
warms up in it, which is why the whole directory is compiled at boot
rather than lazily.

Add `cache: 60` to a page's front-matter and it carries an ETag and
`Cache-Control`; the container makes that `private` by itself if the
page is `secure:`, sets a cookie, or rides a session:

```
$ curl -s -D - -o /dev/null http://127.0.0.1:8080/ | grep -iE "etag|cache-control"
ETag: "PtAZvJpMjt8AleQgru_9zY"
Cache-Control: public, max-age=60
```

## 10. Running it for real

The stock entry point takes the directory and the port, and reads the
rest from the environment:

| | |
|---|---|
| `OKAY_DATA=/var/lib/store` | sessions and the application scope survive a restart |
| `OKAY_LANGS=en,uk` | the languages |
| `OKAY_OPS=1` | `/healthz`, `/stats`, `/metrics` beside the pages |
| `OKAY_TLS_CERT=…` `OKAY_TLS_KEY=file:…` | HTTPS with a real certificate (RSA or EC, `fullchain.pem` and its key) |
| `OKAY_TLS_RELOAD=3600` | re-read the certificate when it changes — certbot renews, no restart |
| `OKAY_FORWARDED=1` | behind a TLS-terminating proxy: trust `X-Forwarded-Proto`, so cookies still get `Secure` |
| `OKAY_TLS=self` | a self-signed certificate, generated once — https with nothing to obtain first |
| `OKAY_HSTS=31536000` | `Strict-Transport-Security` on secure responses |
| `OKAY_HTTPS_ONLY=1` | answer an insecure request with a 301 to https |
| `OKAY_HTTP_PORT=80` | with own TLS: a plaintext port that only redirects |
| `OKAY_PAGES` `OKAY_PORT` | the directory and port, when no command line is given |

```
$ OKAY_OPS=1 OKAY_LANGS=en,uk sbt "okayScript/runMain okay.script.Serve store 8080"
okay-script: compiled 6 page(s) in 2117 ms
okay-script: serving /…/store at http://127.0.0.1:8080/ (+ /healthz /stats /metrics)

$ curl -s http://127.0.0.1:8080/metrics | head -3
# HELP okay_script_page_requests_total requests that resolved to a page …
# TYPE okay_script_page_requests_total counter
okay_script_page_requests_total 1
```

**A container.** `okay-script/deploy/` is rendered from one value
(`ScriptDeploy`) and carries a Dockerfile, a compose file and a Helm
chart. The image runs the same entry point over `/app/pages`; mount
your own directory over that and the site is yours. Because pages are
read at request time, a mounted directory that changes is a site that
changes — no rebuild, no restart.

**https on a dev box, with nothing to obtain.**

```
$ OKAY_TLS=self OKAY_DATA=./data sbt "okayScript/runMain okay.script.Serve store 8443"
okay-script: self-signed certificate in data/okay-script-tls.p12
okay-script: its SHA-256 is A1:B2:… -- a browser will warn, because nobody vouched for it
okay-script: serving /…/store at https://127.0.0.1:8443/
```

The keystore sits beside the data, so a restart keeps the same
identity. Add `OKAY_HTTP_PORT=8080` and that port answers every
request with a 301 to the https one. A public site gets a real certificate instead:

```
$ OKAY_TLS_CERT=/etc/letsencrypt/live/shop/fullchain.pem \
  OKAY_TLS_KEY=file:/etc/letsencrypt/live/shop/privkey.pem \
  OKAY_TLS_RELOAD=3600 sbt "okayScript/runMain okay.script.Serve store 443"
```

The key may be RSA or EC, `fullchain.pem` is presented as the chain
it is, and with `OKAY_TLS_RELOAD` a renewal on disk reaches the next
connection without a restart — a certbot deploy hook is not even
needed. What okay-script does NOT do is OBTAIN the certificate: that
is ACME, the protocol a CA uses to check you control the domain
(it hands you a token to serve under `/.well-known/acme-challenge/`
or to put in DNS, then issues a 90-day certificate). certbot or a
proxy that speaks ACME does that part.

**Behind a proxy.** Terminating TLS in nginx, Caddy or an ingress is
the usual shape, and three things then need saying. Pass `Upgrade`
and `Connection` for EVERY path, not just one — a live page opens its
socket on the page's own path. Set `OKAY_FORWARDED=1` (or
`Site(secureCookies = Some(true))`, which trusts nothing) so cookies
still carry `Secure` when the app itself sees plain HTTP — and on
that footing `OKAY_HSTS` and `OKAY_HTTPS_ONLY` do the two things the
proxy leaves to the app. And treat
`X-Forwarded-For` as the proxy's claim it is: `Web.current.header`
gives it to you, trusting it is your decision.

**Sharing sessions across nodes.** `Sessions.shared(topic)` over an
okay-persist replicated topic makes a cart set on one node readable
on another; `Application.persisted(store)` does the same for the
site-wide scope. Both are constructor arguments to `Site`, and
nothing in a page changes.

---

## Where to go next

- [`specs/okay-script.md`](../specs/okay-script.md) — every decision
  and what it was measured or argued against.
- [`okay-script/examples/site`](../okay-script/examples/site) — the
  finished store this guide is a smaller copy of: catalog, cart,
  checkout with a typed form, admin behind a login, a live poll, two
  languages, an error page.
- [modules/okay-script.md](modules/okay-script.md) — the one-line
  index of every piece.
- [benchmarks §19](benchmarks.md) — the numbers, and the method.
