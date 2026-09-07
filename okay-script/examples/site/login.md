```scala declare
import okay.script.api.*
import okay.security.{Crypto, Password}
import okay.security.given // the JVM Crypto
// the demo's admin password is "okay"; a deployment keeps a hash it
// computed elsewhere -- what matters is that the PAGE never holds an
// issuer: the container mints on signIn (Site(issue = ...))
val adminHash: String = Password.hash("okay".toCharArray)
```
```scala
import okay.script.api.*
import okay.security.Password
import okay.security.given
include("parts/header.md")
val next = Web.current.query.getOrElse("next", "/")
val wrong = Web.current.method == "POST" && {
  val ok = Password.verify(Web.current.form.getOrElse("password", "").toCharArray, adminHash)
  if ok then
    signIn("admin", Set("admin"))
    Response.current.redirect(next)
  !ok
}
```
<link rel="stylesheet" href="/style.css">

# Sign in

```scala
if wrong then println("<p><b>Wrong password.</b></p>")
println(s"""<form method="post" action="/login?next=${java.net.URLEncoder.encode(next, "UTF-8")}">
  <label>Password <input type="password" name="password"></label>
  <button type="submit">Sign in</button>
</form>""")
```
