---
route: true
---
```scala
import okay.script.api.*

val f = Web.current.form
val what = f.getOrElse("what", "").trim
if Web.current.method != "POST" || what.isEmpty then
  Response.current.status = 400
  println("{\"error\":\"a request needs something in it\"}")
else
  // a reference a person can quote back; the log is a later concern
  val ref = "R-" + math.abs((f.getOrElse("name", "") + what).hashCode % 100000)
  Response.current.contentType("application/json; charset=utf-8")
  println("{\"ref\":\"" + ref + "\"}")
```
