```scala
import okay.script.api.*
include("parts/header.md")
```
<link rel="stylesheet" href="/style.css">

# Live

A server-driven okay-ui app: the state and the update live on the
server, the browser receives patches over this page's own WebSocket
and sends events back. Declared once (object level), mounted below.

```scala declare
// a declare block is object level: its imports are its own
import okay.ui.*
import okay.script.api.*
final case class Poll(yes: Int, no: Int, note: String)
val poll = Live(Poll(0, 0, ""))(p =>
  Ui.Column(Vector(
    Ui.Text(s"yes: ${p.yes}   no: ${p.no}", Style(bold = true)),
    Ui.Row(Vector(Ui.Button("yes", "yes"), Ui.Button("no", "no"))),
    Ui.Input(p.note, "note", "note"),
    Ui.Text(if p.note.isEmpty then "" else s"you wrote: ${p.note}", Style(dim = true)),
  )))((p, e) => e match
    case Event.Pressed("yes") => p.copy(yes = p.yes + 1)
    case Event.Pressed("no") => p.copy(no = p.no + 1)
    case Event.Edited("note", v) => p.copy(note = v)
    case _ => p)
```

${mount("poll", poll)}
