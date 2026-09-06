```scala
import okay.script.api.*
```
<link rel="stylesheet" href="/style.css">

# Something broke on ${Web.current.path}

<pre>${Error.current.map(_.message).getOrElse("(no error?)")}</pre>
