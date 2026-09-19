## jetty-virtual-threads — okay-jetty's Server runs on Jetty's own VirtualThreadPool
Landed: 2026-09-19

`Server()` -> `Server(VirtualThreadPool())` — one virtual thread per
request on the main connector, closing the one place in the stack
still on ordinary platform threads (okay-http's JDK backend and this
module's own streaming-write path already used virtual threads).
Jetty 12's own class, no new dependency.

Verified with the module's real `Live`-tagged integration suite (`sbt
integrationTest`, real sockets): 19/19 pass, including WebSocket
sessions and resumable-stream replay.

See jdk-compatibility.md — this is what makes okay.Scoped's JDK25
ScopedValue backend (script-scoped-state-mrjar) actually pay off.
