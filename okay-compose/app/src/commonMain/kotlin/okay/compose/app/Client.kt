// The wire, platform-free: the session (the kept tree, the hybrid rule,
// hello first, events queued until the socket opens — M1's lesson) is
// common code; only the SOCKET differs by platform (expect/actual).
package okay.compose.app

import okay.compose.protocol.*

/** a text WebSocket: the one thing a platform provides */
expect class Socket(url: String, onOpen: () -> Unit, onText: (String) -> Unit, onClosed: () -> Unit) {
    fun connect()
    fun send(text: String)
    fun close()
}

class Client(url: String, private val onTree: (Ui) -> Unit, private val onClosed: () -> Unit) {
    var tree: Ui = Ui.Text("connecting…")
        private set
    private var open = false
    private val queue = ArrayList<Msg>()
    private val socket = Socket(url, onOpen = {
        val pending: List<Msg>
        synchronized(queue) { open = true; pending = ArrayList(queue); queue.clear() }
        raw(Msg.Hello(emptyList(), PROTOCOL_VERSION))
        pending.forEach { raw(it) }
    }, onText = { receive(it) }, onClosed = { onClosed() })

    fun connect(): Unit { socket.connect() }

    /** a server line: a tree or a patch lands on the kept tree; damage is dropped */
    private fun receive(line: String) {
        when (val m = Wire.parse(line)) {
            is Msg.Tree -> tree = m.ui
            is Msg.Patch -> tree = Tree.apply(tree, m.patch)
            is Msg.Close -> { onClosed(); return }
            else -> return
        }
        onTree(tree)
    }

    /** a host event: the hybrid rule decides whether it crosses */
    fun act(e: Event) {
        val (t, out) = Tree.outbound(tree, e)
        if (t !== tree) { tree = t; onTree(t) }
        out?.let { send(Msg.Event(it)) }
    }

    private fun send(m: Msg) {
        val ready: Boolean
        synchronized(queue) { ready = open; if (!ready) queue.add(m) }
        if (ready) raw(m)
    }

    private fun raw(m: Msg): Unit { socket.send(Wire.line(m)) }

    fun close() {
        send(Msg.Event(Event.Closed))
        socket.close()
    }
}
