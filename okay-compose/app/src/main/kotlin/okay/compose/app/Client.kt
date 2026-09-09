// The wire: a WebSocket to an okay-script Live page (or anything that
// serves the protocol), hello first, lines in and out. The JDK's own
// WebSocket — no dependency.
package okay.compose.app

import okay.compose.protocol.*
import java.net.URI
import java.net.http.HttpClient
import java.net.http.WebSocket
import java.util.concurrent.CompletionStage

class Client(private val url: String, private val onTree: (Ui) -> Unit, private val onClosed: () -> Unit) {
    @Volatile var tree: Ui = Ui.Text("connecting…")
        private set
    private var socket: WebSocket? = null
    private val buffer = StringBuilder()

    fun connect() {
        val listener = object : WebSocket.Listener {
            override fun onOpen(ws: WebSocket) {
                ws.sendText(Wire.line(Msg.Hello(emptyList(), PROTOCOL_VERSION)), true)
                ws.request(1)
            }
            override fun onText(ws: WebSocket, data: CharSequence, last: Boolean): CompletionStage<*>? {
                buffer.append(data)
                if (last) {
                    receive(buffer.toString())
                    buffer.setLength(0)
                }
                ws.request(1)
                return null
            }
            override fun onClose(ws: WebSocket, statusCode: Int, reason: String): CompletionStage<*>? {
                onClosed(); return null
            }
            override fun onError(ws: WebSocket, error: Throwable) { onClosed() }
        }
        socket = HttpClient.newHttpClient().newWebSocketBuilder().buildAsync(URI.create(url), listener).join()
    }

    /** a server line: a tree or a patch lands on the kept tree; damage is dropped */
    private fun receive(line: String) {
        when (val m = Wire.parse(line)) {
            is Msg.Tree -> tree = m.ui
            is Msg.Patch -> tree = Tree.apply(tree, m.patch)
            is Msg.Close -> onClosed()
            else -> return
        }
        onTree(tree)
    }

    /** a host event: the hybrid rule decides whether it crosses */
    fun act(e: Event) {
        val (t, out) = Tree.outbound(tree, e)
        if (t !== tree) { tree = t; onTree(t) }
        out?.let { socket?.sendText(Wire.line(Msg.Event(it)), true) }
    }

    fun close() {
        socket?.sendText(Wire.line(Msg.Event(Event.Closed)), true)
        socket?.sendClose(WebSocket.NORMAL_CLOSURE, "bye")
    }
}
