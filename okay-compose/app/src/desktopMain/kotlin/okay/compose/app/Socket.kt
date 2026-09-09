// the desktop socket: the JDK's own WebSocket, no dependency
package okay.compose.app

import java.net.URI
import java.net.http.HttpClient
import java.net.http.WebSocket
import java.util.concurrent.CompletionStage

actual class Socket actual constructor(
    private val url: String, private val onOpen: () -> Unit, private val onText: (String) -> Unit, private val onClosed: () -> Unit,
) {
    private var ws: WebSocket? = null
    private val buffer = StringBuilder()

    actual fun connect() {
        val listener = object : WebSocket.Listener {
            // onOpen fires INSIDE buildAsync().join(), before the field below is
            // assigned: take the socket from the callback (found by a smoke whose
            // hello never left, so no tree ever came)
            override fun onOpen(w: WebSocket) { ws = w; onOpen(); w.request(1) }
            override fun onText(w: WebSocket, data: CharSequence, last: Boolean): CompletionStage<*>? {
                buffer.append(data)
                if (last) { onText(buffer.toString()); buffer.setLength(0) }
                w.request(1)
                return null
            }
            override fun onClose(w: WebSocket, statusCode: Int, reason: String): CompletionStage<*>? { onClosed(); return null }
            override fun onError(w: WebSocket, error: Throwable) { onClosed() }
        }
        ws = HttpClient.newHttpClient().newWebSocketBuilder().buildAsync(URI.create(url), listener).join()
    }

    actual fun send(text: String) { ws?.sendText(text, true) }
    actual fun close() { ws?.sendClose(WebSocket.NORMAL_CLOSURE, "bye") }
}
