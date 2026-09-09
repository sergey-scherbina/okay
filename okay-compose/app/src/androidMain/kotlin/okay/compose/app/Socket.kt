// the Android socket: OkHttp (Android has no java.net.http)
package okay.compose.app

import okhttp3.OkHttpClient
import okhttp3.Request
import okhttp3.Response
import okhttp3.WebSocket
import okhttp3.WebSocketListener

actual class Socket actual constructor(
    private val url: String, private val onOpen: () -> Unit, private val onText: (String) -> Unit, private val onClosed: () -> Unit,
) {
    private var ws: WebSocket? = null

    actual fun connect() {
        val client = OkHttpClient()
        ws = client.newWebSocket(Request.Builder().url(url).build(), object : WebSocketListener() {
            override fun onOpen(webSocket: WebSocket, response: Response) { ws = webSocket; onOpen() }
            override fun onMessage(webSocket: WebSocket, text: String) = onText(text)
            override fun onClosed(webSocket: WebSocket, code: Int, reason: String) = onClosed()
            override fun onFailure(webSocket: WebSocket, t: Throwable, response: Response?) = onClosed()
        })
    }

    actual fun send(text: String) { ws?.send(text) }
    actual fun close() { ws?.close(1000, "bye") }
}
