// okay-compose: a dumb native client of an okay frontend.
//   ./gradlew :app:run --args "ws://127.0.0.1:8080/counter?__live=counter"
// The server changes every piece of logic; this program never does.
package okay.compose.app

import androidx.compose.material.MaterialTheme
import androidx.compose.material.Surface
import androidx.compose.runtime.*
import androidx.compose.ui.window.Window
import androidx.compose.ui.window.application
import okay.compose.protocol.Ui

fun main(args: Array<String>) {
    val url = args.firstOrNull() ?: "ws://127.0.0.1:8080/counter?__live=counter"
    application {
        var tree by remember { mutableStateOf<Ui>(Ui.Text("connecting to $url…")) }
        val client = remember {
            Client(url, onTree = { tree = it }, onClosed = { tree = Ui.Text("connection closed") })
                .also { it.connect() }
        }
        Window(onCloseRequest = { client.close(); exitApplication() }, title = "okay") {
            MaterialTheme { Surface { Render(tree, client::act) } }
        }
    }
}
