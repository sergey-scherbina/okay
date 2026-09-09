// The whole client as one composable: connect, draw the tree, act.
// Desktop's Main and Android's MainActivity both just show this.
package okay.compose.app

import androidx.compose.material.MaterialTheme
import androidx.compose.material.Surface
import androidx.compose.runtime.*
import okay.compose.protocol.Ui

@Composable
fun OkayApp(url: String) {
    var tree by remember { mutableStateOf<Ui>(Ui.Text("connecting to $url…")) }
    val client = remember {
        Client(url, onTree = { tree = it }, onClosed = { tree = Ui.Text("connection closed") })
            .also { it.connect() }
    }
    DisposableEffect(Unit) { onDispose { client.close() } }
    MaterialTheme { Surface { Render(tree, client::act) } }
}
