// The Android app: one activity that shows the client. The server
// address comes from the launch intent (`--es url ws://...`) or the
// emulator's host alias by default.
package okay.compose.app

import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent

class MainActivity : ComponentActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        val url = intent?.getStringExtra("url") ?: "ws://10.0.2.2:8080/counter?__live=counter"
        setContent { OkayApp(url) }
    }
}
