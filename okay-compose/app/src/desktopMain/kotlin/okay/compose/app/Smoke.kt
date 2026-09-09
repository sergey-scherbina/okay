// A headless smoke of the wire, no window: connect, print the tree,
// press a key, print what the server patched, close. For a box with
// no display, and for a quick "is the server speaking the protocol".
//   ./gradlew :app:smoke --args "ws://127.0.0.1:8080/counter?__live=counter inc"
package okay.compose.app

import okay.compose.protocol.*
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit

object Smoke {
    @JvmStatic
    fun main(args: Array<String>) {
        val url = args.getOrNull(0) ?: "ws://127.0.0.1:8080/counter?__live=counter"
        val press = args.getOrNull(1) ?: "inc"
        val trees = ArrayList<Ui>()
        val got = CountDownLatch(2)   // the tree, then the patch the press causes
        val closed = CountDownLatch(1)
        val client = Client(url, onTree = { trees += it; got.countDown() }, onClosed = { closed.countDown() })
        client.connect()
        if (!got.await(10, TimeUnit.SECONDS) && trees.isEmpty()) {
            println("SMOKE FAIL: no tree within 10s from $url"); System.exit(1)
        }
        println("tree: " + Wire.line(Msg.Tree(trees.first())))
        println("keys: " + Tree.keys(trees.first()))
        client.act(Event.Pressed(press))
        if (!got.await(10, TimeUnit.SECONDS)) { println("SMOKE FAIL: no patch after pressing '$press'"); System.exit(1) }
        println("after: " + Wire.line(Msg.Tree(trees.last())))
        client.close()
        println(if (trees.first() != trees.last()) "SMOKE OK: the press changed the tree" else "SMOKE FAIL: the tree did not change")
        System.exit(if (trees.first() != trees.last()) 0 else 1)
    }
}
