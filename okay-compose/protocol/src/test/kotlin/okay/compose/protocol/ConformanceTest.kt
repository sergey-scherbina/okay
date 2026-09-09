// The proof that this client speaks the protocol: replay
// docs/protocol/conformance.jsonl (rendered by okay's own
// TestProtocol) — apply every `in`, hold every `tree` it names, and
// produce every `out` from the same user actions.
package okay.compose.protocol

import kotlinx.serialization.json.*
import java.io.File
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertNotNull
import kotlin.test.assertTrue

class ConformanceTest {

    private val json = Json

    private fun script(): List<JsonObject> {
        val f = generateSequence(File("").absoluteFile) { it.parentFile }
            .map { File(it, "docs/protocol/conformance.jsonl") }.firstOrNull { it.exists() }
            ?: error("docs/protocol/conformance.jsonl not found above ${File("").absolutePath}")
        return f.readLines().filter { it.isNotBlank() }.map { json.parseToJsonElement(it).jsonObject }
    }

    @Test
    fun `every in applies, and the tree after each equals the one the script names`() {
        var tree: Ui? = null
        var applied = 0
        for (r in script()) {
            val line = r["in"] ?: continue
            val expected = r["tree"] ?: error("an in without a tree")
            when (val m = Wire.readMsg(line)) {
                is Msg.Tree -> tree = m.ui
                is Msg.Patch -> tree = Tree.apply(assertNotNull(tree, "a patch before any tree"), m.patch)
                else -> error("unexpected in: $line")
            }
            assertEquals(expected, Wire.ui(tree!!), "after in #$applied: $line")
            applied++
        }
        assertTrue(applied > 3, "the script had $applied ins")
    }

    @Test
    fun `every out re-encodes to itself, and the hybrid produces the Submitted from the tree`() {
        val outs = script().mapNotNull { it["out"] }
        assertTrue(outs.isNotEmpty())
        for (o in outs) {
            val m = assertNotNull(Wire.readMsg(o), "unreadable out: $o")
            assertEquals(o, Wire.msg(m), "re-encoding $o")
        }
        assertEquals(Msg.Hello(emptyList(), PROTOCOL_VERSION), Wire.readMsg(outs.first()))
        // the Submitted the script carries is what this client would send
        // after typing those values into the tree and pressing the form's button
        val submitted = outs.map { Wire.readMsg(it) }.filterIsInstance<Msg.Event>().map { it.event }
            .filterIsInstance<Event.Submitted>().first()
        val first = script().firstNotNullOf { r -> r["in"]?.let { Wire.readMsg(it) as? Msg.Tree }?.ui }
        var tree = first
        for (edit in submitted.edits) {
            val (t, sent) = Tree.outbound(tree, edit)
            assertEquals(null, sent, "a form field edit crossed the wire: $edit")
            tree = t
        }
        val (_, out) = Tree.outbound(tree, Event.Pressed(submitted.key))
        assertEquals(submitted, out)
    }

    @Test
    fun `damage is null, never a throw`() {
        assertEquals(null, Wire.parse("{ not json"))
        assertEquals(null, Wire.parse("""{"Tree":{"ui":{"Nope":{}}}}"""))
        assertEquals(null, Wire.parse("""{"Tree":{"ui":{"Text":{}}}}"""))
        assertEquals(Msg.Close, Wire.parse("""{"Close":{}}"""))
    }
}
