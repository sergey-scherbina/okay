// The frontend protocol's shapes, in Kotlin, from docs/protocol/frontend.md.
// No dependency on okay: this file is the document, transcribed. A
// sum is a sealed interface, a product a data class, an enumeration a
// Kotlin enum whose wire name is its lower-case name.
package okay.compose.protocol

enum class Dir(val wire: String) { Horizontal("h"), Vertical("v") }
enum class Role { Plain, Primary, Danger, Active }
enum class InputKind { Text, Secret, Multiline, Number }
enum class Tone { Plain, Emphasis, Muted, Danger }
enum class Size { Small, Normal, Large }

data class Style(
    val bold: Boolean = false,
    val dim: Boolean = false,
    val tone: Tone = Tone.Plain,
    val size: Size = Size.Normal,
)

/** the tree: level L (every client draws it) and level S (claimed, or lowered by the server) */
sealed interface Ui {
    data class Text(val s: String, val style: Style = Style()) : Ui
    data class Row(val children: List<Ui>, val key: String = "") : Ui
    data class Column(val children: List<Ui>, val key: String = "") : Ui
    data class Box(
        val children: List<Ui>, val dir: Dir, val weights: List<Int> = emptyList(),
        val gap: Int = 0, val pad: Int = 0, val key: String = "",
    ) : Ui
    data class Image(val src: String, val alt: String) : Ui
    data class Button(val label: String, val key: String, val role: Role = Role.Plain) : Ui
    data class Input(
        val value: String, val key: String, val label: String = "",
        val kind: InputKind = InputKind.Text, val live: Boolean = false,
    ) : Ui
    data class Check(val on: Boolean, val key: String, val label: String = "") : Ui
    data class Select(val options: List<String>, val selected: Int, val key: String) : Ui
    data class Scroll(val child: Ui, val key: String = "") : Ui
    /** level L too: the hybrid rule lives on it */
    data class Form(val fields: List<Ui>, val submit: String, val key: String) : Ui
    // level S — this client claims none of them; they arrive lowered
    data class Items(val items: List<Ui>, val key: String) : Ui
    data class Table(val header: List<String>, val rows: List<List<Ui>>, val key: String) : Ui
    data class Tabs(val labels: List<String>, val selected: Int, val pages: List<Ui>, val key: String) : Ui
    data class Modal(val title: String, val body: Ui, val key: String) : Ui
    data class Disclosure(val title: String, val open: Boolean, val body: Ui, val key: String) : Ui
}

sealed interface Event {
    data class Pressed(val key: String) : Event
    data class Edited(val key: String, val value: String) : Event
    data class Toggled(val key: String, val on: Boolean) : Event
    data class Chosen(val key: String, val index: Int) : Event
    data class Key(val ch: String) : Event
    data class Resized(val w: Int, val h: Int) : Event
    object Closed : Event { override fun toString() = "Closed" }
    /** a Form's one event: the field edits, sent together */
    data class Submitted(val key: String, val edits: List<Event>) : Event
}

sealed interface Patch {
    val path: List<Int>
    data class Replace(override val path: List<Int>, val ui: Ui) : Patch
    data class SetText(override val path: List<Int>, val s: String) : Patch
    data class SetValue(override val path: List<Int>, val s: String) : Patch
    data class SetChecked(override val path: List<Int>, val on: Boolean) : Patch
    data class SetSelected(override val path: List<Int>, val index: Int) : Patch
    data class Remove(override val path: List<Int>, val index: Int) : Patch
    data class Reorder(override val path: List<Int>, val order: List<Int>) : Patch
    data class Insert(override val path: List<Int>, val index: Int, val ui: Ui) : Patch
}

/** the envelope: every line on the wire is one of these */
sealed interface Msg {
    data class Hello(val vocab: List<String>, val version: Int) : Msg
    data class Tree(val ui: Ui) : Msg
    data class Patch(val patch: okay.compose.protocol.Patch) : Msg
    data class Event(val event: okay.compose.protocol.Event) : Msg
    object Close : Msg { override fun toString() = "Close" }
}

const val PROTOCOL_VERSION = 1
