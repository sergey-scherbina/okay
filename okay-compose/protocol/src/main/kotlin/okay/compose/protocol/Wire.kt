// The JSON codec, by hand and by the document: a sum is an object
// with ONE key (the case name), a product its fields by name, an
// enumeration its lower-case name. Reading is TOTAL — damage is null,
// never a throw — because a wire that dies on one bad line loses
// every good one after it.
package okay.compose.protocol

import kotlinx.serialization.json.*

object Wire {
    private val json = Json { encodeDefaults = true }

    // ------------------------------------------------------------ write

    fun line(m: Msg): String = json.encodeToString(JsonElement.serializer(), msg(m))

    fun msg(m: Msg): JsonElement = when (m) {
        is Msg.Hello -> case("Hello", "vocab" to arr(m.vocab.map { JsonPrimitive(it) }), "version" to JsonPrimitive(m.version))
        is Msg.Tree -> case("Tree", "ui" to ui(m.ui))
        is Msg.Patch -> case("Patch", "patch" to patch(m.patch))
        is Msg.Event -> case("Event", "event" to event(m.event))
        Msg.Close -> case("Close")
    }

    fun ui(u: Ui): JsonElement = when (u) {
        is Ui.Text -> case("Text", "s" to str(u.s), "style" to style(u.style))
        is Ui.Row -> case("Row", "children" to uis(u.children), "key" to str(u.key))
        is Ui.Column -> case("Column", "children" to uis(u.children), "key" to str(u.key))
        is Ui.Box -> case(
            "Box", "children" to uis(u.children), "dir" to str(u.dir.wire),
            "weights" to arr(u.weights.map { JsonPrimitive(it) }), "gap" to JsonPrimitive(u.gap),
            "pad" to JsonPrimitive(u.pad), "key" to str(u.key),
        )
        is Ui.Image -> case("Image", "src" to str(u.src), "alt" to str(u.alt))
        is Ui.Button -> case("Button", "label" to str(u.label), "key" to str(u.key), "role" to str(u.role.name.lowercase()))
        is Ui.Input -> case(
            "Input", "value" to str(u.value), "key" to str(u.key), "label" to str(u.label),
            "kind" to str(u.kind.name.lowercase()), "live" to JsonPrimitive(u.live),
        )
        is Ui.Check -> case("Check", "on" to JsonPrimitive(u.on), "key" to str(u.key), "label" to str(u.label))
        is Ui.Select -> case("Select", "options" to arr(u.options.map { str(it) }), "selected" to JsonPrimitive(u.selected), "key" to str(u.key))
        is Ui.Scroll -> case("Scroll", "child" to ui(u.child), "key" to str(u.key))
        is Ui.Form -> case("Form", "fields" to uis(u.fields), "submit" to str(u.submit), "key" to str(u.key))
        is Ui.Items -> case("Items", "items" to uis(u.items), "key" to str(u.key))
        is Ui.Table -> case("Table", "header" to arr(u.header.map { str(it) }), "rows" to arr(u.rows.map { uis(it) }), "key" to str(u.key))
        is Ui.Tabs -> case("Tabs", "labels" to arr(u.labels.map { str(it) }), "selected" to JsonPrimitive(u.selected), "pages" to uis(u.pages), "key" to str(u.key))
        is Ui.Modal -> case("Modal", "title" to str(u.title), "body" to ui(u.body), "key" to str(u.key))
        is Ui.Disclosure -> case("Disclosure", "title" to str(u.title), "open" to JsonPrimitive(u.open), "body" to ui(u.body), "key" to str(u.key))
    }

    fun event(e: Event): JsonElement = when (e) {
        is Event.Pressed -> case("Pressed", "key" to str(e.key))
        is Event.Edited -> case("Edited", "key" to str(e.key), "value" to str(e.value))
        is Event.Toggled -> case("Toggled", "key" to str(e.key), "on" to JsonPrimitive(e.on))
        is Event.Chosen -> case("Chosen", "key" to str(e.key), "index" to JsonPrimitive(e.index))
        is Event.Key -> case("Key", "ch" to str(e.ch))
        is Event.Resized -> case("Resized", "w" to JsonPrimitive(e.w), "h" to JsonPrimitive(e.h))
        Event.Closed -> case("Closed")
        is Event.Submitted -> case("Submitted", "key" to str(e.key), "edits" to arr(e.edits.map { event(it) }))
    }

    fun patch(p: Patch): JsonElement = when (p) {
        is Patch.Replace -> case("Replace", "path" to ints(p.path), "ui" to ui(p.ui))
        is Patch.SetText -> case("SetText", "path" to ints(p.path), "s" to str(p.s))
        is Patch.SetValue -> case("SetValue", "path" to ints(p.path), "s" to str(p.s))
        is Patch.SetChecked -> case("SetChecked", "path" to ints(p.path), "on" to JsonPrimitive(p.on))
        is Patch.SetSelected -> case("SetSelected", "path" to ints(p.path), "index" to JsonPrimitive(p.index))
        is Patch.Remove -> case("Remove", "path" to ints(p.path), "index" to JsonPrimitive(p.index))
        is Patch.Reorder -> case("Reorder", "path" to ints(p.path), "order" to ints(p.order))
        is Patch.Insert -> case("Insert", "path" to ints(p.path), "index" to JsonPrimitive(p.index), "ui" to ui(p.ui))
    }

    private fun style(s: Style): JsonElement = JsonObject(
        mapOf(
            "bold" to JsonPrimitive(s.bold), "dim" to JsonPrimitive(s.dim),
            "tone" to str(s.tone.name.lowercase()), "size" to str(s.size.name.lowercase()),
        ),
    )

    private fun case(name: String, vararg fields: Pair<String, JsonElement>): JsonElement =
        JsonObject(mapOf(name to JsonObject(fields.toMap())))
    private fun str(s: String) = JsonPrimitive(s)
    private fun arr(xs: List<JsonElement>) = JsonArray(xs)
    private fun ints(xs: List<Int>) = JsonArray(xs.map { JsonPrimitive(it) })
    private fun uis(xs: List<Ui>) = JsonArray(xs.map { ui(it) })

    // ------------------------------------------------------------- read

    fun parse(line: String): Msg? = try {
        readMsg(json.parseToJsonElement(line))
    } catch (_: Exception) {
        null
    }

    fun readMsg(j: JsonElement): Msg? {
        val (name, f) = sum(j) ?: return null
        return when (name) {
            "Hello" -> Msg.Hello(f.strings("vocab") ?: return null, f.int("version") ?: return null)
            "Tree" -> Msg.Tree(readUi(f["ui"] ?: return null) ?: return null)
            "Patch" -> Msg.Patch(readPatch(f["patch"] ?: return null) ?: return null)
            "Event" -> Msg.Event(readEvent(f["event"] ?: return null) ?: return null)
            "Close" -> Msg.Close
            else -> null
        }
    }

    fun readUi(j: JsonElement): Ui? {
        val (name, f) = sum(j) ?: return null
        fun kids(field: String): List<Ui>? = (f[field] as? JsonArray)?.map { readUi(it) ?: return null }
        return when (name) {
            "Text" -> Ui.Text(f.str("s") ?: return null, readStyle(f["style"]) ?: return null)
            "Row" -> Ui.Row(kids("children") ?: return null, f.str("key") ?: "")
            "Column" -> Ui.Column(kids("children") ?: return null, f.str("key") ?: "")
            "Box" -> Ui.Box(
                kids("children") ?: return null,
                when (f.str("dir")) { "h" -> Dir.Horizontal; "v" -> Dir.Vertical; else -> return null },
                f.ints("weights") ?: emptyList(), f.int("gap") ?: 0, f.int("pad") ?: 0, f.str("key") ?: "",
            )
            "Image" -> Ui.Image(f.str("src") ?: return null, f.str("alt") ?: "")
            "Button" -> Ui.Button(f.str("label") ?: return null, f.str("key") ?: "", enum<Role>(f.str("role")) ?: Role.Plain)
            "Input" -> Ui.Input(
                f.str("value") ?: return null, f.str("key") ?: "", f.str("label") ?: "",
                enum<InputKind>(f.str("kind")) ?: InputKind.Text, f.bool("live") ?: false,
            )
            "Check" -> Ui.Check(f.bool("on") ?: return null, f.str("key") ?: "", f.str("label") ?: "")
            "Select" -> Ui.Select(f.strings("options") ?: return null, f.int("selected") ?: 0, f.str("key") ?: "")
            "Scroll" -> Ui.Scroll(readUi(f["child"] ?: return null) ?: return null, f.str("key") ?: "")
            "Form" -> Ui.Form(kids("fields") ?: return null, f.str("submit") ?: return null, f.str("key") ?: "")
            "Items" -> Ui.Items(kids("items") ?: return null, f.str("key") ?: "")
            "Table" -> Ui.Table(
                f.strings("header") ?: return null,
                (f["rows"] as? JsonArray)?.map { row -> (row as? JsonArray)?.map { readUi(it) ?: return null } ?: return null } ?: return null,
                f.str("key") ?: "",
            )
            "Tabs" -> Ui.Tabs(f.strings("labels") ?: return null, f.int("selected") ?: 0, kids("pages") ?: return null, f.str("key") ?: "")
            "Modal" -> Ui.Modal(f.str("title") ?: return null, readUi(f["body"] ?: return null) ?: return null, f.str("key") ?: "")
            "Disclosure" -> Ui.Disclosure(
                f.str("title") ?: return null, f.bool("open") ?: false,
                readUi(f["body"] ?: return null) ?: return null, f.str("key") ?: "",
            )
            else -> null
        }
    }

    fun readEvent(j: JsonElement): Event? {
        val (name, f) = sum(j) ?: return null
        return when (name) {
            "Pressed" -> Event.Pressed(f.str("key") ?: return null)
            "Edited" -> Event.Edited(f.str("key") ?: return null, f.str("value") ?: return null)
            "Toggled" -> Event.Toggled(f.str("key") ?: return null, f.bool("on") ?: return null)
            "Chosen" -> Event.Chosen(f.str("key") ?: return null, f.int("index") ?: return null)
            "Key" -> Event.Key(f.str("ch") ?: return null)
            "Resized" -> Event.Resized(f.int("w") ?: return null, f.int("h") ?: return null)
            "Closed" -> Event.Closed
            "Submitted" -> Event.Submitted(
                f.str("key") ?: return null,
                (f["edits"] as? JsonArray)?.map { readEvent(it) ?: return null } ?: return null,
            )
            else -> null
        }
    }

    fun readPatch(j: JsonElement): Patch? {
        val (name, f) = sum(j) ?: return null
        val path = f.ints("path") ?: return null
        return when (name) {
            "Replace" -> Patch.Replace(path, readUi(f["ui"] ?: return null) ?: return null)
            "SetText" -> Patch.SetText(path, f.str("s") ?: return null)
            "SetValue" -> Patch.SetValue(path, f.str("s") ?: return null)
            "SetChecked" -> Patch.SetChecked(path, f.bool("on") ?: return null)
            "SetSelected" -> Patch.SetSelected(path, f.int("index") ?: return null)
            "Remove" -> Patch.Remove(path, f.int("index") ?: return null)
            "Reorder" -> Patch.Reorder(path, f.ints("order") ?: return null)
            "Insert" -> Patch.Insert(path, f.int("index") ?: return null, readUi(f["ui"] ?: return null) ?: return null)
            else -> null
        }
    }

    private fun readStyle(j: JsonElement?): Style? {
        val f = j as? JsonObject ?: return Style()
        return Style(
            f.bool("bold") ?: false, f.bool("dim") ?: false,
            enum<Tone>(f.str("tone")) ?: Tone.Plain, enum<Size>(f.str("size")) ?: Size.Normal,
        )
    }

    /** the one-key object a sum is */
    private fun sum(j: JsonElement): Pair<String, JsonObject>? {
        val o = j as? JsonObject ?: return null
        if (o.size != 1) return null
        val (k, v) = o.entries.first()
        return k to (v as? JsonObject ?: return null)
    }

    private inline fun <reified E : Enum<E>> enum(name: String?): E? =
        name?.let { n -> enumValues<E>().firstOrNull { it.name.lowercase() == n } }
    private fun JsonObject.str(k: String): String? = (this[k] as? JsonPrimitive)?.takeIf { it.isString }?.content
    private fun JsonObject.bool(k: String): Boolean? = (this[k] as? JsonPrimitive)?.booleanOrNull
    private fun JsonObject.int(k: String): Int? = (this[k] as? JsonPrimitive)?.doubleOrNull?.toInt()
    private fun JsonObject.ints(k: String): List<Int>? = (this[k] as? JsonArray)?.map { (it as? JsonPrimitive)?.doubleOrNull?.toInt() ?: return null }
    private fun JsonObject.strings(k: String): List<String>? =
        (this[k] as? JsonArray)?.map { (it as? JsonPrimitive)?.takeIf { p -> p.isString }?.content ?: return null }
}
