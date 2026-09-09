// What a client does with the tree: apply the server's patches
// (paths index children in order — docs/protocol/frontend.md), and
// the HYBRID rule: a Form's fields fold here, its button submits once,
// a live input speaks per change, everything else crosses the wire.
package okay.compose.protocol

object Tree {

    // ------------------------------------------------------------ patch

    fun apply(tree: Ui, p: Patch): Ui = when (p) {
        is Patch.Replace -> at(tree, p.path) { p.ui }
        is Patch.SetText -> at(tree, p.path) { u -> if (u is Ui.Text) u.copy(s = p.s) else u }
        is Patch.SetValue -> at(tree, p.path) { u -> if (u is Ui.Input) u.copy(value = p.s) else u }
        is Patch.SetChecked -> at(tree, p.path) { u -> if (u is Ui.Check) u.copy(on = p.on) else u }
        is Patch.SetSelected -> at(tree, p.path) { u -> if (u is Ui.Select) u.copy(selected = p.index) else u }
        is Patch.Remove -> at(tree, p.path) { u -> kids(u) { c -> c.filterIndexed { i, _ -> i != p.index } } }
        is Patch.Reorder -> at(tree, p.path) { u -> kids(u) { c -> p.order.map { c[it] } } }
        is Patch.Insert -> at(tree, p.path) { u -> kids(u) { c -> c.toMutableList().apply { add(p.index, p.ui) } } }
    }

    private fun at(u: Ui, path: List<Int>, f: (Ui) -> Ui): Ui {
        if (path.isEmpty()) return f(u)
        val i = path[0]
        val rest = path.drop(1)
        return when (u) {
            is Ui.Row -> u.copy(children = u.children.set(i) { at(it, rest, f) })
            is Ui.Column -> u.copy(children = u.children.set(i) { at(it, rest, f) })
            is Ui.Box -> u.copy(children = u.children.set(i) { at(it, rest, f) })
            is Ui.Scroll -> if (i == 0) u.copy(child = at(u.child, rest, f)) else u
            is Ui.Form -> u.copy(fields = u.fields.set(i) { at(it, rest, f) })
            is Ui.Items -> u.copy(items = u.items.set(i) { at(it, rest, f) })
            is Ui.Modal -> if (i == 1) u.copy(body = at(u.body, rest, f)) else u
            is Ui.Disclosure -> if (i == 1) u.copy(body = at(u.body, rest, f)) else u
            else -> u   // a path into a leaf: the server never makes one
        }
    }

    private fun kids(u: Ui, f: (List<Ui>) -> List<Ui>): Ui = when (u) {
        is Ui.Row -> u.copy(children = f(u.children))
        is Ui.Column -> u.copy(children = f(u.children))
        is Ui.Box -> u.copy(children = f(u.children))
        is Ui.Form -> u.copy(fields = f(u.fields))
        is Ui.Items -> u.copy(items = f(u.items))
        else -> u
    }

    private fun List<Ui>.set(i: Int, f: (Ui) -> Ui): List<Ui> =
        if (i < 0 || i >= size) this else mapIndexed { j, u -> if (j == i) f(u) else u }

    // ----------------------------------------------------------- hybrid

    /** every Form on the tree: its key, and its fields' keys */
    fun forms(u: Ui): Map<String, Set<String>> = when (u) {
        is Ui.Row -> u.children.flatMap { forms(it).entries }.associate { it.key to it.value }
        is Ui.Column -> u.children.flatMap { forms(it).entries }.associate { it.key to it.value }
        is Ui.Box -> u.children.flatMap { forms(it).entries }.associate { it.key to it.value }
        is Ui.Scroll -> forms(u.child)
        is Ui.Form -> u.fields.flatMap { forms(it).entries }.associate { it.key to it.value } +
            (u.key to u.fields.flatMap { keys(it) }.toSet())
        is Ui.Items -> u.items.flatMap { forms(it).entries }.associate { it.key to it.value }
        is Ui.Table -> u.rows.flatten().flatMap { forms(it).entries }.associate { it.key to it.value }
        is Ui.Tabs -> u.pages.getOrNull(u.selected)?.let { forms(it) } ?: emptyMap()
        is Ui.Modal -> forms(u.body)
        is Ui.Disclosure -> if (u.open) forms(u.body) else emptyMap()
        else -> emptyMap()
    }

    /** the capability list: every key an event may name */
    fun keys(u: Ui): Set<String> = when (u) {
        is Ui.Row -> u.children.flatMap { keys(it) }.toSet()
        is Ui.Column -> u.children.flatMap { keys(it) }.toSet()
        is Ui.Box -> u.children.flatMap { keys(it) }.toSet()
        is Ui.Scroll -> keys(u.child)
        is Ui.Text, is Ui.Image -> emptySet()
        is Ui.Button -> setOf(u.key)
        is Ui.Input -> setOf(u.key)
        is Ui.Check -> setOf(u.key)
        is Ui.Select -> setOf(u.key)
        is Ui.Form -> u.fields.flatMap { keys(it) }.toSet() + u.key
        is Ui.Items -> u.items.flatMap { keys(it) }.toSet()
        is Ui.Table -> u.rows.flatten().flatMap { keys(it) }.toSet()
        is Ui.Tabs -> u.labels.indices.map { "${u.key}\$tab$it" }.toSet() + (u.pages.getOrNull(u.selected)?.let { keys(it) } ?: emptySet())
        is Ui.Modal -> keys(u.body)
        is Ui.Disclosure -> (if (u.open) keys(u.body) else emptySet()) + u.key
    }

    /** the widgets in tab order — the interactive leaves */
    fun focusable(u: Ui): List<Ui> = when (u) {
        is Ui.Row -> u.children.flatMap { focusable(it) }
        is Ui.Column -> u.children.flatMap { focusable(it) }
        is Ui.Box -> u.children.flatMap { focusable(it) }
        is Ui.Scroll -> focusable(u.child)
        is Ui.Text, is Ui.Image -> emptyList()
        is Ui.Button, is Ui.Input, is Ui.Check, is Ui.Select -> listOf(u)
        is Ui.Form -> u.fields.flatMap { focusable(it) } + Ui.Button(u.submit, u.key, Role.Primary)
        is Ui.Items -> u.items.flatMap { focusable(it) }
        is Ui.Table -> u.rows.flatten().flatMap { focusable(it) }
        is Ui.Tabs -> u.labels.mapIndexed { i, l -> Ui.Button(l, "${u.key}\$tab$i", if (i == u.selected) Role.Active else Role.Plain) } +
            (u.pages.getOrNull(u.selected)?.let { focusable(it) } ?: emptyList())
        is Ui.Modal -> focusable(u.body)
        is Ui.Disclosure -> listOf(Ui.Button(u.title, u.key, if (u.open) Role.Active else Role.Plain)) + (if (u.open) focusable(u.body) else emptyList())
    }

    /** a bottom-up rewrite of every node */
    fun map(u: Ui, f: (Ui) -> Ui): Ui = f(
        when (u) {
            is Ui.Row -> u.copy(children = u.children.map { map(it, f) })
            is Ui.Column -> u.copy(children = u.children.map { map(it, f) })
            is Ui.Box -> u.copy(children = u.children.map { map(it, f) })
            is Ui.Scroll -> u.copy(child = map(u.child, f))
            is Ui.Form -> u.copy(fields = u.fields.map { map(it, f) })
            is Ui.Items -> u.copy(items = u.items.map { map(it, f) })
            is Ui.Table -> u.copy(rows = u.rows.map { r -> r.map { map(it, f) } })
            is Ui.Tabs -> u.copy(pages = u.pages.map { map(it, f) })
            is Ui.Modal -> u.copy(body = map(u.body, f))
            is Ui.Disclosure -> u.copy(body = map(u.body, f))
            else -> u
        },
    )

    /**
     * The hybrid rule, on the client: an event that stays local answers
     * the tree it changes; null means "send it". This client claims no
     * semantic node, so only the Form's fields are local here.
     */
    fun foldLocal(tree: Ui, e: Event): Ui? {
        val fieldOf = forms(tree).flatMap { (f, ks) -> ks.map { it to f } }.toMap()
        fun live(k: String) = focusable(tree).any { it is Ui.Input && it.key == k && it.live }
        return when (e) {
            is Event.Edited -> if (fieldOf.containsKey(e.key) && !live(e.key))
                map(tree) { u -> if (u is Ui.Input && u.key == e.key) u.copy(value = e.value) else u } else null
            is Event.Toggled -> if (fieldOf.containsKey(e.key))
                map(tree) { u -> if (u is Ui.Check && u.key == e.key) u.copy(on = e.on) else u } else null
            is Event.Chosen -> if (fieldOf.containsKey(e.key))
                map(tree) { u -> if (u is Ui.Select && u.key == e.key) u.copy(selected = e.index) else u } else null
            else -> null
        }
    }

    /** the Form's one event, from the values its fields hold now */
    fun submit(tree: Ui, formKey: String): Event? {
        var found: Ui.Form? = null
        map(tree) { u -> if (u is Ui.Form && u.key == formKey && found == null) found = u; u }
        val f = found ?: return null
        return Event.Submitted(
            formKey,
            f.fields.flatMap { focusable(it) }.mapNotNull {
                when (it) {
                    is Ui.Input -> Event.Edited(it.key, it.value)
                    is Ui.Check -> Event.Toggled(it.key, it.on)
                    is Ui.Select -> Event.Chosen(it.key, it.selected)
                    else -> null
                }
            },
        )
    }

    /** what the client sends for a host event: the local fold, the
     * Form's submit, or the event itself */
    fun outbound(tree: Ui, e: Event): Pair<Ui, Event?> {
        foldLocal(tree, e)?.let { return it to null }
        if (e is Event.Pressed && forms(tree).containsKey(e.key)) return tree to submit(tree, e.key)
        return tree to e
    }
}
