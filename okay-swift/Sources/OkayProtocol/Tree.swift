// What a client does with the tree: apply the server's patches (paths
// index children in order — docs/protocol/frontend.md), and the HYBRID
// rule: a Form's fields fold here, its button submits once, a live
// input speaks per change, everything else crosses the wire.
//
// On the Scala side each of these has a name in the optics
// (specs/optics.md stage 2), and porting one is easier knowing which:
//   apply(_:_:)  ~ Ui.path(patch.path)  — the affine at an index path
//   map(_:_:)    ~ Ui.everywhere        — the traversal over every node
//   keys / forms ~ Ui.shown             — only what is on screen
//   the key work ~ Ui.key(k)            — every node a key names
// A port needs none of that machinery; the names are the map.

public enum Tree {

    // ------------------------------------------------------------ patch

    public static func apply(_ tree: Ui, _ p: Patch) -> Ui {
        switch p {
        case let .replace(path, u): return at(tree, path) { _ in u }
        case let .setText(path, s): return at(tree, path) { u in if case let .text(_, st) = u { return .text(s: s, style: st) }; return u }
        case let .setValue(path, s): return at(tree, path) { u in
            if case let .input(_, k, l, kind, live) = u { return .input(value: s, key: k, label: l, kind: kind, live: live) }; return u }
        case let .setChecked(path, on): return at(tree, path) { u in if case let .check(_, k, l) = u { return .check(on: on, key: k, label: l) }; return u }
        case let .setSelected(path, i): return at(tree, path) { u in if case let .select(o, _, k) = u { return .select(options: o, selected: i, key: k) }; return u }
        case let .remove(path, i): return at(tree, path) { u in kids(u) { c in c.enumerated().filter { $0.offset != i }.map { $0.element } } }
        case let .reorder(path, order): return at(tree, path) { u in kids(u) { c in order.map { c[$0] } } }
        case let .insert(path, i, new): return at(tree, path) { u in kids(u) { c in var c = c; c.insert(new, at: i); return c } }
        }
    }

    static func at(_ u: Ui, _ path: [Int], _ f: (Ui) -> Ui) -> Ui {
        guard let i = path.first else { return f(u) }
        let rest = Array(path.dropFirst())
        switch u {
        case let .row(c, k): return .row(children: set(c, i) { at($0, rest, f) }, key: k)
        case let .column(c, k): return .column(children: set(c, i) { at($0, rest, f) }, key: k)
        case let .box(c, d, w, g, p, k): return .box(children: set(c, i) { at($0, rest, f) }, dir: d, weights: w, gap: g, pad: p, key: k)
        case let .scroll(c, k): return i == 0 ? .scroll(child: at(c, rest, f), key: k) : u
        case let .form(fs, s, k): return .form(fields: set(fs, i) { at($0, rest, f) }, submit: s, key: k)
        case let .items(it, k): return .items(items: set(it, i) { at($0, rest, f) }, key: k)
        case let .modal(t, b, k): return i == 1 ? .modal(title: t, body: at(b, rest, f), key: k) : u
        case let .disclosure(t, o, b, k): return i == 1 ? .disclosure(title: t, open: o, body: at(b, rest, f), key: k) : u
        default: return u   // a path into a leaf: the server never makes one
        }
    }

    static func kids(_ u: Ui, _ f: ([Ui]) -> [Ui]) -> Ui {
        switch u {
        case let .row(c, k): return .row(children: f(c), key: k)
        case let .column(c, k): return .column(children: f(c), key: k)
        case let .box(c, d, w, g, p, k): return .box(children: f(c), dir: d, weights: w, gap: g, pad: p, key: k)
        case let .form(fs, s, k): return .form(fields: f(fs), submit: s, key: k)
        case let .items(it, k): return .items(items: f(it), key: k)
        default: return u
        }
    }

    static func set(_ xs: [Ui], _ i: Int, _ f: (Ui) -> Ui) -> [Ui] {
        guard i >= 0 && i < xs.count else { return xs }
        var out = xs; out[i] = f(xs[i]); return out
    }

    // ----------------------------------------------------------- hybrid

    /// every Form on the tree: its key, and its fields' keys
    public static func forms(_ u: Ui) -> [String: Set<String>] {
        func merge(_ xs: [Ui]) -> [String: Set<String>] { xs.reduce(into: [:]) { $0.merge(forms($1)) { a, _ in a } } }
        switch u {
        case let .row(c, _), let .column(c, _), let .box(c, _, _, _, _, _): return merge(c)
        case let .scroll(c, _): return forms(c)
        case let .form(fs, _, k):
            var m = merge(fs); m[k] = Set(fs.flatMap { keys($0) }); return m
        case let .items(i, _): return merge(i)
        case let .table(_, rows, _): return merge(rows.flatMap { $0 })
        case let .tabs(_, s, pages, _): return s < pages.count ? forms(pages[s]) : [:]
        case let .modal(_, b, _): return forms(b)
        case let .disclosure(_, o, b, _): return o ? forms(b) : [:]
        default: return [:]
        }
    }

    /// the capability list: every key an event may name
    public static func keys(_ u: Ui) -> Set<String> {
        switch u {
        case let .row(c, _), let .column(c, _), let .box(c, _, _, _, _, _): return Set(c.flatMap { keys($0) })
        case let .scroll(c, _): return keys(c)
        case .text, .image: return []
        case let .button(_, k, _), let .input(_, k, _, _, _), let .check(_, k, _), let .select(_, _, k): return [k]
        case let .form(fs, _, k): return Set(fs.flatMap { keys($0) }).union([k])
        case let .items(i, _): return Set(i.flatMap { keys($0) })
        case let .table(_, rows, _): return Set(rows.flatMap { $0 }.flatMap { keys($0) })
        case let .tabs(l, s, pages, k):
            let tabs = Set(l.indices.map { "\(k)$tab\($0)" })
            return s < pages.count ? tabs.union(keys(pages[s])) : tabs
        case let .modal(_, b, _): return keys(b)
        case let .disclosure(_, o, b, k): return (o ? keys(b) : []).union([k])
        }
    }

    /// the interactive leaves, in order
    public static func focusable(_ u: Ui) -> [Ui] {
        switch u {
        case let .row(c, _), let .column(c, _), let .box(c, _, _, _, _, _): return c.flatMap { focusable($0) }
        case let .scroll(c, _): return focusable(c)
        case .text, .image: return []
        case .button, .input, .check, .select: return [u]
        case let .form(fs, s, k): return fs.flatMap { focusable($0) } + [.button(label: s, key: k, role: .primary)]
        case let .items(i, _): return i.flatMap { focusable($0) }
        case let .table(_, rows, _): return rows.flatMap { $0 }.flatMap { focusable($0) }
        case let .tabs(l, s, pages, k):
            let bar = l.enumerated().map { Ui.button(label: $0.element, key: "\(k)$tab\($0.offset)", role: $0.offset == s ? .active : .plain) }
            return bar + (s < pages.count ? focusable(pages[s]) : [])
        case let .modal(_, b, _): return focusable(b)
        case let .disclosure(t, o, b, k): return [.button(label: t, key: k, role: o ? .active : .plain)] + (o ? focusable(b) : [])
        }
    }

    /// a bottom-up rewrite of every node
    public static func map(_ u: Ui, _ f: (Ui) -> Ui) -> Ui {
        let inner: Ui
        switch u {
        case let .row(c, k): inner = .row(children: c.map { map($0, f) }, key: k)
        case let .column(c, k): inner = .column(children: c.map { map($0, f) }, key: k)
        case let .box(c, d, w, g, p, k): inner = .box(children: c.map { map($0, f) }, dir: d, weights: w, gap: g, pad: p, key: k)
        case let .scroll(c, k): inner = .scroll(child: map(c, f), key: k)
        case let .form(fs, s, k): inner = .form(fields: fs.map { map($0, f) }, submit: s, key: k)
        case let .items(i, k): inner = .items(items: i.map { map($0, f) }, key: k)
        case let .table(h, rows, k): inner = .table(header: h, rows: rows.map { $0.map { map($0, f) } }, key: k)
        case let .tabs(l, s, p, k): inner = .tabs(labels: l, selected: s, pages: p.map { map($0, f) }, key: k)
        case let .modal(t, b, k): inner = .modal(title: t, body: map(b, f), key: k)
        case let .disclosure(t, o, b, k): inner = .disclosure(title: t, open: o, body: map(b, f), key: k)
        default: inner = u
        }
        return f(inner)
    }

    /// the hybrid rule, on the client: an event that stays local answers
    /// the tree it changes; nil means "send it"
    public static func foldLocal(_ tree: Ui, _ e: Event) -> Ui? {
        let fieldOf = forms(tree).reduce(into: [String: String]()) { acc, kv in kv.value.forEach { acc[$0] = kv.key } }
        func live(_ k: String) -> Bool { focusable(tree).contains { if case let .input(_, key, _, _, l) = $0 { return key == k && l }; return false } }
        switch e {
        case let .edited(k, v):
            guard fieldOf[k] != nil, !live(k) else { return nil }
            return map(tree) { u in if case let .input(_, key, l, kind, lv) = u, key == k { return .input(value: v, key: key, label: l, kind: kind, live: lv) }; return u }
        case let .toggled(k, on):
            guard fieldOf[k] != nil else { return nil }
            return map(tree) { u in if case let .check(_, key, l) = u, key == k { return .check(on: on, key: key, label: l) }; return u }
        case let .chosen(k, i):
            guard fieldOf[k] != nil else { return nil }
            return map(tree) { u in if case let .select(o, _, key) = u, key == k { return .select(options: o, selected: i, key: key) }; return u }
        default: return nil
        }
    }

    /// the Form's one event, from the values its fields hold now
    public static func submit(_ tree: Ui, _ formKey: String) -> Event? {
        var found: [Ui]? = nil
        _ = map(tree) { u in if case let .form(fs, _, k) = u, k == formKey, found == nil { found = fs }; return u }
        guard let fields = found else { return nil }
        let edits: [Event] = fields.flatMap { focusable($0) }.compactMap {
            switch $0 {
            case let .input(v, k, _, _, _): return .edited(key: k, value: v)
            case let .check(on, k, _): return .toggled(key: k, on: on)
            case let .select(_, i, k): return .chosen(key: k, index: i)
            default: return nil
            }
        }
        return .submitted(key: formKey, edits: edits)
    }

    /// what the client sends for a host event: the local fold, the Form's submit, or the event itself
    public static func outbound(_ tree: Ui, _ e: Event) -> (Ui, Event?) {
        if let t = foldLocal(tree, e) { return (t, nil) }
        if case let .pressed(k) = e, forms(tree)[k] != nil { return (tree, submit(tree, k)) }
        return (tree, e)
    }
}
