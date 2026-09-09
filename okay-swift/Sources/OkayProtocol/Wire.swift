// The JSON codec, by hand and by the document: a sum is an object with
// ONE key (the case name), a product its fields by name, an enumeration
// its lower-case name. Reading is TOTAL — damage is nil, never a
// throw — because a wire that dies on one bad line loses every good
// one after it. Foundation's JSONSerialization, nothing else.
import Foundation

public enum Wire {

    // ------------------------------------------------------------ write

    public static func line(_ m: Msg) -> String {
        let data = try! JSONSerialization.data(withJSONObject: msg(m), options: [.sortedKeys])
        return String(data: data, encoding: .utf8)!
    }

    public static func msg(_ m: Msg) -> [String: Any] {
        switch m {
        case let .hello(vocab, version): return sum("Hello", ["vocab": vocab, "version": version])
        case let .tree(u): return sum("Tree", ["ui": ui(u)])
        case let .patch(p): return sum("Patch", ["patch": patch(p)])
        case let .event(e): return sum("Event", ["event": event(e)])
        case .close: return sum("Close", [:])
        }
    }

    public static func ui(_ u: Ui) -> [String: Any] {
        switch u {
        case let .text(s, st): return sum("Text", ["s": s, "style": style(st)])
        case let .row(c, k): return sum("Row", ["children": c.map(ui), "key": k])
        case let .column(c, k): return sum("Column", ["children": c.map(ui), "key": k])
        case let .box(c, d, w, g, p, k):
            return sum("Box", ["children": c.map(ui), "dir": d.rawValue, "weights": w, "gap": g, "pad": p, "key": k])
        case let .image(src, alt): return sum("Image", ["src": src, "alt": alt])
        case let .button(l, k, r): return sum("Button", ["label": l, "key": k, "role": r.rawValue])
        case let .input(v, k, l, kind, live):
            return sum("Input", ["value": v, "key": k, "label": l, "kind": kind.rawValue, "live": live])
        case let .check(on, k, l): return sum("Check", ["on": on, "key": k, "label": l])
        case let .select(o, s, k): return sum("Select", ["options": o, "selected": s, "key": k])
        case let .scroll(c, k): return sum("Scroll", ["child": ui(c), "key": k])
        case let .form(f, s, k): return sum("Form", ["fields": f.map(ui), "submit": s, "key": k])
        case let .items(i, k): return sum("Items", ["items": i.map(ui), "key": k])
        case let .table(h, r, k): return sum("Table", ["header": h, "rows": r.map { $0.map(ui) }, "key": k])
        case let .tabs(l, s, p, k): return sum("Tabs", ["labels": l, "selected": s, "pages": p.map(ui), "key": k])
        case let .modal(t, b, k): return sum("Modal", ["title": t, "body": ui(b), "key": k])
        case let .disclosure(t, o, b, k): return sum("Disclosure", ["title": t, "open": o, "body": ui(b), "key": k])
        }
    }

    public static func event(_ e: Event) -> [String: Any] {
        switch e {
        case let .pressed(k): return sum("Pressed", ["key": k])
        case let .edited(k, v): return sum("Edited", ["key": k, "value": v])
        case let .toggled(k, on): return sum("Toggled", ["key": k, "on": on])
        case let .chosen(k, i): return sum("Chosen", ["key": k, "index": i])
        case let .key(ch): return sum("Key", ["ch": ch])
        case let .resized(w, h): return sum("Resized", ["w": w, "h": h])
        case .closed: return sum("Closed", [:])
        case let .submitted(k, edits): return sum("Submitted", ["key": k, "edits": edits.map(event)])
        }
    }

    public static func patch(_ p: Patch) -> [String: Any] {
        switch p {
        case let .replace(path, u): return sum("Replace", ["path": path, "ui": ui(u)])
        case let .setText(path, s): return sum("SetText", ["path": path, "s": s])
        case let .setValue(path, s): return sum("SetValue", ["path": path, "s": s])
        case let .setChecked(path, on): return sum("SetChecked", ["path": path, "on": on])
        case let .setSelected(path, i): return sum("SetSelected", ["path": path, "index": i])
        case let .remove(path, i): return sum("Remove", ["path": path, "index": i])
        case let .reorder(path, o): return sum("Reorder", ["path": path, "order": o])
        case let .insert(path, i, u): return sum("Insert", ["path": path, "index": i, "ui": ui(u)])
        }
    }

    static func style(_ s: Style) -> [String: Any] {
        ["bold": s.bold, "dim": s.dim, "tone": s.tone.rawValue, "size": s.size.rawValue]
    }
    static func sum(_ name: String, _ fields: [String: Any]) -> [String: Any] { [name: fields] }

    // ------------------------------------------------------------- read

    public static func parse(_ line: String) -> Msg? {
        guard let data = line.data(using: .utf8),
              let obj = try? JSONSerialization.jsonObject(with: data) else { return nil }
        return readMsg(obj)
    }

    public static func readMsg(_ j: Any) -> Msg? {
        guard let (name, f) = sumOf(j) else { return nil }
        switch name {
        case "Hello":
            guard let v = f["vocab"] as? [String], let ver = int(f["version"]) else { return nil }
            return .hello(vocab: v, version: ver)
        case "Tree": guard let u = readUi(f["ui"]) else { return nil }; return .tree(ui: u)
        case "Patch": guard let p = readPatch(f["patch"]) else { return nil }; return .patch(patch: p)
        case "Event": guard let e = readEvent(f["event"]) else { return nil }; return .event(event: e)
        case "Close": return .close
        default: return nil
        }
    }

    public static func readUi(_ j: Any?) -> Ui? {
        guard let j = j, let (name, f) = sumOf(j) else { return nil }
        func kids(_ field: String) -> [Ui]? {
            guard let arr = f[field] as? [Any] else { return nil }
            var out: [Ui] = []
            for x in arr { guard let u = readUi(x) else { return nil }; out.append(u) }
            return out
        }
        func str(_ k: String) -> String { f[k] as? String ?? "" }
        switch name {
        case "Text":
            guard let s = f["s"] as? String else { return nil }
            return .text(s: s, style: readStyle(f["style"]))
        case "Row": guard let c = kids("children") else { return nil }; return .row(children: c, key: str("key"))
        case "Column": guard let c = kids("children") else { return nil }; return .column(children: c, key: str("key"))
        case "Box":
            guard let c = kids("children"), let d = Dir(rawValue: str("dir")) else { return nil }
            return .box(children: c, dir: d, weights: ints(f["weights"]) ?? [], gap: int(f["gap"]) ?? 0, pad: int(f["pad"]) ?? 0, key: str("key"))
        case "Image": guard let src = f["src"] as? String else { return nil }; return .image(src: src, alt: str("alt"))
        case "Button":
            guard let l = f["label"] as? String else { return nil }
            return .button(label: l, key: str("key"), role: Role(rawValue: str("role")) ?? .plain)
        case "Input":
            guard let v = f["value"] as? String else { return nil }
            return .input(value: v, key: str("key"), label: str("label"), kind: InputKind(rawValue: str("kind")) ?? .text, live: f["live"] as? Bool ?? false)
        case "Check": guard let on = f["on"] as? Bool else { return nil }; return .check(on: on, key: str("key"), label: str("label"))
        case "Select":
            guard let o = f["options"] as? [String] else { return nil }
            return .select(options: o, selected: int(f["selected"]) ?? 0, key: str("key"))
        case "Scroll": guard let c = readUi(f["child"]) else { return nil }; return .scroll(child: c, key: str("key"))
        case "Form":
            guard let fs = kids("fields"), let s = f["submit"] as? String else { return nil }
            return .form(fields: fs, submit: s, key: str("key"))
        case "Items": guard let i = kids("items") else { return nil }; return .items(items: i, key: str("key"))
        case "Table":
            guard let h = f["header"] as? [String], let rs = f["rows"] as? [Any] else { return nil }
            var rows: [[Ui]] = []
            for r in rs {
                guard let cells = r as? [Any] else { return nil }
                var row: [Ui] = []
                for c in cells { guard let u = readUi(c) else { return nil }; row.append(u) }
                rows.append(row)
            }
            return .table(header: h, rows: rows, key: str("key"))
        case "Tabs":
            guard let l = f["labels"] as? [String], let p = kids("pages") else { return nil }
            return .tabs(labels: l, selected: int(f["selected"]) ?? 0, pages: p, key: str("key"))
        case "Modal":
            guard let t = f["title"] as? String, let b = readUi(f["body"]) else { return nil }
            return .modal(title: t, body: b, key: str("key"))
        case "Disclosure":
            guard let t = f["title"] as? String, let b = readUi(f["body"]) else { return nil }
            return .disclosure(title: t, open: f["open"] as? Bool ?? false, body: b, key: str("key"))
        default: return nil
        }
    }

    public static func readEvent(_ j: Any?) -> Event? {
        guard let j = j, let (name, f) = sumOf(j) else { return nil }
        switch name {
        case "Pressed": guard let k = f["key"] as? String else { return nil }; return .pressed(key: k)
        case "Edited": guard let k = f["key"] as? String, let v = f["value"] as? String else { return nil }; return .edited(key: k, value: v)
        case "Toggled": guard let k = f["key"] as? String, let on = f["on"] as? Bool else { return nil }; return .toggled(key: k, on: on)
        case "Chosen": guard let k = f["key"] as? String, let i = int(f["index"]) else { return nil }; return .chosen(key: k, index: i)
        case "Key": guard let ch = f["ch"] as? String else { return nil }; return .key(ch: ch)
        case "Resized": guard let w = int(f["w"]), let h = int(f["h"]) else { return nil }; return .resized(w: w, h: h)
        case "Closed": return .closed
        case "Submitted":
            guard let k = f["key"] as? String, let es = f["edits"] as? [Any] else { return nil }
            var edits: [Event] = []
            for e in es { guard let ev = readEvent(e) else { return nil }; edits.append(ev) }
            return .submitted(key: k, edits: edits)
        default: return nil
        }
    }

    public static func readPatch(_ j: Any?) -> Patch? {
        guard let j = j, let (name, f) = sumOf(j), let path = ints(f["path"]) else { return nil }
        switch name {
        case "Replace": guard let u = readUi(f["ui"]) else { return nil }; return .replace(path: path, ui: u)
        case "SetText": guard let s = f["s"] as? String else { return nil }; return .setText(path: path, s: s)
        case "SetValue": guard let s = f["s"] as? String else { return nil }; return .setValue(path: path, s: s)
        case "SetChecked": guard let on = f["on"] as? Bool else { return nil }; return .setChecked(path: path, on: on)
        case "SetSelected": guard let i = int(f["index"]) else { return nil }; return .setSelected(path: path, index: i)
        case "Remove": guard let i = int(f["index"]) else { return nil }; return .remove(path: path, index: i)
        case "Reorder": guard let o = ints(f["order"]) else { return nil }; return .reorder(path: path, order: o)
        case "Insert": guard let i = int(f["index"]), let u = readUi(f["ui"]) else { return nil }; return .insert(path: path, index: i, ui: u)
        default: return nil
        }
    }

    static func readStyle(_ j: Any?) -> Style {
        guard let f = j as? [String: Any] else { return Style() }
        return Style(bold: f["bold"] as? Bool ?? false, dim: f["dim"] as? Bool ?? false,
                     tone: Tone(rawValue: f["tone"] as? String ?? "") ?? .plain,
                     size: Size(rawValue: f["size"] as? String ?? "") ?? .normal)
    }

    /// the one-key object a sum is
    static func sumOf(_ j: Any) -> (String, [String: Any])? {
        guard let o = j as? [String: Any], o.count == 1, let (k, v) = o.first, let f = v as? [String: Any] else { return nil }
        return (k, f)
    }
    static func int(_ j: Any?) -> Int? {
        if let n = j as? NSNumber { return n.intValue }
        return nil
    }
    static func ints(_ j: Any?) -> [Int]? {
        guard let arr = j as? [Any] else { return nil }
        var out: [Int] = []
        for x in arr { guard let n = int(x) else { return nil }; out.append(n) }
        return out
    }
}
