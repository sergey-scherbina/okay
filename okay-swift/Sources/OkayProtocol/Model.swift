// The frontend protocol's shapes, in Swift, from docs/protocol/frontend.md.
// No dependency on okay: this file is the document, transcribed. A sum
// is an enum with associated values, a product its fields, an
// enumeration a String-backed enum whose raw value is the wire name.

public enum Dir: String, Equatable { case horizontal = "h", vertical = "v" }
public enum Role: String, Equatable { case plain, primary, danger, active }
public enum InputKind: String, Equatable { case text, secret, multiline, number }
public enum Tone: String, Equatable { case plain, emphasis, muted, danger }
public enum Size: String, Equatable { case small, normal, large }

public struct Style: Equatable {
    public var bold: Bool
    public var dim: Bool
    public var tone: Tone
    public var size: Size
    public init(bold: Bool = false, dim: Bool = false, tone: Tone = .plain, size: Size = .normal) {
        self.bold = bold; self.dim = dim; self.tone = tone; self.size = size
    }
}

/// the tree: level L (every client draws it) and level S (claimed, or lowered by the server)
public indirect enum Ui: Equatable {
    case text(s: String, style: Style)
    case row(children: [Ui], key: String)
    case column(children: [Ui], key: String)
    case box(children: [Ui], dir: Dir, weights: [Int], gap: Int, pad: Int, key: String)
    case image(src: String, alt: String)
    case button(label: String, key: String, role: Role)
    case input(value: String, key: String, label: String, kind: InputKind, live: Bool)
    case check(on: Bool, key: String, label: String)
    case select(options: [String], selected: Int, key: String)
    case scroll(child: Ui, key: String)
    /// level L too: the hybrid rule lives on it
    case form(fields: [Ui], submit: String, key: String)
    // level S — this client claims none of them; they arrive lowered
    case items(items: [Ui], key: String)
    case table(header: [String], rows: [[Ui]], key: String)
    case tabs(labels: [String], selected: Int, pages: [Ui], key: String)
    case modal(title: String, body: Ui, key: String)
    case disclosure(title: String, open: Bool, body: Ui, key: String)
}

public indirect enum Event: Equatable {
    case pressed(key: String)
    case edited(key: String, value: String)
    case toggled(key: String, on: Bool)
    case chosen(key: String, index: Int)
    case key(ch: String)
    case resized(w: Int, h: Int)
    case closed
    /// a Form's one event: the field edits, sent together
    case submitted(key: String, edits: [Event])
}

public enum Patch: Equatable {
    case replace(path: [Int], ui: Ui)
    case setText(path: [Int], s: String)
    case setValue(path: [Int], s: String)
    case setChecked(path: [Int], on: Bool)
    case setSelected(path: [Int], index: Int)
    case remove(path: [Int], index: Int)
    case reorder(path: [Int], order: [Int])
    case insert(path: [Int], index: Int, ui: Ui)
}

/// the envelope: every line on the wire is one of these
public enum Msg: Equatable {
    case hello(vocab: [String], version: Int)
    case tree(ui: Ui)
    case patch(patch: Patch)
    case event(event: Event)
    case close
}

public let protocolVersion = 1
