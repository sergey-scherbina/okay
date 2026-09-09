// Level L, drawn with SwiftUI. The tree is the state; every widget
// reads its value from the tree and reports its event to `act`, which
// applies the hybrid rule and re-renders — the same loop the browser's
// live.js, okay's own Wire.client and the Compose client run.
import SwiftUI
import OkayProtocol

public struct Render: View {
    let u: Ui
    let act: (Event) -> Void
    public init(_ u: Ui, act: @escaping (Event) -> Void) { self.u = u; self.act = act }

    public var body: some View {
        switch u {
        case let .text(s, st):
            Text(s)
                .fontWeight(st.bold || st.tone == .emphasis ? .bold : .regular)
                .font(st.size == .small ? .caption : st.size == .large ? .title2 : .body)
                .foregroundColor(st.tone == .danger ? .red : (st.dim || st.tone == .muted) ? .secondary : .primary)
        case let .row(c, _):
            HStack(alignment: .center, spacing: 4) { ForEach(Array(c.enumerated()), id: \.offset) { Render($0.element, act: act) } }
        case let .column(c, _):
            VStack(alignment: .leading, spacing: 4) { ForEach(Array(c.enumerated()), id: \.offset) { Render($0.element, act: act) } }
        case let .box(c, d, _, g, p, _):
            // weights are SwiftUI's natural sizes here; gap and pad are character units, ~8pt
            if d == .horizontal {
                HStack(spacing: CGFloat(g * 8)) { ForEach(Array(c.enumerated()), id: \.offset) { Render($0.element, act: act) } }.padding(CGFloat(p * 8))
            } else {
                VStack(alignment: .leading, spacing: CGFloat(g * 8)) { ForEach(Array(c.enumerated()), id: \.offset) { Render($0.element, act: act) } }.padding(CGFloat(p * 8))
            }
        case let .image(_, alt):
            Text("[image: \(alt)]")   // a src is a URL the client has no loader for yet
        case let .button(l, k, r):
            Button(l) { act(.pressed(key: k)) }
                .buttonStyle(.bordered)
                .tint(r == .danger ? .red : r == .primary ? .accentColor : nil)
                .fontWeight(r == .active ? .bold : .regular)
        case let .input(v, k, l, kind, _):
            let binding = Binding(get: { v }, set: { act(.edited(key: k, value: $0)) })
            VStack(alignment: .leading, spacing: 2) {
                if !l.isEmpty { Text(l).font(.caption) }
                if kind == .secret { SecureField(l, text: binding).textFieldStyle(.roundedBorder) }
                else if kind == .multiline { TextEditor(text: binding).frame(minHeight: 88) }
                else { TextField(l, text: binding).textFieldStyle(.roundedBorder) }
            }
        case let .check(on, k, l):
            Toggle(l, isOn: Binding(get: { on }, set: { act(.toggled(key: k, on: $0)) }))
        case let .select(o, s, k):
            Picker("", selection: Binding(get: { s }, set: { act(.chosen(key: k, index: $0)) })) {
                ForEach(Array(o.enumerated()), id: \.offset) { Text($0.element).tag($0.offset) }
            }
        case let .scroll(c, _):
            ScrollView { Render(c, act: act) }
        case let .form(fs, s, k):
            VStack(alignment: .leading, spacing: 8) {
                ForEach(Array(fs.enumerated()), id: \.offset) { Render($0.element, act: act) }
                Render(.button(label: s, key: k, role: .primary), act: act)
            }
        // level S never arrives: this client claims nothing, the server lowers
        case let .items(i, _):
            VStack(alignment: .leading) { ForEach(Array(i.enumerated()), id: \.offset) { Render($0.element, act: act) } }
        case let .table(h, rows, _):
            VStack(alignment: .leading) {
                HStack { ForEach(Array(h.enumerated()), id: \.offset) { Text($0.element).bold() } }
                ForEach(Array(rows.enumerated()), id: \.offset) { r in HStack { ForEach(Array(r.element.enumerated()), id: \.offset) { Render($0.element, act: act) } } }
            }
        case let .tabs(l, s, p, k):
            VStack(alignment: .leading) {
                HStack { ForEach(Array(l.enumerated()), id: \.offset) { Render(.button(label: $0.element, key: "\(k)$tab\($0.offset)", role: $0.offset == s ? .active : .plain), act: act) } }
                if s < p.count { Render(p[s], act: act) }
            }
        case let .modal(t, b, _):
            VStack(alignment: .leading) { Text(t).bold(); Render(b, act: act) }.padding(8)
        case let .disclosure(t, o, b, k):
            VStack(alignment: .leading) {
                Render(.button(label: t, key: k, role: o ? .active : .plain), act: act)
                if o { Render(b, act: act) }
            }
        }
    }
}

/// the whole client as a view: connect on appear, draw the tree, close on disappear
public struct OkayApp: View {
    @State private var tree: Ui = .text(s: "connecting…", style: Style())
    @State private var client: Client? = nil
    let url: URL
    public init(url: URL) { self.url = url }
    public var body: some View {
        Render(tree) { e in client?.act(e) }
            .onAppear {
                let c = Client(url: url, onTree: { t in DispatchQueue.main.async { tree = t } },
                               onClosed: { DispatchQueue.main.async { tree = .text(s: "connection closed", style: Style()) } })
                client = c
                c.connect()
            }
            .onDisappear { client?.close() }
    }
}
