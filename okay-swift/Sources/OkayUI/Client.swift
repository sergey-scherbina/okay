// The wire: a WebSocket to an okay-script Live page (or anything that
// serves the protocol), hello first, lines in and out. Foundation's
// URLSessionWebSocketTask — no dependency. Events before the socket
// opens are queued, not dropped.
import Foundation
import OkayProtocol

public final class Client: NSObject, URLSessionWebSocketDelegate {
    public private(set) var tree: Ui = .text(s: "connecting…", style: Style())
    private var task: URLSessionWebSocketTask?
    private var open = false
    private var queue: [Msg] = []
    private let onTree: (Ui) -> Void
    private let onClosed: () -> Void
    private let lock = NSLock()

    public init(url: URL, onTree: @escaping (Ui) -> Void, onClosed: @escaping () -> Void) {
        self.onTree = onTree; self.onClosed = onClosed
        super.init()
        let session = URLSession(configuration: .default, delegate: self, delegateQueue: nil)
        task = session.webSocketTask(with: url)
    }

    public func connect() {
        task?.resume()
        receive()
    }

    public func urlSession(_ session: URLSession, webSocketTask: URLSessionWebSocketTask, didOpenWithProtocol protocol: String?) {
        lock.lock()
        open = true
        let pending = queue; queue = []
        lock.unlock()
        raw(.hello(vocab: [], version: protocolVersion))
        pending.forEach(raw)
    }

    public func urlSession(_ session: URLSession, webSocketTask: URLSessionWebSocketTask, didCloseWith closeCode: URLSessionWebSocketTask.CloseCode, reason: Data?) {
        onClosed()
    }

    private func receive() {
        task?.receive { [weak self] result in
            guard let self = self else { return }
            switch result {
            case .failure: self.onClosed()
            case let .success(message):
                if case let .string(line) = message { self.line(line) }
                self.receive()
            }
        }
    }

    /// a server line: a tree or a patch lands on the kept tree; damage is dropped
    private func line(_ s: String) {
        switch Wire.parse(s) {
        case let .tree(u)?: tree = u
        case let .patch(p)?: tree = Tree.apply(tree, p)
        case .close?: onClosed(); return
        default: return
        }
        onTree(tree)
    }

    /// a host event: the hybrid rule decides whether it crosses
    public func act(_ e: Event) {
        let (t, out) = Tree.outbound(tree, e)
        if t != tree { tree = t; onTree(t) }
        if let o = out { send(.event(event: o)) }
    }

    private func send(_ m: Msg) {
        lock.lock()
        let ready = open
        if !ready { queue.append(m) }
        lock.unlock()
        if ready { raw(m) }
    }

    private func raw(_ m: Msg) {
        task?.send(.string(Wire.line(m))) { _ in }
    }

    public func close() {
        send(.event(event: .closed))
        task?.cancel(with: .normalClosure, reason: nil)
    }
}
