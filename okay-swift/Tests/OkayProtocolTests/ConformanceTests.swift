// The proof that this client speaks the protocol: replay
// docs/protocol/conformance.jsonl (rendered by okay's own TestProtocol)
// — apply every `in`, hold every `tree` it names, produce every `out`.
import XCTest
@testable import OkayProtocol

final class ConformanceTests: XCTestCase {

    func script() throws -> [[String: Any]] {
        var dir = URL(fileURLWithPath: #filePath)
        while dir.path != "/" {
            let f = dir.appendingPathComponent("docs/protocol/conformance.jsonl")
            if FileManager.default.fileExists(atPath: f.path) {
                let text = try String(contentsOf: f, encoding: .utf8)
                return try text.split(separator: "\n").filter { !$0.isEmpty }.map {
                    try JSONSerialization.jsonObject(with: Data($0.utf8)) as! [String: Any]
                }
            }
            dir = dir.deletingLastPathComponent()
        }
        XCTFail("docs/protocol/conformance.jsonl not found above \(#filePath)"); return []
    }

    func testEveryInAppliesAndTheTreeAfterEachEqualsTheOneNamed() throws {
        var tree: Ui? = nil
        var applied = 0
        for r in try script() {
            guard let line = r["in"] else { continue }
            let expected = r["tree"] as! NSDictionary
            switch Wire.readMsg(line) {
            case let .tree(u)?: tree = u
            case let .patch(p)?: tree = Tree.apply(try XCTUnwrap(tree, "a patch before any tree"), p)
            default: XCTFail("unexpected in: \(line)")
            }
            XCTAssertEqual(NSDictionary(dictionary: Wire.ui(tree!)), expected, "after in #\(applied)")
            applied += 1
        }
        XCTAssertGreaterThan(applied, 3)
    }

    func testEveryOutReencodesAndTheHybridProducesTheSubmitted() throws {
        let outs = try script().compactMap { $0["out"] }
        XCTAssertFalse(outs.isEmpty)
        for o in outs {
            let m = try XCTUnwrap(Wire.readMsg(o), "unreadable out: \(o)")
            XCTAssertEqual(NSDictionary(dictionary: Wire.msg(m)), o as! NSDictionary)
        }
        XCTAssertEqual(Wire.readMsg(outs[0]), .hello(vocab: [], version: protocolVersion))
        var submitted: (String, [Event])? = nil
        for o in outs { if case let .event(.submitted(k, edits))? = Wire.readMsg(o) { submitted = (k, edits); break } }
        let (key, edits) = try XCTUnwrap(submitted, "the script carries a Submitted")
        var tree: Ui? = nil
        for r in try script() { if let l = r["in"], case let .tree(u)? = Wire.readMsg(l) { tree = u; break } }
        var t = try XCTUnwrap(tree)
        for edit in edits {
            let (t2, sent) = Tree.outbound(t, edit)
            XCTAssertNil(sent, "a form field edit crossed the wire: \(edit)")
            t = t2
        }
        let (_, out) = Tree.outbound(t, .pressed(key: key))
        XCTAssertEqual(out, .submitted(key: key, edits: edits))
    }

    func testDamageIsNilNeverAThrow() {
        XCTAssertNil(Wire.parse("{ not json"))
        XCTAssertNil(Wire.parse("{\"Tree\":{\"ui\":{\"Nope\":{}}}}"))
        XCTAssertNil(Wire.parse("{\"Tree\":{\"ui\":{\"Text\":{}}}}"))
        XCTAssertEqual(Wire.parse("{\"Close\":{}}"), .close)
    }
}
