// A headless smoke of the wire, no window: connect, print the tree,
// press a key, print what the server patched, close.
//   swift run okay-smoke "ws://127.0.0.1:8080/counter?__live=counter" inc
import Foundation
import OkayProtocol
import OkayUI

let url = URL(string: CommandLine.arguments.count > 1 ? CommandLine.arguments[1] : "ws://127.0.0.1:8080/counter?__live=counter")!
let press = CommandLine.arguments.count > 2 ? CommandLine.arguments[2] : "inc"
var trees: [Ui] = []
let got = DispatchSemaphore(value: 0)
let client = Client(url: url, onTree: { t in trees.append(t); got.signal() }, onClosed: {})
client.connect()
if got.wait(timeout: .now() + 10) == .timedOut {
    print("SMOKE FAIL: no tree within 10s from \(url)"); exit(1)
}
print("tree: " + Wire.line(.tree(ui: trees[0])))
print("keys: \(Tree.keys(trees[0]).sorted())")
client.act(.pressed(key: press))
if got.wait(timeout: .now() + 10) == .timedOut {
    print("SMOKE FAIL: no patch after pressing '\(press)'"); exit(1)
}
print("after: " + Wire.line(.tree(ui: trees[trees.count - 1])))
client.close()
if trees[0] != trees[trees.count - 1] { print("SMOKE OK: the press changed the tree"); exit(0) }
print("SMOKE FAIL: the tree did not change"); exit(1)
