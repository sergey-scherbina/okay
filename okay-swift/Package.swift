// swift-tools-version: 5.9
// okay-swift: the Swift thin client of the frontend protocol
// (specs/frontend.md "Mobile", M2). Depends on NOTHING of okay: it
// implements ../docs/protocol/frontend.md and replays
// ../docs/protocol/conformance.jsonl.
//   swift test                       the conformance proof (macOS)
//   swift run okay-smoke ws://...    headless: connect, press, patch
//   xcodebuild -scheme OkayUI -destination 'generic/platform=iOS Simulator' build
import PackageDescription

let package = Package(
    name: "okay-swift",
    platforms: [.iOS(.v16), .macOS(.v13)],
    products: [
        .library(name: "OkayProtocol", targets: ["OkayProtocol"]),
        .library(name: "OkayUI", targets: ["OkayUI"]),
        .executable(name: "okay-smoke", targets: ["okay-smoke"]),
    ],
    targets: [
        .target(name: "OkayProtocol"),
        .target(name: "OkayUI", dependencies: ["OkayProtocol"]),
        .executableTarget(name: "okay-smoke", dependencies: ["OkayProtocol", "OkayUI"]),
        .testTarget(name: "OkayProtocolTests", dependencies: ["OkayProtocol"]),
    ]
)
