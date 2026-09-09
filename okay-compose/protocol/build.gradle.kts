// the protocol, pure: shapes, codec, tree patching, the hybrid rule,
// and the conformance test over docs/protocol/conformance.jsonl
plugins {
    kotlin("jvm") version "2.1.20"
    kotlin("plugin.serialization") version "2.1.20"
}

kotlin { jvmToolchain(21) }

dependencies {
    implementation("org.jetbrains.kotlinx:kotlinx-serialization-json:1.8.0")
    testImplementation(kotlin("test"))
}

tasks.test { useJUnitPlatform() }
