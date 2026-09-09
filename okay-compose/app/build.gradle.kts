// the Compose client: level L drawn natively, over a WebSocket. Desktop
// today (Compose Multiplatform's desktop target — the same composables
// are the Android target's once an SDK is on the machine; see
// specs/frontend.md stage 3).
plugins {
    kotlin("jvm") version "2.1.20"
    kotlin("plugin.compose") version "2.1.20"
    id("org.jetbrains.compose") version "1.7.3"
}

kotlin { jvmToolchain(21) }

dependencies {
    implementation(project(":protocol"))
    implementation(compose.desktop.currentOs)
    implementation(compose.material)
}

compose.desktop {
    application {
        mainClass = "okay.compose.app.MainKt"
    }
}

// the headless smoke: connect to a live server, press, read the patch
tasks.register<JavaExec>("smoke") {
    group = "verification"
    classpath = sourceSets["main"].runtimeClasspath
    mainClass.set("okay.compose.app.Smoke")
}
