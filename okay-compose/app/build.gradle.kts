// the Compose client: level L drawn natively, over a WebSocket. ONE set
// of composables (commonMain), two targets — desktop (JVM) and Android
// — that differ only in the socket (expect/actual: the JDK's WebSocket
// on the desktop, OkHttp on Android, which has no java.net.http).
// specs/frontend.md stage 3 and "Mobile" M3.
plugins {
    kotlin("multiplatform") version "2.1.20"
    kotlin("plugin.compose") version "2.1.20"
    id("org.jetbrains.compose") version "1.7.3"
    id("com.android.application") version "8.7.3"
}

kotlin {
    jvmToolchain(21)
    jvm("desktop")
    androidTarget()
    // expect/actual CLASSES are "Beta" in Kotlin 2.1; the one here is the socket
    compilerOptions { freeCompilerArgs.add("-Xexpect-actual-classes") }

    sourceSets {
        val commonMain by getting {
            dependencies {
                implementation(project(":protocol"))
                implementation(compose.runtime)
                implementation(compose.foundation)
                implementation(compose.material)
            }
        }
        val desktopMain by getting {
            dependencies { implementation(compose.desktop.currentOs) }
        }
        val androidMain by getting {
            dependencies {
                implementation("androidx.activity:activity-compose:1.9.3")
                implementation("com.squareup.okhttp3:okhttp:4.12.0")
            }
        }
    }
}

android {
    namespace = "okay.compose.app"
    compileSdk = 35
    defaultConfig {
        applicationId = "okay.compose.app"
        minSdk = 26
        targetSdk = 35
        versionCode = 1
        versionName = "0.1"
    }
    compileOptions {
        sourceCompatibility = JavaVersion.VERSION_17
        targetCompatibility = JavaVersion.VERSION_17
    }
}

compose.desktop {
    application {
        mainClass = "okay.compose.app.MainKt"
    }
}

// the headless smoke: connect to a live server, press, read the patch
tasks.register<JavaExec>("smoke") {
    group = "verification"
    val desktop = kotlin.targets.getByName("desktop") as org.jetbrains.kotlin.gradle.targets.jvm.KotlinJvmTarget
    classpath = desktop.compilations.getByName("main").runtimeDependencyFiles +
        desktop.compilations.getByName("main").output.allOutputs
    mainClass.set("okay.compose.app.Smoke")
}
