// okay-compose: the Compose thin client of the frontend protocol
// (specs/frontend.md stage 3). A Gradle build beside sbt, on purpose:
// it depends on NOTHING of okay — it reads ../docs/protocol/*.
//   cd okay-compose && ./gradlew :protocol:test        the conformance proof
//   cd okay-compose && ./gradlew :app:run --args "ws://127.0.0.1:8080/counter?__live=counter"
pluginManagement {
    repositories {
        gradlePluginPortal()
        google()
        mavenCentral()
    }
}
dependencyResolutionManagement {
    repositories {
        google()
        mavenCentral()
    }
}
rootProject.name = "okay-compose"
include(":protocol", ":app")
