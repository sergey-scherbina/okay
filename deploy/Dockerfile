# The deploy scaffold's build (specs/deploy.md): ARG-selected, so
# one Dockerfile serves every Okay service — a new one adds two
# build.sbt lines (assembly/mainClass, a stable assemblyJarName)
# and reuses this file unchanged.
#
#   docker build --build-arg MODULE=okayDemo -t okay/demo:local -f deploy/Dockerfile .
#
# or, simpler, deploy/scripts/okay-package.sh <module> [tag].

ARG SBT_IMAGE=sbtscala/scala-sbt:eclipse-temurin-21.0.5_11_1.10.7_3.5.2
ARG JRE_IMAGE=eclipse-temurin:21-jre-alpine

FROM ${SBT_IMAGE} AS build
ARG MODULE
WORKDIR /src
# dependency layer cached separately from source changes
COPY project project
COPY build.sbt .
RUN sbt update
COPY . .
RUN sbt "${MODULE}/assembly"
# the convention (specs/deploy.md): every scaffold-ready module sets
# `assembly / assemblyJarName := "app.jar"`, so a clean build tree
# has exactly one — the module just built
RUN cp "$(find . -path '*/target/scala-*/app.jar')" /app.jar

FROM ${JRE_IMAGE}
RUN addgroup -S okay && adduser -S okay -G okay
COPY --from=build /app.jar /app/app.jar
USER okay
WORKDIR /app
# the app picks its own port from its own env var (OKAY_CHAT_PORT for
# DemoChat, e.g.); EXPOSE is documentation, not enforcement
EXPOSE 8080
ENTRYPOINT ["java", "-jar", "/app/app.jar"]
