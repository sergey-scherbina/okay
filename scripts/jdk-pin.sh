# jdk-pin.sh — sourced, not run: pin the JDK that launches sbt to what
# .sdkmanrc declares. Shared by gate.sh and jmh-lane.sh (jmh-lane-jdk-pin,
# 2026-09-25): jmh-lane.sh ran a bare `sbt` on the PATH's java — sdkman's
# global `current`, JDK 17 on this box — and dotc on 17 cannot compile a
# `versioned` Multi-Release variant (`22 is not a valid choice for
# -java-output-version`), so every JMH lane of the core failed at once.
# One copy of the rule, not one per script (policy P-6). JDK_PIN_ROOT is
# the checkout holding .sdkmanrc; unset, the current directory (gate.sh).
#
# PIN THE AMBIENT JDK TO WHAT .sdkmanrc DECLARES (jdk-local-default,
# 2026-09-22), without touching sdkman's global `current` — the
# ambient JVM is what launches sbt and therefore what compiles
# everything (in-process dotc; specs/jdk-compatibility.md), and it is
# controlled by the SHELL's PATH, not by anything in build.sbt. A
# machine whose global default has drifted to the wrong major (17 was
# seen mid-session: JEP 444 virtual-thread APIs vanish, `Thread
# .startVirtualThread`/`.ofVirtual` fail with "is not a member of
# object Thread") should still build correctly from this repo, so
# every sbt invocation resolves its OWN JDK from the project's own
# pin rather than trusting whatever the box's default happens to be
# right now. Silently does nothing if sdkman or the pinned candidate
# is absent — this is a convenience, not a hard requirement.
if [ "${GATE_JDK_AUTOPIN:-1}" != "0" ]; then
  sdk_java="$(sed -n 's/^java=//p' "${JDK_PIN_ROOT:-.}/.sdkmanrc" 2>/dev/null | head -1)"
  sdk_java_home="$HOME/.sdkman/candidates/java/${sdk_java}"
  if [ -n "$sdk_java" ] && [ -d "$sdk_java_home" ]; then
    export JAVA_HOME="$sdk_java_home"
    export PATH="$JAVA_HOME/bin:$PATH"
  fi
fi
