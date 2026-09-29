## delta-jdk-skip - TestDelta says it cannot run on JDK 24+ instead of failing four tests

delta-kernel resolves paths through Hadoop, which calls
`Subject.getSubject`, unsupported from JDK 24 (JEP 486). build.sbt pins the
suite's fork to a JDK 21 where the box has one. Where it has none (a cloud
container, CI on a newer JDK) the fork ran on the ambient JDK and all four
tests failed with "getSubject is not supported", indistinguishable in a
whole-build gate from a defect. The suite now ignores itself on JDK 24+
and prints why. On the operator's Mac, with JDK 21 installed, nothing
changes. With this and intent-model-reproducible, the two reds a whole
`affected` gate carried from the environment are gone.
