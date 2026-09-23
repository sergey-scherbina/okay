## jvm-languages-guide - one guide for Java streams, Clojure and Frege, and a check that keeps its examples true

docs/jvm-languages.md is a user guide to using okay together with other
JVM languages. It covers the one idea behind the three bridges (a
`Stage` is a gatherer and a transducer; an effect crosses as data in the
other language's own small library, never as lazy IO), each bridge with
its examples, how to choose one, the rules the bridges keep (lazy data
pure by type, one-shot built pipelines, types refused by name at the
seam), the measured costs, the build setup, and the literature. It is
linked from docs/README.md and guide.md.

`TestDocSnippets` (okay-deploy) is new. Every line of every code block in
a listed document must be a verbatim line of a tested Scala, Clojure or
Frege source. A block no test can pin, such as build configuration, is
marked `<!-- not-a-test: … -->`. A check run before writing it found
seven lines on this session's interop pages that no test contained:
examples tidied for the page after the test was written. They are
copied back verbatim now. The list is opt-in, so older guides join as
they are verified.
