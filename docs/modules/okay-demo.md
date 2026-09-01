# okay-demo

The showcase module — every demo is a real program exercising the
stack end to end, and several double as acceptance tests.

| | |
|---|---|
| `Combine` | the stream-exercise ported from Cats/FS2: `Stage.transduce` and `mapAccumulate` doing the same join in a fraction of the code — the example that extracted those primitives into core |
| `RepoAgent` / `RepoMcp` | this repository indexed by its own lex/parse/rag machinery, served as an agent and as an MCP server on stdio; the test asserts the index finds the library's own definitions |
| `IndexReport` | the index, reported |
| `ChatDemo` | the chat over okay-http + okay-llm + okay-match: streaming through the route, the match tools driven by a live local model where one answers (TestLive skip otherwise), sqlite via the Sql seam |

`run / fork := true` — RepoMcp owns its stdin (an MCP client
launches the class directly; `sbt -batch` keeps stdin for itself).
