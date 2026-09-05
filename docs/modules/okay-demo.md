# okay-demo

Building something like this yourself, from an empty directory and
outside this repository, is [Building a chat
application](../building-a-chat-app.md) — the same route, the same UI
construction, at a size you can read in one sitting.

The showcase module — every demo is a real program exercising the
stack end to end, and several double as acceptance tests.

| | |
|---|---|
| `Combine` | the stream-exercise ported from Cats/FS2: `Stage.transduce` and `mapAccumulate` doing the same join in a fraction of the code — the example that extracted those primitives into core |
| `RepoAgent` / `RepoMcp` | this repository indexed by its own lex/parse/rag machinery, served as an agent and as an MCP server on stdio; the test asserts the index finds the library's own definitions |
| `IndexReport` | the index, reported |
| `ChatDemo` | the chat over okay-http + okay-llm: streaming through the route, and a shared task board (`Board`) the model drives through tools — log-first, so `POST /admin/replay` derives it again from the durable log. Composed, not monolithic — okay-chat (the route and model seam), okay-admin (protected `/admin/replay`), okay-subscription (the pay gate), okay-live (Hub/Registry), okay-ops (health/metrics) and okay-deploy (its own committed deployment) are all extracted modules `ChatDemo.routes` wires together with `orElse` |

`run / fork := true` — RepoMcp owns its stdin (an MCP client
launches the class directly; `sbt -batch` keeps stdin for itself).

## ChatDemo — the chat that runs the stack

One JVM main serves a chat page and, behind it, most of the
repository at once: okay-jetty streams the reply live, okay-llm
speaks the model, Cut guards the stream, okay-ui renders on a real
React, `Board` keeps a shared task list, and the
whole thing works — tested end to end — with NO model and NO build
step, because the offline mode is the demo, not a mock of it.
The design record is [specs/demo-chat.md](../../specs/demo-chat.md).

### Running it

```bash
sbt okayChatWebJS/fastLinkJS          # optional: the React frontend
sbt okayDemo/run                      # scripted model — always works
OKAY_CHAT_BASE=http://127.0.0.1:8089 sbt okayDemo/run   # any OpenAI-compatible model
ANTHROPIC_API_KEY=... sbt okayDemo/run                  # Anthropic
# → http://127.0.0.1:8090
```

| env | meaning | default |
|---|---|---|
| `OKAY_CHAT_PORT` | where to listen | 8090 |
| `OKAY_CHAT_MAX` | the Cut token budget | 512 |
| `OKAY_CHAT_DB` | the board's log: a FileStore directory; `:memory:` keeps nothing | `okay-board.log` |
| `OKAY_CHAT_LOG` | the two-node lane's shared log directory — a follower polls it and derives its own board from it | `okay-chat.log` |
| `OKAY_CHAT_BASE` | an OpenAI-compatible endpoint (local models fit) | — |
| `POST /login` `{email}` | mints a one-time 6-digit code (demo-sessions; no email transport exists yet, so it rides the response AND the server console) | — |
| `POST /login/confirm` `{email,code}` | spends the code once, answers `{token}` — a signed ES256 session; present it as `Authorization: Bearer` on `/chat` | — |
| `POST /admin/replay` | protected (okay-admin): needs `Authorization: Bearer <admin token>`; the token is printed to the server console once at startup | — |
| `ANTHROPIC_API_KEY` | the Anthropic model | — |
| `OKAY_CHAT_APP` | path to the linked frontend | auto-discovered |
| `GET /healthz` / `/readyz` / `/stats` / `/metrics` | okay-ops's routes, wired in unchanged (docs/modules/okay-ops.md) | — |

### The pieces and who does what

`ChatDemo.routes` composes several extracted modules with `orElse`;
each owns its own doc, so this section says only what DemoChat asks
of each, not how each works internally.

- **Streaming and the model** (okay-chat): `Chat.chatRoute` answers
  `POST /chat` — okay-jetty writes the body chunk by chunk on a
  virtual thread as tokens arrive; `Cut.checked` stands in the stream
  with the token budget, and over it generation is CUT mid-sentence
  (the page renders "✂ generation cut"). `Chat.Model` has three
  fillings (`scripted`/`local`/`live`); a `turnOverride` lets
  `ChatDemo` intercept a `/match`-prefixed message for its own
  board turn while everything else rides the plain model.
  Details: docs/modules/okay-chat.md.
- **Protected admin** (okay-admin): `Admin.routes` answers `POST
  /admin/replay` behind a bearer token — `Admin.Issuer` mints the
  in-process one this demo uses; `main()` prints it once at startup,
  and `/board`'s replay button sends it as `Authorization: Bearer`.
  Details: docs/modules/okay-admin.md.
- **The pay gate** (okay-subscription): free for a profile's join
  month, gated (never deleted) after unless `Subscription.pay` was
  called this period — wired into `find_candidates`/`facts_assert`
  and offered to the model as the `subscription_pay` tool.
  Details: docs/modules/okay-subscription.md.
- **Live feeds** (okay-live): `Hub[String]` is the market-wide
  broadcast behind `/events/board`; `Registry[String, String]` is
  the per-email inbox behind `/events/<email>`. Details:
  docs/modules/okay-live.md.
- **Health and metrics** (okay-ops): `Ops.routes(chatStore)` answers
  `/healthz`, `/readyz`, `/stats`, `/metrics` over the chat log's
  underlying persist `Store`. Details: docs/modules/okay-ops.md.
- **Deployment** (okay-deploy): `DemoDeploy.spec` is the value,
  `okay-demo/deploy/` its committed rendering (Dockerfile,
  compose.yaml, helm/) — regenerate with `sbt "okayDemo/runMain
  okay.demo.DemoDeploy"`, `TestDemoDeploy` refuses drift; build the
  jar/image with `okay-deploy/bin/okay-package.sh okayDemo okay-demo`.
  Details: docs/modules/okay-deploy.md.
- **Two page faces**: the vanilla page (inline HTML+JS, zero build)
  and the React page (okay-ui's tree rendered by CDN React UMD
  through `ReactJs`; the chat's brain is `okay-demo/web`'s pure
  `view`/`update`, JVM-tested; the Elm fold runs on `runAsync`).
- **Sessions** (demo-sessions, `okay.demo.Login`): a `/match` turn
  under a verified session is asserted under THAT email, not
  whatever "email x@y" the message text says — `POST /login` then
  `/login/confirm` mints an ES256 token (one in-process key pair; a
  restart signs everyone out); the vanilla page's login widget stores
  it and sends `Authorization: Bearer` on every `/chat` call. No
  email transport exists yet, so the confirm code rides the response.
- **The board**: one shared `Board` per server, arriving as a context
  parameter rather than reached for as a global — `main` wires the
  durable one, a test wires a memory one, and the same `routes` value
  serves both. The log is the truth and the board is a projection:
  `POST /admin/replay` drops it and derives it again.

### How the model runs the board

With a model configured there is NO gate: every turn is an agent turn
(okay-agent's `converse` over `Provider.openAi`/`anthropic`), the
board's tool table is always offered, and the system prompt hands the
DECISION to the model — a sentence about a task means work the tools,
anything else means just answer. The model never writes to the board:
there is no path from a sentence to the projection that does not go
through a tool, so it cannot invent a task that is not there.

The notification layer is STRUCTURAL, not the model's: the tool
table is wrapped —

- `facts_assert` of an offer runs the reverse search over stored
  needs (and vice versa): the waiting party's page rings the moment
  the counterpart appears, whatever the order of events;
- `match_inquire`/`match_respond` run the negotiation round: the
  asked provider's page rings with the deal number; an acceptance
  hands the seeker the contact THE DEAL just unlocked and stands the
  round's other candidates down; a full-decline round says the
  request still stands;
- `flow_advance` delivers ANY scenario's transition notifications to
  the role-holders' inboxes, templates filled.

Delivery is `GET /events/<email>` — an SSE stream each page holds
open from the first email it sees, rendered as 🔔 bubbles.

### The offline phrases (the no-model driver)

The same tool table, driven by fixed phrasings — so tests and the
keyless demo cover everything the model can do:

| phrase | effect |
|---|---|
| `умею <что>` / `offer: <что>` (+ `email <адрес>`) | store the offer (+ a Matched contact fact) |
| `нужен <кто>` / `need: <что>` | store the need, list candidates NUMBERED |
| `спроси 1 2` / `спроси всех` | inquire the chosen candidates |
| `берусь <N>` / `отказываюсь <N>` | the provider's answer |
| `сценарий <имя> роль=email …` | start any registered flow |
| `шаг <N> <переход>` | fire the writer's transition |
| `флоу <N>` | state and history |

Prefix `/match` forces the driver (and, with a model, is a hint).

### The tests as the tour

`TestChatDemo` (13, over a real socket; live legs skip without a
model): incremental streaming, the cut, the two-sided match, the
reverse chain's two windows, the jobs-domain negotiation round
(decline → accept-with-contact → stand-down), the escrow scenario
by phrases alone, the React page, and the model deciding by itself.
`okay-demo/web`'s brain tests run on the JVM with scripted events —
the React frontend's logic never sees a browser.
