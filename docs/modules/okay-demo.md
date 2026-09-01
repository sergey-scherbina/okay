# okay-demo — the chat that runs the stack

One JVM main serves a chat page and, behind it, most of the
repository at once: okay-jetty streams the reply live, okay-llm
speaks the model, Cut guards the stream, okay-ui renders on a real
React, okay-match runs a marketplace with negotiations, and the
whole thing works — tested end to end — with NO model and NO build
step, because the offline mode is the demo, not a mock of it.
The design record is [specs/demo-chat.md](../../specs/demo-chat.md).

## Running it

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
| `OKAY_CHAT_DB` | sqlite file for the marketplace; `:memory:` opts out | `okay-chat.db` |
| `OKAY_CHAT_BASE` | an OpenAI-compatible endpoint (local models fit) | — |
| `ANTHROPIC_API_KEY` | the Anthropic model | — |
| `OKAY_CHAT_APP` | path to the linked frontend | auto-discovered |

## The pieces and who does what

- **Streaming**: `POST /chat` answers `text/event-stream`;
  okay-jetty writes the body `Source[Chunk[Byte]]` chunk by chunk on
  a virtual thread. The page appends tokens as they arrive.
- **The guard**: `Cut.checked` stands in the token stream with the
  budget; over it, generation is CUT mid-sentence — the page renders
  "✂ generation cut" — and the source records no further pulls.
- **The model seam** (`ChatDemo.Model`): `scripted` (deterministic,
  offline), `local` (any OpenAI-compatible base), `live`
  (Anthropic) — one function type, three fillings.
- **Two faces**: the vanilla page (inline HTML+JS, zero build) and
  the React page (okay-ui's tree rendered by CDN React UMD through
  `ReactJs`; the chat's brain is `okay-demo/web`'s pure
  `view`/`update`, JVM-tested; the Elm fold runs on `runAsync`).
- **The marketplace**: one shared `MatchStore` per server — sqlite
  by default, durable across restarts.

## How the model runs the marketplace

With a model configured there is NO gate: every turn is an agent
turn (okay-agent's `converse` over `Provider.openAi`/`anthropic`),
okay-match's tool table is always offered, and the system prompt
hands the DECISION to the model — an offer or a need (services,
housing, jobs — any domain) means work the tools; anything else
means just answer. Proven live both ways: an offer with no prefix
reaches the tools; small talk leaves the store untouched.

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

## The offline phrases (the no-model driver)

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

## The tests as the tour

`TestChatDemo` (13, over a real socket; live legs skip without a
model): incremental streaming, the cut, the two-sided match, the
reverse chain's two windows, the jobs-domain negotiation round
(decline → accept-with-contact → stand-down), the escrow scenario
by phrases alone, the React page, and the model deciding by itself.
`okay-demo/web`'s brain tests run on the JVM with scripted events —
the React frontend's logic never sees a browser.
