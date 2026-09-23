# okay-scala2-http

okay-http for **Scala 2.13**. A Scala 2 compiler can read okay-http's
`Request`, `Method` and `Body`, but not its `Response` (the streamed
body's type names the effect row) or its `Route` (Scala 3 generic
tuples). This module supplies what is missing:

| | |
|---|---|
| `Response` | text, html, bytes, JSON and streamed (`lines`) bodies; `status`, `header`, `text`, `bytes` |
| `Routes`, `GET`/`POST`/`PUT`/`PATCH`/`DELETE`, `Path`, `Requests` | routing as Scala 2 pattern matching over a `PartialFunction`; path and query decoded by okay-http's own reader (`okay.http.Urls`) |
| `Server.use` / `Server.start` | serve for the duration of a program, or until `close()` |
| `Client` | `send`, `get`, `post`, `postJson` (body read in full), `lines` (streamed) |

The walkthrough is section 8b of
[okay from Scala 2.13](../scala2.md#8b-http-routes-a-server-a-client), and
the signatures are in [okay-scala2](okay-scala2.md#api-reference).
