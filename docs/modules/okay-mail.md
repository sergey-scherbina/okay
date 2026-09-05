# okay-mail

Sending mail: SMTP as a wire, not a driver dependency.

Asked for by a consumer whose service worked and could not have users:
a one-time code to an email address is how a stranger proves the
address is theirs, and until they do, their profile is a draft. Their
delivery was a line in a server log marked development-only.

It is the shape [`okay-pg`](okay-pg.md) already has — a protocol
spoken over a socket with its own TLS rather than a driver — and a
smaller instance of it: SMTP is line-oriented text, STARTTLS runs over
[`okay-tls`](okay-tls.md), and AUTH PLAIN and LOGIN are simpler than
the SCRAM this repository has already written.

**Send only.** Receiving is IMAP or POP, a different and much larger
module, and the consumer who asked argued against bundling it.

| | |
|---|---|
| `Address`, `Mail` | one message: envelope, headers, a UTF-8 body |
| `Mail.Send` | `Mail => Either[Rejection, Accepted]` — an effect, so `main` wires a server and a test wires a recorder |
| `Mail.Rejection` | why it did not go, AS DATA |
| `Mail.Recorder` | the test double, and the development stand-in |
| `Session` | the protocol as a pure function: reply in, next line out |
| `Smtp` | the socket, the TLS upgrade, the timeouts |

```scala
val server = Smtp.Server("smtp.example.com", 587, Address("noreply@example.com"),
  login = Smtp.login("noreply", Secret("env:SMTP_PASSWORD")).toOption)

val send: Mail.Send = m => Smtp.blocking(server, Secrets.env, m)

send(Mail(from, Seq(Address("ada@example.org")), "Ваш код", "Ваш код: 123456")) match
  case Right(_)                              => // gone
  case Left(Rejection.Recipient(a, _, why))  => // tell them THIS address is wrong
  case Left(Rejection.MailboxFull(a, _, _))  => // and this one is not
  case Left(Rejection.RelayRefused(_, _))    => // and this one is ours to fix
```

**Failure is data.** A rejected recipient, a full mailbox and a
refused relay are three different things, and a caller answering a
person has to tell them apart; an exception collapses all three into
"it did not work", which is the one message nobody can act on. A 550
that mentions relaying is a configuration problem and a 550 that does
not is a bad address — the same code, and sending someone to fix the
wrong one fixes nothing.

**The wire is a function and the socket is a shell.** `Session.next`
takes the state and the server's reply and returns the lines to write.
That is what lets sixteen tests cover the whole protocol — multi-line
replies, STARTTLS, both AUTH mechanisms, per-recipient rejection,
dot-stuffing, MIME and encoded words — in the default gate with no
network at all. The socket half is a page of code and its loopback
test binds a real port, so that one is `Live`-tagged and runs under
`sbt integrationTest`.

**Refusing to send in the clear is the default.** A server that
advertises no STARTTLS is refused unless a caller sets `requireTls =
false` — sending unencrypted has to be something a caller states, not
something it inherits.

**UTF-8 travels as base64 under a MIME trio, and a non-ASCII subject
as an RFC 2047 encoded word.** Eight-bit bytes through a server that
never advertised 8BITMIME is corruption nobody sees until a person
reads it, and the consumer who asked for this writes Ukrainian and
Polish.

Out of scope, deliberately: receiving, attachments, multipart,
templates, a connection pool, DKIM. Each arrives with a reader or not
at all.
