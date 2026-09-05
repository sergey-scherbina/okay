# Sending mail — SMTP as a wire, not a dependency

## Overview

A consumer's first BLOCKING request. A one-time code to an email
address is how a stranger proves the address is theirs, and until they
do, their profile is a draft: invisible in search, unable to hold a
deal. Their delivery was `Identity.console` — the code printed to a
server log, honestly marked development-only — so the service worked
and could not have users.

It belongs here for the reason okay-pg does. Postgres arrives as its
own wire with its own SCRAM and TLS rather than as a driver
dependency; okay-http and okay-tls are the same shape. SMTP is a
smaller version of that job: a line-oriented text protocol over a
socket, STARTTLS over the `okay-tls` that already exists, and AUTH
PLAIN/LOGIN which are simpler than the SCRAM already written.

**Send only.** Receiving is IMAP or POP, a different and much larger
module, and the consumer who asked for this argued against bundling
it.

## Interface

- [x] `Address(email, name)` and `Mail(from, to, subject, body,
      headers)` — one message, a UTF-8 body, no attachments and no
      templates. The feature arrives with its reader.
- [x] `Send = Mail => Either[Rejection, Accepted] ! Async` — an
      effect, so `main` wires a server and a test wires a recorder,
      the way `Transport` already works.
- [x] `Rejection` as DATA, not an exception: `Recipient`,
      `MailboxFull`, `RelayRefused`, `Auth`, `Connection`, `Protocol`.
      A caller answering a person must tell a bad address from a full
      mailbox from a relay that refused it; an exception collapses
      all three into "it did not work".
- [x] `Smtp.send(server)` for the real thing, `Mail.recorder` for a
      test.

## Design — the wire is pure and the socket is a shell

The protocol is a conversation: the client writes a line, the server
answers with a code and text, and what the client writes next depends
on the answer. That is a FUNCTION, and keeping it one is what makes
this testable without a network:

    Session.next(state, reply) -> Step(what to write, new state) | Done

- [x] the pure half decides every line and every outcome, and runs in
      the default gate against scripted server replies
- [x] the socket half connects, reads lines, writes lines and upgrades
      to TLS — thin enough to read in one screen
- [x] the loopback test binds a real port, so it is `Live`-tagged
      under the standing policy and out of `sbt test`

## Behavior

- [x] a multi-line reply (`250-SIZE` … `250 HELP`) is one reply
- [x] `STARTTLS` when the server advertises it and the config asks;
      refusing to send in the clear is the DEFAULT, and sending
      without TLS is a choice a caller states
- [x] `AUTH PLAIN` and `AUTH LOGIN`, chosen from what the server
      advertises; credentials come from `Secrets`, never inline
- [x] one `RCPT TO` per recipient, and a rejection there names the
      ADDRESS — a mail to three people where one address is dead must
      not read as a total failure
- [x] `DATA` dot-stuffs: a body line beginning with `.` is doubled,
      which is the oldest bug in SMTP clients
- [x] a UTF-8 body travels as base64 with a MIME trio
      (`MIME-Version`, `Content-Type: text/plain; charset=utf-8`,
      `Content-Transfer-Encoding: base64`), because 8-bit bytes
      through a server that never advertised 8BITMIME is corruption
      nobody sees until a person reads it
- [x] a non-ASCII `Subject` is an RFC 2047 encoded-word — the
      consumer asking for this writes Ukrainian and Polish
- [x] `QUIT` always, including after a rejection

## Out of scope, deliberately

Receiving. Attachments. MIME multipart. Templates. A connection pool.
DKIM signing. Each of them arrives with a reader or not at all.

## Results — okay-mail-smtp (2026-09-05)

The module is 3 files and 16 + 3 tests: the protocol, the socket, the
message.

**Sixteen of the nineteen tests need no network**, which is the whole
reason `Session` is a function of (state, reply). Multi-line replies,
STARTTLS, both AUTH mechanisms, per-recipient rejection, dot-stuffing,
the MIME trio and encoded words are all decided there and asserted
against scripted server lines. The three that do need a socket run
against a forty-line loopback server, bind a real port, and are
therefore `Live`-tagged and out of the default gate.

**What the consumer asked for, and what they got:**

| asked | shipped |
|---|---|
| send only | send only; no IMAP, no POP |
| one message, no MIME framework | `Mail`, plus the one MIME trio a UTF-8 body cannot travel without |
| an effect a test can replace | `Mail.Send` is a function; `Smtp.send` for a server, `Mail.Recorder` for a test |
| failure as data | six `Rejection` cases, and the 550-means-two-things distinction |

**Three decisions they did not ask for and would have hit anyway:**

- **Refusing to send in the clear is the DEFAULT.** A server with no
  STARTTLS is refused unless `requireTls = false`. Sending
  unencrypted has to be a caller's stated choice, not one inherited
  from a server's capability list.
- **A 550 mentioning relaying is not a bad address.** Same code, two
  meanings, and telling a person the wrong one sends them to fix
  something that is not broken. `RelayRefused` is separate from
  `Recipient` for that reason alone.
- **UTF-8 as base64 under a MIME trio, and RFC 2047 for the subject.**
  Eight-bit bytes through a server that never advertised 8BITMIME is
  corruption nobody sees until a person reads it, and the consumer
  writes Ukrainian and Polish. Their one-time-code mail would have
  arrived as mojibake on the first non-ASCII subject.

**One thing about my own process, recorded because it nearly shipped.**
The SASL PLAIN payload is NUL, user, NUL, password — and I typed the
NULs as literal control characters into the source. They were
invisible in every view of the file, and only rejected because a tool
refused to run a command containing them. They are `\u0000` escapes
now, with a comment saying why. A literal control character in a
source file is a defect that no review catches by reading.

### Not measured, and named as such

Nothing here has a benchmark. It is a protocol, its cost is a network
round trip, and a microsecond figure for line assembly would be the
kind of number this line spent today removing from another module.

### Not done

STARTTLS is exercised in the pure tests and NOT over a real socket:
the loopback leg runs in the clear because a TLS server needs a
certificate and a key, which is a test fixture this module does not
have yet. Filed as `mail-loopback-tls`.

