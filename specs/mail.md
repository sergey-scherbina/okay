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

- [ ] `Address(email, name)` and `Mail(from, to, subject, body,
      headers)` — one message, a UTF-8 body, no attachments and no
      templates. The feature arrives with its reader.
- [ ] `Send = Mail => Either[Rejection, Accepted] ! Async` — an
      effect, so `main` wires a server and a test wires a recorder,
      the way `Transport` already works.
- [ ] `Rejection` as DATA, not an exception: `Recipient`,
      `MailboxFull`, `RelayRefused`, `Auth`, `Connection`, `Protocol`.
      A caller answering a person must tell a bad address from a full
      mailbox from a relay that refused it; an exception collapses
      all three into "it did not work".
- [ ] `Smtp.send(server)` for the real thing, `Mail.recorder` for a
      test.

## Design — the wire is pure and the socket is a shell

The protocol is a conversation: the client writes a line, the server
answers with a code and text, and what the client writes next depends
on the answer. That is a FUNCTION, and keeping it one is what makes
this testable without a network:

    Session.next(state, reply) -> Step(what to write, new state) | Done

- [ ] the pure half decides every line and every outcome, and runs in
      the default gate against scripted server replies
- [ ] the socket half connects, reads lines, writes lines and upgrades
      to TLS — thin enough to read in one screen
- [ ] the loopback test binds a real port, so it is `Live`-tagged
      under the standing policy and out of `sbt test`

## Behavior

- [ ] a multi-line reply (`250-SIZE` … `250 HELP`) is one reply
- [ ] `STARTTLS` when the server advertises it and the config asks;
      refusing to send in the clear is the DEFAULT, and sending
      without TLS is a choice a caller states
- [ ] `AUTH PLAIN` and `AUTH LOGIN`, chosen from what the server
      advertises; credentials come from `Secrets`, never inline
- [ ] one `RCPT TO` per recipient, and a rejection there names the
      ADDRESS — a mail to three people where one address is dead must
      not read as a total failure
- [ ] `DATA` dot-stuffs: a body line beginning with `.` is doubled,
      which is the oldest bug in SMTP clients
- [ ] a UTF-8 body travels as base64 with a MIME trio
      (`MIME-Version`, `Content-Type: text/plain; charset=utf-8`,
      `Content-Transfer-Encoding: base64`), because 8-bit bytes
      through a server that never advertised 8BITMIME is corruption
      nobody sees until a person reads it
- [ ] a non-ASCII `Subject` is an RFC 2047 encoded-word — the
      consumer asking for this writes Ukrainian and Polish
- [ ] `QUIT` always, including after a rejection

## Out of scope, deliberately

Receiving. Attachments. MIME multipart. Templates. A connection pool.
DKIM signing. Each of them arrives with a reader or not at all.
