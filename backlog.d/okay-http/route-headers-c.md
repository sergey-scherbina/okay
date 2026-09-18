- [x] route-headers-c — DONE 2026-09-11. `Answer.headers` and
      `Router.answering(status, names*)`, rendered as
      `responses[*].headers`. The distinction the stage exists to
      state: a secured route's `www-authenticate` is declared AND
      written from one value, so it is true by construction; an
      author's declaration is DESCRIPTION, and enforcing it would turn
      a documentation slip into a 500. Saying which half is
      load-bearing beats pretending both are.
