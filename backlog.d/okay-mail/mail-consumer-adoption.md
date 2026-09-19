- [ ] mail-consumer-adoption — the consumer who asked for okay-mail
      replaces `Identity.console` with it. Not my lane to do, but the
      one that tells whether the seam is right: their `deliver` is
      `(Channel, String, String) => Unit` and `Mail.Send` has to plug
      in without anything else changing, which was their stated
      requirement.
      (was filed under "the autonomy programme" — the reasoning
      that section carries is in BACKLOG-ARCHIVE.md)
