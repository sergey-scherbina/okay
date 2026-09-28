- [ ] telegram-awaiting — `Chats.awaiting(chat)`: a chat whose screen asked
      for a typed value (`Act.Ask`, the pencil beside an `Input`) and has
      not had it yet. A consumer that understands text ITSELF — okay-watch's
      check bot reads a sentence with okay-dlm before handing anything to
      the screen — has to know which of the two the next message belongs
      to, and today it cannot: a press is opaque callback data, so the
      consumer either steals the value the screen asked for or hands the
      screen a sentence it has no focus for and the person gets SILENCE
      (`Session.hear` of a `Said` with no focus is no event and no act).
      The fact is already in the acts `Chats.perform` performs, so the
      answer is a flag kept there — set on `Ask`, cleared when a message
      reaches that chat's door. specs/telegram-bot.md's behaviour box; a
      test through the recording Bot API.
