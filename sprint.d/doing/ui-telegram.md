- [~] ui-telegram — Telegram as one more CLIENT of okay-ui's wire
      protocol (the operator, via okay-chat 2026-09-23: «сделать чтобы
      уи для телеграма был тоже как и остальные уи … еще один бекенд …
      и для веб версии тоже. Чтобы это была одна и та же логика
      буквально»). specs/ui-telegram.md FIRST. Stage 1, this lane:
      (a) `TelegramView.render(ui)`: the pure mapping Ui -> one message
      (text + inline keyboard), vocabulary {link}; Button a button,
      Check a toggle button, Select a button per option, Input a line
      and an edit button, Form fields + its submit, Link a URL button;
      callback data short and bound to the frame; (b) `TelegramClient`:
      a pure Stage between Telegram updates and Wire protocol lines —
      hello, tree, patches applied to its copy, a press -> Pressed/
      Toggled/Chosen, a text after an edit button -> Edited, a form's
      local edits folded and Submitted like the browser's; a press on
      a stale frame refused; out: message actions as DATA (send/edit/
      answer-callback/force-reply). No HTTP here — the Bot API call is
      the consumer's. Gate: one app served by `Wire.serve` answers
      the SAME states to the scripted test host and to scripted
      Telegram updates through `TelegramClient`.
      Stage 2 (okay-chat): its admin labelling as that app, in the
      chat. Stage 3 (okay-chat): the same app on the web.
