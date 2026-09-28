- [ ] dlm-explain-missing — an `Explanation` of a `Route.Missing` names no
      layer, no rule and no lesson, though a layer DID decide the intent:
      `Missing(intent, slot)` carries no `Support` (by design — what to
      ask is the caller's), and `Explanation.of` reads support out of
      `Fires` alone. Found 2026-09-28 by okay-watch's bot: a sentence a
      LESSON routed to `check`, whose address slot was missing, explained
      as «layer: none, lesson: none» — an audit that cannot say the
      memory decided cannot show what learning did. The fact is already
      in `noticed`, which carries every layer's support for every intent
      it saw, so the fix is a lookup and no type changes: for a route
      that NAMES an intent, take that intent's support from `noticed`
      when the route does not carry one. specs/dlm-learning.md's
      `explain` line; a test for a Missing decided by a rule and one by
      a lesson.
