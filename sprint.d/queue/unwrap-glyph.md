- unwrap-glyph — one glyph, one meaning. ALL FOUR STAGES LANDED
  2026-09-18 (82753d21, 18a558be, 74ecac89; specs/unwrap-glyph.md
  Results). `.?` is the direct mark again; the Throws glyphs live in
  their type's companion where a converted receiver cannot reach them;
  the row peek is `peek`. The spec's own stage-1 design was REFUTED by
  the compiler (`throws` is covariant in E, so no condition on E can
  separate a converted receiver from a genuine one) and the Results
  carry the refusal message that showed it. Delete this entry at the
  next queue rewrite.
