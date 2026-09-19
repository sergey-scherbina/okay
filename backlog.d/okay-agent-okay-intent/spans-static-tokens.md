- [ ] spans-static-tokens — `Static` already holds a vector per token
      unit, which would give `okay.intent.Spans` a JS road with no
      runtime and no model file. The measurement says context is worth
      0.2–0.3 of cosine (specs/intent-spans.md), so this is a
      compromise to MEASURE against the contextual encoder on the same
      317 turns, not a default. Trigger: a consumer that needs the slot
      layer where `okay-onnx` cannot follow.
