- [~] onnx-runtime-130 — okay-onnx pins onnxruntime 1.20.0, and 1.20
      MIS-EXECUTES a per-channel int8 MatMul. Measured by okay-chat
      2026-09-25 (its BACKLOG ort-130-minilm, gliner-onnx; specs/meaning.md
      there): GLiNER multi-v2.1 quantised per channel read «москве» 0.31
      under 1.20 and 0.90 under 1.30 — the same file, the same tensors,
      reproduced in Python on both versions; fp32 is right under both. And
      the quantised MiniLM (Xenova paraphrase-multilingual-MiniLM-L12-v2,
      model_quantized.onnx) MOVES under 1.30: min cosine 0.973, max
      component 0.036 over 400 phrases — the same defect in what okay-onnx
      users embed with. okay-chat moved (dependencyOverrides 1.30.0,
      0d6f6db6 there): its routing and journal regress unchanged, its slot
      readers slightly better. Here: raise okay-onnx to 1.30.x with its own
      gate — okay-onnx's tests, the intent fixtures that embed through it,
      and every consumer's recompiled artifacts measured, because the
      vectors change.
