## onnx-runtime-130 - okay-onnx on onnxruntime 1.30: 1.20 mis-executes per-channel int8

Measured by okay-chat (its ort-130-minilm and gliner-onnx, 2026-09-25):
onnxruntime 1.20 computes a per-channel int8 MatMul wrong — a GLiNER model
quantised per channel read «москве» 0.31 under 1.20 and 0.90 under 1.30,
the same file and tensors, reproduced in Python on both versions — and the
quantised MiniLM okay-onnx is used with moved under 1.30 (min cosine 0.973
over 400 phrases). fp32 is right under both.

- build.sbt: okay-onnx's onnxruntime 1.20.0 -> 1.30.0.
- okay-onnx `TestRuntimeVersion`: the LOADED native version is 1.30+,
  needs no model; seen red with 1.20 put back, green on 1.30. The property
  tests beside it (TestEncoder, 4, with OKAY_ONNX_MODEL = the MiniLM) pass
  under both versions — which is exactly why they could not catch it.
- Consumers' vectors move: okay-chat measured its routing and journal
  regress unchanged and its slot readers slightly better, and recompiled
  its artifacts (0d6f6db6 there). Any other consumer recompiles too.
