package okay.onnx

/**
 * THE RUNTIME IS NOT BELOW 1.30, and the reason is a measured defect, not
 * a taste (sprint onnx-runtime-130, measured by okay-chat 2026-09-25):
 * onnxruntime 1.20 MIS-EXECUTES a per-channel int8 MatMul. A GLiNER model
 * quantised per channel read «москве» 0.31 under 1.20 and 0.90 under 1.30
 * — the same file and the same tensors, reproduced in Python on both — and
 * the quantised MiniLM this module is used with moved under 1.30 (min
 * cosine 0.973). fp32 models are right under both.
 *
 * The property tests beside this one cannot see that: they check shapes
 * and invariants, which a wrong MatMul keeps. So the version itself is
 * asserted, read from the NATIVE library the process actually loaded, not
 * from build.sbt — a dependency override or an eviction elsewhere is
 * exactly how an older runtime would come back unnoticed. Needs no model.
 */
class TestRuntimeVersion extends munit.FunSuite {

  test("the loaded onnxruntime is 1.30 or newer — 1.20 breaks per-channel int8") {
    val v = ai.onnxruntime.OrtEnvironment.getEnvironment.getVersion
    val Array(major, minor) = v.split("\\.").take(2).map(_.takeWhile(_.isDigit).toInt)
    assert(major > 1 || (major == 1 && minor >= 30), s"onnxruntime $v is loaded; 1.30+ is required (see the header)")
  }
}
