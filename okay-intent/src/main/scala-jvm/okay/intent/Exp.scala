package okay.intent

/** e^x, the same bits on every JVM and CPU (intent-model-reproducible,
 * 2026-09-29). `Math.exp` may differ by an ulp between an intrinsic and
 * fdlibm: the shipped model, fitted on an ARM Mac, re-fitted one bit
 * apart on x86 and TestModels (which re-derives it byte for byte) went
 * red. StrictMath is fdlibm everywhere. */
private[intent] object Exp:
  inline def apply(x: Double): Double = StrictMath.exp(x)
