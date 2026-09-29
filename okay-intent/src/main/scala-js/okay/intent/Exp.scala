package okay.intent

/** e^x on Scala.js, which has no StrictMath: the engine's own. Nothing
 * re-derives a model here byte for byte; that proof is the JVM's
 * (TestModels, intent-model-reproducible). */
private[intent] object Exp:
  inline def apply(x: Double): Double = math.exp(x)
