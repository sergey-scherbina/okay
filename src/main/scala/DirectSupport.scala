package okay

import scala.annotation.implicitNotFound

/**
 * The tiny capability surface shared by the effect kernel and the
 * optional direct DSL.  It deliberately contains no macro or runtime
 * implementation, so `okay` remains usable without `okay-direct`.
 */
@implicitNotFound("no Direct.Effect[${F}]: auto-coloring is OPT-IN per signature.\nRegister the effect once — `given Direct.Effect[${F}] with {}` — or use the explicit marks\n(.reflect / .!? / !prog), which need no marker.")
trait DirectEffect[F[_]]

/** Evidence installed only while a `direct` block is being compiled. */
@implicitNotFound("no DirectCtx[${F}]: auto-coloring works only INSIDE a direct block.\nWrap the code in direct[F] { ... } — or use the explicit marks (.reflect / .!? / !prog),\nwhich need no capability.")
final class DirectCtx[F[_]] private[okay] ()
