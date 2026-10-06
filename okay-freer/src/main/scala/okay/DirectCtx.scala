package okay

import scala.annotation.implicitNotFound

/** Evidence installed only while a `direct` block is being compiled. */
@implicitNotFound("no DirectCtx[${F}]: auto-coloring works only INSIDE a direct block.\nWrap the code in direct[F] { ... } — or use the explicit marks (.reflect / .? / !prog),\nwhich need no capability.")
final class DirectCtx[F[_]] private[okay] ()
