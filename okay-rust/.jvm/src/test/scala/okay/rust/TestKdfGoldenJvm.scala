package okay.rust

import okay.Handler

/** the pinned vectors are BouncyCastle's bytes (default gate) */
class TestKdfGoldenJvm extends KdfGoldenSuite:
  def handler: Handler[Kdf] = Kdf.using(op =>
    if op.memoryKb < 8 * op.parallelism then Left("parameters Argon2 refuses") else TestKdf.bouncy(op))
