package okay.rust

import okay.Answers

/** the pinned vectors are BouncyCastle's bytes (default gate) */
class TestPasswordHashGoldenJvm extends PasswordHashGoldenSuite:
  def handler: Answers[PasswordHash] = PasswordHash.using(op =>
    if op.memoryKb < 8 * op.parallelism then Left("parameters Argon2 refuses") else TestPasswordHash.bouncy(op))
