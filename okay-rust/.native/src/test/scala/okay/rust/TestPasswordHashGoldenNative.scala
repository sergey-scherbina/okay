package okay.rust

import okay.Answers

/** Scala Native, the Rust staticlib through @extern, held to the pinned bytes
 * (scripts/rust-native-check.sh links the library and runs this) */
class TestPasswordHashGoldenNative extends PasswordHashGoldenSuite:
  def handler: Answers[PasswordHash] = PasswordHash.native
