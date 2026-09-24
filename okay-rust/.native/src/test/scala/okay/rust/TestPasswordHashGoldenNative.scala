package okay.rust

import okay.Handler

/** Scala Native, the Rust staticlib through @extern, held to the pinned bytes
 * (scripts/rust-native-check.sh links the library and runs this) */
class TestPasswordHashGoldenNative extends PasswordHashGoldenSuite:
  def handler: Handler[PasswordHash] = PasswordHash.native
