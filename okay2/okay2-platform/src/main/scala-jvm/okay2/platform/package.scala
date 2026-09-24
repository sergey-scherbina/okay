package okay2

import okay2.async.{BlockingDefaults, CanBlock, Scheduler, Timer}

/**
 * okay2-platform — the JVM under okay2-async (specs/okay2.md,
 * stage 4): `import okay2.platform._` installs the capabilities every
 * blocking door asks for — `CanBlock` (park a virtual thread),
 * `Timer` (one scheduled thread holds every pending delay), the
 * default `Scheduler` (Loom where this JVM has it, the owned workers
 * where it does not) — plus `Net`, and the fiber combinators that
 * need a blocking join (`parAll`, `parTraverse`, `retry`,
 * `supervised`). Where the Scala 3 core's givens are top-level in
 * package `okay`, Scala 2's live here in the package object, so one
 * import brings them all.
 */
package object platform extends platform.BlockingOps {

  /** the JVM's three capabilities as ONE implicit: `CanBlock` (park a
   * virtual thread), `Timer` (one scheduled thread for every delay),
   * the default `Scheduler` (Loom where this JVM has it, `own` watched
   * where it does not; `-Dokay.scheduler=own|adaptive|drive|threads|loom`
   * selects another). The companions derive each from this, so a local
   * `implicit val S: Scheduler = ...` overrides without ambiguity. */
  implicit val jvm: BlockingDefaults = new BlockingDefaults {
    def canBlock: CanBlock = Platform.canBlock
    def timer: Timer = Platform.timer
    def scheduler: Scheduler = Platform.scheduler
  }

  /** the three, by name, for a caller that wants one explicitly */
  def canBlock: CanBlock = Platform.canBlock
  def timer: Timer = Platform.timer
  def scheduler: Scheduler = Platform.scheduler

  /** the blocking socket behind Async.Run */
  implicit val net: Net = Platform.net
}
