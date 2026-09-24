package okay2

import okay2.async.{BlockingDefaults, CanBlock, Scheduler, Timer}

/**
 * okay2-platform on Scala Native (okay2-cross): `import okay2.platform._`
 * installs the three capabilities as on the JVM — `CanBlock` by
 * wait/notify, a thread per timer, one OS thread per fiber — plus `Net`
 * and the blocking combinators.
 */
package object platform extends platform.BlockingOps {

  implicit val native: BlockingDefaults = new BlockingDefaults {
    def canBlock: CanBlock = Platform.canBlock
    def timer: Timer = Platform.timer
    def scheduler: Scheduler = Platform.scheduler
  }

  def canBlock: CanBlock = Platform.canBlock
  def timer: Timer = Platform.timer
  def scheduler: Scheduler = Platform.scheduler

  /** the blocking socket behind Async.Run */
  implicit val net: Net = Platform.net
}
