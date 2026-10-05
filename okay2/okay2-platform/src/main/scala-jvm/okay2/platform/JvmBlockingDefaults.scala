package okay2.platform

import okay2.async.{BlockingDefaults, CanBlock, Scheduler, Timer}

/** The JVM's existing capabilities, available to JPMS service discovery. */
final class JvmBlockingDefaults extends BlockingDefaults {
  def canBlock: CanBlock = Platform.canBlock
  def timer: Timer = Platform.timer
  def scheduler: Scheduler = Platform.scheduler
}
