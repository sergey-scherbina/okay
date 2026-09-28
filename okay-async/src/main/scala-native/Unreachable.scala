package okay

/** no collector door on this platform: an abandoned program's scope
 * is released only by the doors that see a program end */
private[okay] object Unreachable:
  def onCollected(o: AnyRef, action: Runnable): Unit = ()
