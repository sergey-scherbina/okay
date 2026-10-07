package okay


/** JS has no producer threads: nothing arrives while a consumer waits,
 * so every rung is empty and `threads` says so — a `Wait` gives up at
 * once and the consumer registers */
private[okay] object PlatformPause extends Pause:
  def threads: Boolean = false
  def spin(): Unit = ()
  def yieldNow(): Unit = ()
  def nano(): Unit = ()
  def block(): Unit = ()
