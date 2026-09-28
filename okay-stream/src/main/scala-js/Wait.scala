package okay

/** JS has no producer threads: nothing arrives while the merge spins,
 * so a dry ring registers at once — no rung is ever climbed */
private[okay] object Wait:
  final val Threads = false
  def yieldNow(): Unit = ()
  def sleepBriefly(): Unit = ()
