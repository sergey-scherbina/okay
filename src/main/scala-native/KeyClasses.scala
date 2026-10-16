package okay

import okay.one.row.In

/** the key of each effect constructor class, made once: Scala Native has no `ClassValue` — a concurrent map */
private[okay] object KeyClasses:
  private val byClass = java.util.concurrent.ConcurrentHashMap[Class[?], KeyOf[?]]()
  def of(c: Class[?]): KeyOf[?] = byClass.computeIfAbsent(c, c => KeyOf[[x] =>> Any](In.At(c)))
