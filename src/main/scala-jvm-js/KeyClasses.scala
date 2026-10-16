package okay

import okay.one.row.In

/** the key of each effect constructor class, made once: a `ClassValue`, the JVM's and Scala.js's (Scala Native has
 * none — its own `KeyClasses` beside) */
private[okay] object KeyClasses:
  private object byClass extends ClassValue[KeyOf[?]]:
    protected def computeValue(c: Class[?]): KeyOf[?] = KeyOf[[x] =>> Any](In.At(c))
  def of(c: Class[?]): KeyOf[?] = byClass.get(c)
