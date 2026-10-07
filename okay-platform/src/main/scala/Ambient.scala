package okay


import okay.freer.*


import okay.std.*
/**
 * THE REAL THINGS, in the runtime module (specs/audit-ready.md): the
 * system clock and the platform's random source, as the handlers of
 * the core's `Clock` and `Random` ports, and the ambient HLC and Uid
 * generators okay-data used to carry itself. okay-data is a `business`
 * module of okay-audit now and reads neither a clock nor a random
 * source; this object is where those reads live, listed in the
 * inventory as okay-platform's.
 */
object Ambient:
  /** milliseconds since the epoch, from the system's wall clock */
  def millis(): Long = System.currentTimeMillis()

  /** 64 random bits from the platform's generator — NOT cryptographic */
  def randomLong(): Long = scala.util.Random.nextLong()

  /** the `Clock` port answered by the system's wall clock */
  val clock: Handler[Clock, [A] =>> A] = Clock.at(() => millis())

  /** the `Random` port answered by the platform's generator */
  val random: Handler[Random, [A] =>> A] = Random.at(() => randomLong())

  /** the ambient hybrid logical clock over the system's wall time */
  val hlc: Hlc.Clock = Hlc.at(() => millis())

  /** the next HLC stamp from the ambient clock */
  def stamp(): Hlc.Stamp = hlc.next()

  /** merge a remote stamp into the ambient HLC */
  def observe(remote: Hlc.Stamp): Hlc.Stamp = hlc.observe(remote)

  /** the ambient Uid generator: the system clock, the platform's random.
   * A UUIDv7 is unique, not unguessable — a secret is okay-security's */
  val uids: Uid.Gen = Uid.at(() => millis(), () => randomLong())

  /** the next id from the ambient generator */
  def uid(): Uid = uids.next()
