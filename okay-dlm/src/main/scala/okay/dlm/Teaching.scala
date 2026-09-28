package okay.dlm

/**
 * WHO MAY TEACH WHAT — the rights seam of the learning mode
 * (specs/dlm-learning.md §3).
 *
 * A deterministic model learns in one way only: a person pairs a
 * sentence with a class the model already has. This is what decides
 * whose pairing counts for whom. OURS is the narrowest: everybody
 * teaches themselves, nobody is a teacher or a steward, every channel
 * is on — the model as it has always learned. A service with roles
 * brings its own `given Teaching` over its own lists; the library
 * never stores a right.
 */
trait Teaching:
  /** may this person teach this pair for themselves? */
  def own(who: String, intent: String): Boolean
  /** may this person's lesson become everyone's on its own — a teacher? */
  def teacher(who: String): Boolean
  /** may this person act on somebody else's lessons — withdraw, share, revoke? */
  def steward(who: String): Boolean
  /** the kill switch: learning off entirely, or per channel */
  def enabled(channel: Teaching.Channel): Boolean

object Teaching:

  enum Channel:
    case Lesson, Withdrawal, Corpus

  /** OURS: the narrowest rights and every channel on */
  given ours: Teaching = roles()

  /** rights from predicates — a service's lists, an identity provider */
  def roles(own: (String, String) => Boolean = (_, _) => true,
            teachers: String => Boolean = _ => false,
            stewards: String => Boolean = _ => false,
            channels: Channel => Boolean = _ => true): Teaching =
    val (o, t, s, c) = (own, teachers, stewards, channels)
    new Teaching:
      def own(who: String, intent: String) = o(who, intent)
      def teacher(who: String) = t(who)
      def steward(who: String) = s(who)
      def enabled(channel: Channel) = c(channel)

  /** the kill switch thrown: nothing learns, every door refuses, the
   * fold stays as it was — and it needs no redeploy to throw */
  val off: Teaching = roles(channels = _ => false)

  /** the same rights with one channel switched */
  def switched(t: Teaching, channel: Channel, on: Boolean): Teaching = new Teaching:
    def own(who: String, intent: String) = t.own(who, intent)
    def teacher(who: String) = t.teacher(who)
    def steward(who: String) = t.steward(who)
    def enabled(c: Channel) = if c == channel then on else t.enabled(c)
