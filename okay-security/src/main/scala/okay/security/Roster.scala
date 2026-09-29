package okay.security

import okay.codec.Json
import okay.codec.Json.*
import okay.persist.{Ack, Topic}

/**
 * WHO A MESSAGE CAME FROM, AND WHAT THEY MAY DO (specs/identity-roster.md).
 *
 * The primitives next door say what a principal IS and what a policy
 * DECIDES; nothing said that Telegram user `123456` is one, or that they
 * may run agents in one project. okay-chat wrote that as application
 * code and ran it against real users; this is the shape with the product
 * taken out: an address on a channel bound to a principal, roles as
 * grants, both appended to a topic and folded back on start.
 */

/** where a message came from — total: a channel this module does not
 * name is `Other(kind)`, named, never dropped */
enum Channel:
  case Telegram, Web, Console, Mcp, Mail
  case Other(kind: String)

object Channel:
  def code(c: Channel): String = c match
    case Other(k) => k
    case c => c.toString.toLowerCase
  def parse(s: String): Channel = s.trim.toLowerCase match
    case "telegram" => Telegram
    case "web" => Web
    case "console" => Console
    case "mcp" => Mcp
    case "mail" => Mail
    case k => Other(k)

/** an address on a channel, bound to a principal at a time */
final case class Binding(channel: Channel, address: String, principal: String, at: Long)

/** a role is a fact about a principal, read by a Policy; a scope is a
 * resource prefix the role is confined to, or none */
final case class Grant(principal: String, role: String, scope: Option[String], at: Long)

/**
 * The roster: a fold of its topic. Every change is appended (keyed by
 * principal) and then applied, so a crash between the two loses nothing
 * that was acknowledged; `load()` replays. In memory the fold is two
 * immutable maps behind one lock, which is what lets the JS host keep
 * the same roster.
 */
final class Roster(topic: Topic, now: () => Long = () => System.currentTimeMillis()):
  private val lock = new Object
  private var bound = Map.empty[(Channel, String), Binding]
  private var held = Map.empty[String, Vector[Grant]]

  load(): Unit

  /** fold the topic from its beginning; how many records were applied */
  def load(): Int =
    var n = 0
    lock.synchronized {
      bound = Map.empty; held = Map.empty
      for part <- 0 until topic.partitions do
        var from = topic.begin(part)
        var going = true
        while going do
          topic.read(part, from, 256) match
            case Topic.Read.TooEarly(b) => from = b
            case Topic.Read.Records(rs) if rs.isEmpty => going = false
            case Topic.Read.Records(rs) =>
              rs.foreach { r => if apply(Json.parse(new String(r.value, "UTF-8"))) then n += 1 }
              from = rs.last.offset + 1
    }
    n

  // ---- the fold ----------------------------------------------------------

  /** one record into the maps; false for a record this version does not read */
  private def apply(j: Json): Boolean =
    Claims.str(j, "kind") match
      case Some("bound") =>
        val b = Binding(Channel.parse(Claims.str(j, "channel").getOrElse("")), Claims.str(j, "address").getOrElse(""),
          Claims.str(j, "principal").getOrElse(""), Claims.num(j, "at").getOrElse(0L))
        bound += (b.channel, b.address) -> b; true
      case Some("unbound") =>
        bound -= ((Channel.parse(Claims.str(j, "channel").getOrElse("")), Claims.str(j, "address").getOrElse(""))); true
      case Some("granted") =>
        val g = Grant(Claims.str(j, "principal").getOrElse(""), Claims.str(j, "role").getOrElse(""),
          Claims.str(j, "scope"), Claims.num(j, "at").getOrElse(0L))
        val rest = held.getOrElse(g.principal, Vector.empty).filterNot(o => o.role == g.role && o.scope == g.scope)
        held += g.principal -> (rest :+ g); true
      case Some("revoked") =>
        val p = Claims.str(j, "principal").getOrElse("")
        val role = Claims.str(j, "role").getOrElse(""); val scope = Claims.str(j, "scope")
        held += p -> held.getOrElse(p, Vector.empty).filterNot(o => o.role == role && o.scope == scope); true
      case _ => false

  private def write(principal: String, j: Json): Unit =
    topic.append(principal.getBytes("UTF-8"), Json.print(j).getBytes("UTF-8"), Ack.Durable): Unit
    apply(j): Unit

  private def rec(kind: String, fields: (String, Json)*): Json =
    JObj(Vector("kind" -> JStr(kind)) ++ fields.toVector :+ ("at" -> JNum(now().toDouble)))

  // ---- bindings -----------------------------------------------------------

  /** this address names this principal from now on; a second bind of the
   * same address replaces the first */
  def bind(channel: Channel, address: String, principal: String): Binding = lock.synchronized {
    write(principal, rec("bound", "channel" -> JStr(Channel.code(channel)), "address" -> JStr(address),
      "principal" -> JStr(principal)))
    bound((channel, address))
  }

  def unbind(channel: Channel, address: String): Boolean = lock.synchronized {
    bound.get((channel, address)) match
      case None => false
      case Some(b) =>
        write(b.principal, rec("unbound", "channel" -> JStr(Channel.code(channel)), "address" -> JStr(address)))
        true
  }

  /** the principal behind an address, with the roles they hold as claims */
  def whoIs(channel: Channel, address: String): Option[Principal] = lock.synchronized {
    bound.get((channel, address)).map(b => principalOf(b.principal))
  }

  def addressesOf(principal: String): Vector[(Channel, String)] = lock.synchronized {
    bound.values.filter(_.principal == principal).toVector.sortBy(_.at).map(b => (b.channel, b.address))
  }

  def bindings: Vector[Binding] = lock.synchronized(bound.values.toVector.sortBy(b => (b.at, b.address)))

  // ---- grants -------------------------------------------------------------

  def grant(principal: String, role: String, scope: Option[String] = None): Grant = lock.synchronized {
    write(principal, rec("granted", "principal" -> JStr(principal), "role" -> JStr(role),
      "scope" -> scope.fold[Json](JNull)(JStr(_))))
    held(principal).last
  }

  /** false, and nothing appended, when the principal does not hold it */
  def revoke(principal: String, role: String, scope: Option[String] = None): Boolean = lock.synchronized {
    if !held.getOrElse(principal, Vector.empty).exists(g => g.role == role && g.scope == scope) then false
    else
      write(principal, rec("revoked", "principal" -> JStr(principal), "role" -> JStr(role),
        "scope" -> scope.fold[Json](JNull)(JStr(_))))
      true
  }

  def rolesOf(principal: String): Vector[Grant] = lock.synchronized(held.getOrElse(principal, Vector.empty))

  def grants: Vector[Grant] = lock.synchronized(held.values.flatten.toVector.sortBy(g => (g.at, g.principal, g.role)))

  /** everyone bound or granted, as principals */
  def principals: Vector[Principal] = lock.synchronized {
    (bound.values.map(_.principal) ++ held.keys).toVector.distinct.sorted.map(principalOf)
  }

  /** `id` is the roster's principal; `name` the last address bound to it
   * (or the id); the roles ride in `claims.json` under `roles`, where
   * `Policy.role` already reads them */
  private def principalOf(id: String): Principal =
    val name = bound.values.filter(_.principal == id).toVector.sortBy(_.at).lastOption.map(_.address).getOrElse(id)
    val roles = held.getOrElse(id, Vector.empty).map(_.role).distinct
    Principal(id, name, Claims(json = JObj(Vector("roles" -> JArr(roles.map(JStr(_)))))))

object Roster:

  /** a Policy that reads roles from the roster: Permit when the principal
   * holds `role`, and — where the grant has a scope — the resource starts
   * with it */
  def role(r: Roster, role: String): Policy = (p, _, resource) =>
    if r.rolesOf(p.id).exists(g => g.role == role && g.scope.forall(resource.startsWith)) then Decision.Permit
    else Decision.Deny(s"'${p.id}' does not hold '$role' for '$resource'")

  /** the principal `owner` is bound to this address and holds `owner`, from
   * configuration and never "whoever wrote first"; the console is the
   * owner's too — the process is theirs. Idempotent: an existing roster
   * is loaded, not re-written. */
  def owned(topic: Topic, channel: Channel, address: String,
            now: () => Long = () => System.currentTimeMillis()): Roster =
    val r = Roster(topic, now)
    if r.whoIs(channel, address).isEmpty then
      r.bind(channel, address, Owner): Unit
      r.bind(Channel.Console, "", Owner): Unit
      r.grant(Owner, Owner): Unit
    r

  val Owner = "owner"
