package okay.resilience

import okay.*
import java.net.{InetAddress, UnknownHostException}

/** the JVM's own sources: the process environment and the resolver */
object DiscoveryJvm:

  /** `Discovery.env` over `sys.env` */
  def env(): Discovery = Discovery.env(sys.env.get)

  /** every A/AAAA record of the name, on `port` — a Kubernetes
    * headless Service is exactly that; an unknown name answers empty,
    * not a throw. Wrap in `Discovery.cached` for a ttl. */
  def dns(port: Int): Discovery = new Discovery:
    def resolve(service: String): Vector[Endpoint] ! Async = okay.async {
      try InetAddress.getAllByName(service).toVector.map(a => Endpoint(a.getHostAddress, port)).distinct
      catch case _: UnknownHostException => Vector.empty
    }
