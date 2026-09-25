package okay.kernel

import okay.{!, Resource}

/** how many providers a port takes: exactly one, or every one there is */
enum Arity:
  case One, Many

/** a named check every implementation of a port must pass — at start,
 * not only in the repository's tests: a plugin from a directory never
 * ran through them */
final case class Law[A](name: String, check: A => Either[String, Unit])

object Law:
  /** a law as a predicate, with the sentence said when it fails */
  def holds[A](name: String)(p: A => Boolean): Law[A] =
    Law(name, a => if p(a) then Right(()) else Left(s"$name does not hold"))

/**
 * A CONTRACT: a typed name, the version of the contract (the trait `A`
 * and its laws — not of any implementation), how many providers it
 * takes, and its laws. Identity is the name: two `Port` values with one
 * name are one port, and the kernel refuses them if their versions
 * differ (specs/kernel.md).
 */
final class Port[A] private (val name: String, val version: Version, val arity: Arity,
                             val laws: Vector[Law[A]]):
  override def toString: String = s"$name@$version"
  override def equals(o: scala.Any): Boolean = o match
    case p: Port[?] => p.name == name
    case _ => false
  override def hashCode: Int = name.hashCode

object Port:
  def one[A](name: String, version: Version, laws: Law[A]*): Port[A] =
    new Port(name, version, Arity.One, laws.toVector)
  def many[A](name: String, version: Version, laws: Law[A]*): Port[A] =
    new Port(name, version, Arity.Many, laws.toVector)

/** what a plugin needs: a port, the contract versions it can use, and
 * whether it runs without one */
final case class Need(port: Port[?], range: Range, optional: Boolean = false)

object Need:
  /** the usual need: at least the version the plugin was written against */
  def of(port: Port[?], range: String): Need = Need(port, Range(range))
  def maybe(port: Port[?], range: String): Need = Need(port, Range(range), optional = true)

/**
 * What a plugin gives: an implementation of `port`, made from what the
 * plugin needs, as a `Resource` so the kernel releases it.
 *
 * `built` is the contract version the plugin was COMPILED against, said
 * as a literal. It is not `port.version`: inside a plugin that reads the
 * HOST's value at run time, which is exactly the number that cannot
 * tell a stale plugin from a current one.
 */
final case class Provision[A](port: Port[A], built: Version, make: Wiring => A ! Resource)

object Provision:
  /** a provision with nothing to release */
  def value[A](port: Port[A], built: String)(make: Wiring => A): Provision[A] =
    Provision(port, Version(built), w => okay.pure(make(w)))

/**
 * A part of a program the kernel assembles. A class with a public
 * no-argument constructor when it is found by `ServiceLoader` (a Scala
 * `object` is not one); listed in
 * `META-INF/services/okay.kernel.Plugin`.
 */
trait Plugin:
  def id: String
  def version: Version
  /** the kernel APIs this plugin was built for */
  def kernel: Range = Range.Caret(Kernel.api)
  def needs: Vector[Need]
  def provides: Vector[Provision[?]]
  override def toString: String = s"$id@$version"

/** what a plugin reads while it is being made: only the ports it
 * declared in `needs` — a plugin that reached past its declaration
 * would make the plan a lie */
trait Wiring:
  def one[A](p: Port[A]): A
  def all[A](p: Port[A]): Vector[A]
  def maybe[A](p: Port[A]): Option[A]
