package okay.graphql

import okay.Async
import okay.one.*
import okay.one.Cont.perform
import okay.one.row.*

/** A SOURCE OF DATA, told apart by identity: one source object, one batch per round, one cache per execution. It answers MANY keys in one
 * call — a SQL `IN`, a batch endpoint; a key it leaves out has no value, and the program that asked fails. Two kinds:
 * a plain function (`Source`), or a program of `Async` (`AsyncSource`, `Executor.executeAsync` only) */
sealed trait DataSource[K, V]:
  def name: String
  /** whether a key's answer is kept for the rest of the execution (the default), or asked again each round —
   * data that changes while the request runs */
  def cached: Boolean = true

/** a source answering at once */
trait Source[K, V] extends DataSource[K, V]:
  def fetch(keys: Seq[K]): Map[K, V]

object Source:
  def apply[K, V](name: String, cached: Boolean = true)(f: Seq[K] => Map[K, V]): Source[K, V] =
    val n = name
    val c = cached
    new Source[K, V]:
      def name: String = n
      override def cached: Boolean = c
      def fetch(keys: Seq[K]): Map[K, V] = f(keys)
      override def toString: String = s"Source($n)"

/** a source answering with a program of `Async` — a database driver, an HTTP client: every async source of a round
 * is called at once, the round going on when all answered */
trait AsyncSource[K, V] extends DataSource[K, V]:
  def fetch(keys: Seq[K]): Map[K, V] ! (Async +: Pure)

object AsyncSource:
  def apply[K, V](name: String, cached: Boolean = true)(f: Seq[K] => Map[K, V] ! (Async +: Pure)): AsyncSource[K, V] =
    val n = name
    val c = cached
    new AsyncSource[K, V]:
      def name: String = n
      override def cached: Boolean = c
      def fetch(keys: Seq[K]): Map[K, V] ! (Async +: Pure) = f(keys)
      override def toString: String = s"AsyncSource($n)"

/** THE EFFECT OF DATA. `Get` a key of a source; `All` and `Both` independent programs — the applicative product:
 * their fetches meet in one round. The executor answers them, a round at a time (`Executor`) */
enum Fetch[X]:
  case Get[K, V](source: DataSource[K, V], key: K) extends Fetch[V]
  case All[A](ps: List[A ! Fetches]) extends Fetch[List[A]]
  case Both[A, B](a: A ! Fetches, b: B ! Fetches) extends Fetch[(A, B)]

/** what a resolver may do: fetch, nothing else */
type Fetches = Fetch +: Pure

object Fetch:
  /** the value of `key` in `source`: batched with every other key of it asked in the same round */
  def get[K, V](source: DataSource[K, V], key: K): V ! Fetches = perform[Fetch, Fetches, V](Get(source, key))
  /** independent programs, their fetches in the same rounds: the values in order */
  def all[A](ps: List[A ! Fetches]): List[A] ! Fetches = perform[Fetch, Fetches, List[A]](All(ps))
  /** a key each, one round */
  def traverse[A, B](as: List[A])(f: A => B ! Fetches): List[B] ! Fetches = all(as.map(f))
  /** two independent programs, their fetches in the same rounds */
  def both[A, B](a: A ! Fetches, b: B ! Fetches): (A, B) ! Fetches = perform[Fetch, Fetches, (A, B)](Both(a, b))
