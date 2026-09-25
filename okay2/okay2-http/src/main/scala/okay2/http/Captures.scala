package okay2.http

/**
 * What a route captures, in Scala 2 terms (the Scala 3 core types a
 * route's captures as a TUPLE and joins two with `Route.Split` by
 * induction over `*:`).
 *
 * A route's type argument is the capture's OUTSIDE form, the one a
 * handler, `url` and `unapply` use: `Unit` for none, the value itself
 * for one, `(A, B, ...)` for several, up to 8. Joining two routes needs
 * the INSIDE form, a list built by induction, so each outside form has
 * one inside form (`Captures`), the list comes back out (`Tupled`), and
 * `Split` joins through both (`Concat` on the lists).
 *
 * The bijection holds because no captured value is itself a tuple: a
 * capture is a `Param` (strings, numbers, booleans, what a user adds)
 * or its `Option`/`Vector`. A tuple type argument therefore always means
 * several captures, and the tuple instances outrank the single one.
 */
sealed trait HList
final case class :*:[+H, +T <: HList](head: H, tail: T) extends HList
sealed trait HNil extends HList
case object HNil extends HNil

/** the inside form of an outside capture type */
@scala.annotation.implicitNotFound("a route captures Unit, one value, or a tuple of up to 8; ${A} is none of these")
trait Captures[A] {
  type L <: HList
  def toList(a: A): L
  def fromList(l: L): A
}

object Captures extends CapturesOne {
  type Aux[A, L0 <: HList] = Captures[A] { type L = L0 }

  /** the instance WITH its refinement, which `implicitly` drops */
  def apply[A](implicit c: Captures[A]): Aux[A, c.L] = c

  implicit val none: Aux[Unit, HNil] = new Captures[Unit] {
    type L = HNil
    def toList(a: Unit): HNil = HNil
    def fromList(l: HNil): Unit = ()
  }
  implicit def tuple2[T1, T2]: Aux[(T1, T2), T1 :*: T2 :*: HNil] = new Captures[(T1, T2)] {
    type L = T1 :*: T2 :*: HNil
    def toList(a: (T1, T2)): L = :*:(a._1, :*:(a._2, HNil))
    def fromList(l: L): (T1, T2) = l match { case :*:(x1, :*:(x2, HNil)) => (x1, x2) }
  }
  implicit def tuple3[T1, T2, T3]: Aux[(T1, T2, T3), T1 :*: T2 :*: T3 :*: HNil] = new Captures[(T1, T2, T3)] {
    type L = T1 :*: T2 :*: T3 :*: HNil
    def toList(a: (T1, T2, T3)): L = :*:(a._1, :*:(a._2, :*:(a._3, HNil)))
    def fromList(l: L): (T1, T2, T3) = l match { case :*:(x1, :*:(x2, :*:(x3, HNil))) => (x1, x2, x3) }
  }
  implicit def tuple4[T1, T2, T3, T4]: Aux[(T1, T2, T3, T4), T1 :*: T2 :*: T3 :*: T4 :*: HNil] = new Captures[(T1, T2, T3, T4)] {
    type L = T1 :*: T2 :*: T3 :*: T4 :*: HNil
    def toList(a: (T1, T2, T3, T4)): L = :*:(a._1, :*:(a._2, :*:(a._3, :*:(a._4, HNil))))
    def fromList(l: L): (T1, T2, T3, T4) = l match { case :*:(x1, :*:(x2, :*:(x3, :*:(x4, HNil)))) => (x1, x2, x3, x4) }
  }
  implicit def tuple5[T1, T2, T3, T4, T5]: Aux[(T1, T2, T3, T4, T5), T1 :*: T2 :*: T3 :*: T4 :*: T5 :*: HNil] = new Captures[(T1, T2, T3, T4, T5)] {
    type L = T1 :*: T2 :*: T3 :*: T4 :*: T5 :*: HNil
    def toList(a: (T1, T2, T3, T4, T5)): L = :*:(a._1, :*:(a._2, :*:(a._3, :*:(a._4, :*:(a._5, HNil)))))
    def fromList(l: L): (T1, T2, T3, T4, T5) = l match { case :*:(x1, :*:(x2, :*:(x3, :*:(x4, :*:(x5, HNil))))) => (x1, x2, x3, x4, x5) }
  }
  implicit def tuple6[T1, T2, T3, T4, T5, T6]: Aux[(T1, T2, T3, T4, T5, T6), T1 :*: T2 :*: T3 :*: T4 :*: T5 :*: T6 :*: HNil] = new Captures[(T1, T2, T3, T4, T5, T6)] {
    type L = T1 :*: T2 :*: T3 :*: T4 :*: T5 :*: T6 :*: HNil
    def toList(a: (T1, T2, T3, T4, T5, T6)): L = :*:(a._1, :*:(a._2, :*:(a._3, :*:(a._4, :*:(a._5, :*:(a._6, HNil))))))
    def fromList(l: L): (T1, T2, T3, T4, T5, T6) = l match { case :*:(x1, :*:(x2, :*:(x3, :*:(x4, :*:(x5, :*:(x6, HNil)))))) => (x1, x2, x3, x4, x5, x6) }
  }
  implicit def tuple7[T1, T2, T3, T4, T5, T6, T7]: Aux[(T1, T2, T3, T4, T5, T6, T7), T1 :*: T2 :*: T3 :*: T4 :*: T5 :*: T6 :*: T7 :*: HNil] = new Captures[(T1, T2, T3, T4, T5, T6, T7)] {
    type L = T1 :*: T2 :*: T3 :*: T4 :*: T5 :*: T6 :*: T7 :*: HNil
    def toList(a: (T1, T2, T3, T4, T5, T6, T7)): L = :*:(a._1, :*:(a._2, :*:(a._3, :*:(a._4, :*:(a._5, :*:(a._6, :*:(a._7, HNil)))))))
    def fromList(l: L): (T1, T2, T3, T4, T5, T6, T7) = l match { case :*:(x1, :*:(x2, :*:(x3, :*:(x4, :*:(x5, :*:(x6, :*:(x7, HNil))))))) => (x1, x2, x3, x4, x5, x6, x7) }
  }
  implicit def tuple8[T1, T2, T3, T4, T5, T6, T7, T8]: Aux[(T1, T2, T3, T4, T5, T6, T7, T8), T1 :*: T2 :*: T3 :*: T4 :*: T5 :*: T6 :*: T7 :*: T8 :*: HNil] = new Captures[(T1, T2, T3, T4, T5, T6, T7, T8)] {
    type L = T1 :*: T2 :*: T3 :*: T4 :*: T5 :*: T6 :*: T7 :*: T8 :*: HNil
    def toList(a: (T1, T2, T3, T4, T5, T6, T7, T8)): L = :*:(a._1, :*:(a._2, :*:(a._3, :*:(a._4, :*:(a._5, :*:(a._6, :*:(a._7, :*:(a._8, HNil))))))))
    def fromList(l: L): (T1, T2, T3, T4, T5, T6, T7, T8) = l match { case :*:(x1, :*:(x2, :*:(x3, :*:(x4, :*:(x5, :*:(x6, :*:(x7, :*:(x8, HNil)))))))) => (x1, x2, x3, x4, x5, x6, x7, x8) }
  }
}

/** the single capture: a value that is not a tuple, at LOWER priority
 * than the tuple instances */
trait CapturesOne {
  implicit def one[T]: Captures.Aux[T, T :*: HNil] = new Captures[T] {
    type L = T :*: HNil
    def toList(a: T): L = :*:(a, HNil)
    def fromList(l: L): T = l.head
  }
}

/** a list back to its outside form: the inverse of `Captures` */
@scala.annotation.implicitNotFound("a route captures at most 8 values; ${L} is longer")
trait Tupled[L <: HList] {
  type Out
  def fromList(l: L): Out
  def toList(o: Out): L
}

object Tupled {
  type Aux[L <: HList, O] = Tupled[L] { type Out = O }

  implicit val none: Aux[HNil, Unit] = new Tupled[HNil] {
    type Out = Unit
    def fromList(l: HNil): Unit = ()
    def toList(o: Unit): HNil = HNil
  }

  implicit def one[T]: Aux[T :*: HNil, T] = new Tupled[T :*: HNil] {
    type Out = T
    def fromList(l: T :*: HNil): T = l.head
    def toList(o: T): T :*: HNil = :*:(o, HNil)
  }
  implicit def tuple2[T1, T2]: Aux[T1 :*: T2 :*: HNil, (T1, T2)] = new Tupled[T1 :*: T2 :*: HNil] {
    type Out = (T1, T2)
    def fromList(l: T1 :*: T2 :*: HNil): (T1, T2) = l match { case :*:(x1, :*:(x2, HNil)) => (x1, x2) }
    def toList(o: (T1, T2)): T1 :*: T2 :*: HNil = :*:(o._1, :*:(o._2, HNil))
  }
  implicit def tuple3[T1, T2, T3]: Aux[T1 :*: T2 :*: T3 :*: HNil, (T1, T2, T3)] = new Tupled[T1 :*: T2 :*: T3 :*: HNil] {
    type Out = (T1, T2, T3)
    def fromList(l: T1 :*: T2 :*: T3 :*: HNil): (T1, T2, T3) = l match { case :*:(x1, :*:(x2, :*:(x3, HNil))) => (x1, x2, x3) }
    def toList(o: (T1, T2, T3)): T1 :*: T2 :*: T3 :*: HNil = :*:(o._1, :*:(o._2, :*:(o._3, HNil)))
  }
  implicit def tuple4[T1, T2, T3, T4]: Aux[T1 :*: T2 :*: T3 :*: T4 :*: HNil, (T1, T2, T3, T4)] = new Tupled[T1 :*: T2 :*: T3 :*: T4 :*: HNil] {
    type Out = (T1, T2, T3, T4)
    def fromList(l: T1 :*: T2 :*: T3 :*: T4 :*: HNil): (T1, T2, T3, T4) = l match { case :*:(x1, :*:(x2, :*:(x3, :*:(x4, HNil)))) => (x1, x2, x3, x4) }
    def toList(o: (T1, T2, T3, T4)): T1 :*: T2 :*: T3 :*: T4 :*: HNil = :*:(o._1, :*:(o._2, :*:(o._3, :*:(o._4, HNil))))
  }
  implicit def tuple5[T1, T2, T3, T4, T5]: Aux[T1 :*: T2 :*: T3 :*: T4 :*: T5 :*: HNil, (T1, T2, T3, T4, T5)] = new Tupled[T1 :*: T2 :*: T3 :*: T4 :*: T5 :*: HNil] {
    type Out = (T1, T2, T3, T4, T5)
    def fromList(l: T1 :*: T2 :*: T3 :*: T4 :*: T5 :*: HNil): (T1, T2, T3, T4, T5) = l match { case :*:(x1, :*:(x2, :*:(x3, :*:(x4, :*:(x5, HNil))))) => (x1, x2, x3, x4, x5) }
    def toList(o: (T1, T2, T3, T4, T5)): T1 :*: T2 :*: T3 :*: T4 :*: T5 :*: HNil = :*:(o._1, :*:(o._2, :*:(o._3, :*:(o._4, :*:(o._5, HNil)))))
  }
  implicit def tuple6[T1, T2, T3, T4, T5, T6]: Aux[T1 :*: T2 :*: T3 :*: T4 :*: T5 :*: T6 :*: HNil, (T1, T2, T3, T4, T5, T6)] = new Tupled[T1 :*: T2 :*: T3 :*: T4 :*: T5 :*: T6 :*: HNil] {
    type Out = (T1, T2, T3, T4, T5, T6)
    def fromList(l: T1 :*: T2 :*: T3 :*: T4 :*: T5 :*: T6 :*: HNil): (T1, T2, T3, T4, T5, T6) = l match { case :*:(x1, :*:(x2, :*:(x3, :*:(x4, :*:(x5, :*:(x6, HNil)))))) => (x1, x2, x3, x4, x5, x6) }
    def toList(o: (T1, T2, T3, T4, T5, T6)): T1 :*: T2 :*: T3 :*: T4 :*: T5 :*: T6 :*: HNil = :*:(o._1, :*:(o._2, :*:(o._3, :*:(o._4, :*:(o._5, :*:(o._6, HNil))))))
  }
  implicit def tuple7[T1, T2, T3, T4, T5, T6, T7]: Aux[T1 :*: T2 :*: T3 :*: T4 :*: T5 :*: T6 :*: T7 :*: HNil, (T1, T2, T3, T4, T5, T6, T7)] = new Tupled[T1 :*: T2 :*: T3 :*: T4 :*: T5 :*: T6 :*: T7 :*: HNil] {
    type Out = (T1, T2, T3, T4, T5, T6, T7)
    def fromList(l: T1 :*: T2 :*: T3 :*: T4 :*: T5 :*: T6 :*: T7 :*: HNil): (T1, T2, T3, T4, T5, T6, T7) = l match { case :*:(x1, :*:(x2, :*:(x3, :*:(x4, :*:(x5, :*:(x6, :*:(x7, HNil))))))) => (x1, x2, x3, x4, x5, x6, x7) }
    def toList(o: (T1, T2, T3, T4, T5, T6, T7)): T1 :*: T2 :*: T3 :*: T4 :*: T5 :*: T6 :*: T7 :*: HNil = :*:(o._1, :*:(o._2, :*:(o._3, :*:(o._4, :*:(o._5, :*:(o._6, :*:(o._7, HNil)))))))
  }
  implicit def tuple8[T1, T2, T3, T4, T5, T6, T7, T8]: Aux[T1 :*: T2 :*: T3 :*: T4 :*: T5 :*: T6 :*: T7 :*: T8 :*: HNil, (T1, T2, T3, T4, T5, T6, T7, T8)] = new Tupled[T1 :*: T2 :*: T3 :*: T4 :*: T5 :*: T6 :*: T7 :*: T8 :*: HNil] {
    type Out = (T1, T2, T3, T4, T5, T6, T7, T8)
    def fromList(l: T1 :*: T2 :*: T3 :*: T4 :*: T5 :*: T6 :*: T7 :*: T8 :*: HNil): (T1, T2, T3, T4, T5, T6, T7, T8) = l match { case :*:(x1, :*:(x2, :*:(x3, :*:(x4, :*:(x5, :*:(x6, :*:(x7, :*:(x8, HNil)))))))) => (x1, x2, x3, x4, x5, x6, x7, x8) }
    def toList(o: (T1, T2, T3, T4, T5, T6, T7, T8)): T1 :*: T2 :*: T3 :*: T4 :*: T5 :*: T6 :*: T7 :*: T8 :*: HNil = :*:(o._1, :*:(o._2, :*:(o._3, :*:(o._4, :*:(o._5, :*:(o._6, :*:(o._7, :*:(o._8, HNil))))))))
  }
}

/** two lists joined, and split back at the first one's length */
trait Concat[A <: HList, B <: HList] {
  type Out <: HList
  def join(a: A, b: B): Out
  def split(o: Out): (A, B)
}

object Concat {
  type Aux[A <: HList, B <: HList, O <: HList] = Concat[A, B] { type Out = O }

  implicit def empty[B <: HList]: Aux[HNil, B, B] = new Concat[HNil, B] {
    type Out = B
    def join(a: HNil, b: B): B = b
    def split(o: B): (HNil, B) = (HNil, o)
  }

  implicit def cons[H, T <: HList, B <: HList, O <: HList](implicit c: Aux[T, B, O]): Aux[H :*: T, B, H :*: O] =
    new Concat[H :*: T, B] {
      type Out = H :*: O
      def join(a: H :*: T, b: B): H :*: O = :*:(a.head, c.join(a.tail, b))
      def split(o: H :*: O): (H :*: T, B) = {
        val (t, b) = c.split(o.tail)
        (:*:(o.head, t), b)
      }
    }
}

/**
 * Two captures joined, in their outside forms: `Unit` and `Int` join to
 * `Int`, `Int` and `String` to `(Int, String)`, `(Int, String)` and
 * `Option[Int]` to `(Int, String, Option[Int])`. `split` is the inverse,
 * which is what `url` needs (the Scala 3 core's `Route.Split`).
 */
@scala.annotation.implicitNotFound("cannot join the captures ${A} and ${B}: a route captures at most 8 values")
trait Split[A, B] {
  type Out
  def join(a: A, b: B): Out
  def split(o: Out): (A, B)
}

object Split {
  type Aux[A, B, O] = Split[A, B] { type Out = O }

  /** the instance WITH its refinement, which `implicitly` drops */
  def apply[A, B](implicit s: Split[A, B]): Aux[A, B, s.Out] = s

  implicit def through[A, B, LA <: HList, LB <: HList, LO <: HList, O](
      implicit ca: Captures.Aux[A, LA], cb: Captures.Aux[B, LB], cat: Concat.Aux[LA, LB, LO], t: Tupled.Aux[LO, O]): Aux[A, B, O] =
    new Split[A, B] {
      type Out = O
      def join(a: A, b: B): O = t.fromList(cat.join(ca.toList(a), cb.toList(b)))
      def split(o: O): (A, B) = {
        val (la, lb) = cat.split(t.toList(o))
        (ca.fromList(la), cb.fromList(lb))
      }
    }
}
