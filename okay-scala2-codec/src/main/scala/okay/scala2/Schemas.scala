package okay.scala2

import okay.codec.Schema
import scala.reflect.ClassTag

/**
 * `derives Schema` for Scala 2.13 (specs/scala2-facade.md, stage 6).
 *
 * okay-codec derives a `Schema` with a Scala 3 Mirror macro, which a
 * Scala 2 compiler cannot run. Everything else about a `Schema` it CAN
 * use: the enum, its cases, `wrap`/`refine`/`enumeration`, and the
 * given instances (Scala 2's implicit search finds them). So this
 * object only builds the two nodes the derivation would have built:
 *
 * {{{
 * final case class Person(name: String, age: Int)
 * implicit val person: Schema[Person] =
 *   Schemas.product2("Person", "name", "age")(Person.apply)(p => (p.name, p.age))
 * }}}
 *
 * in the style of circe's `forProductN`: the field names, the
 * companion's `apply` (which fixes the field types), and the way back
 * to a tuple. Field schemas are implicit and BY-NAME, so a recursive
 * type's schema can be an `implicit lazy val` that mentions itself.
 *
 * `product1` to `product16` are generated, one shape; see the bottom
 * of this file for the generator's rule.
 */
object Schemas {

  /**
   * THE CAST, and why it is right. `Schema.SProduct` hands its `make`
   * the decoded fields as a `Seq[Any]`, in field order, each decoded by
   * the schema declared at that position. So position `i` holds a value
   * of the type whose schema is at position `i`, which is `F`. Scala 3's
   * own derivation performs the same cast.
   */
  private def field[F](xs: Seq[Any], i: Int): F = xs(i).asInstanceOf[F]

  /** a case object, or any value that carries no fields */
  def constant[A](name: String, value: A): Schema[A] =
    Schema.SProduct[A](name, Vector.empty, _ => value, _ => Seq.empty)

  def product1[A, F1](name: String, n1: String)(make: F1 => A)(parts: A => F1)
    (using s1: => Schema[F1]): Schema[A] =
    Schema.SProduct[A](name, Vector((n1, Schema.once(s1))),
      xs => make(field[F1](xs, 0)), a => Seq(parts(a)))

  def product2[A, F1, F2](name: String, n1: String, n2: String)(make: (F1, F2) => A)(parts: A => (F1, F2))
    (using s1: => Schema[F1], s2: => Schema[F2]): Schema[A] =
    Schema.SProduct[A](name, Vector((n1, Schema.once(s1)), (n2, Schema.once(s2))),
      xs => make(field[F1](xs, 0), field[F2](xs, 1)), a => { val t = parts(a); Seq(t._1, t._2) })

  def product3[A, F1, F2, F3](name: String, n1: String, n2: String, n3: String)(make: (F1, F2, F3) => A)(parts: A => (F1, F2, F3))
    (using s1: => Schema[F1], s2: => Schema[F2], s3: => Schema[F3]): Schema[A] =
    Schema.SProduct[A](name, Vector((n1, Schema.once(s1)), (n2, Schema.once(s2)), (n3, Schema.once(s3))),
      xs => make(field[F1](xs, 0), field[F2](xs, 1), field[F3](xs, 2)), a => { val t = parts(a); Seq(t._1, t._2, t._3) })

  def product4[A, F1, F2, F3, F4](name: String, n1: String, n2: String, n3: String, n4: String)(make: (F1, F2, F3, F4) => A)(parts: A => (F1, F2, F3, F4))
    (using s1: => Schema[F1], s2: => Schema[F2], s3: => Schema[F3], s4: => Schema[F4]): Schema[A] =
    Schema.SProduct[A](name, Vector((n1, Schema.once(s1)), (n2, Schema.once(s2)), (n3, Schema.once(s3)), (n4, Schema.once(s4))),
      xs => make(field[F1](xs, 0), field[F2](xs, 1), field[F3](xs, 2), field[F4](xs, 3)), a => { val t = parts(a); Seq(t._1, t._2, t._3, t._4) })

  def product5[A, F1, F2, F3, F4, F5](name: String, n1: String, n2: String, n3: String, n4: String, n5: String)(make: (F1, F2, F3, F4, F5) => A)(parts: A => (F1, F2, F3, F4, F5))
    (using s1: => Schema[F1], s2: => Schema[F2], s3: => Schema[F3], s4: => Schema[F4], s5: => Schema[F5]): Schema[A] =
    Schema.SProduct[A](name, Vector((n1, Schema.once(s1)), (n2, Schema.once(s2)), (n3, Schema.once(s3)), (n4, Schema.once(s4)), (n5, Schema.once(s5))),
      xs => make(field[F1](xs, 0), field[F2](xs, 1), field[F3](xs, 2), field[F4](xs, 3), field[F5](xs, 4)), a => { val t = parts(a); Seq(t._1, t._2, t._3, t._4, t._5) })

  def product6[A, F1, F2, F3, F4, F5, F6](name: String, n1: String, n2: String, n3: String, n4: String, n5: String, n6: String)(make: (F1, F2, F3, F4, F5, F6) => A)(parts: A => (F1, F2, F3, F4, F5, F6))
    (using s1: => Schema[F1], s2: => Schema[F2], s3: => Schema[F3], s4: => Schema[F4], s5: => Schema[F5], s6: => Schema[F6]): Schema[A] =
    Schema.SProduct[A](name, Vector((n1, Schema.once(s1)), (n2, Schema.once(s2)), (n3, Schema.once(s3)), (n4, Schema.once(s4)), (n5, Schema.once(s5)), (n6, Schema.once(s6))),
      xs => make(field[F1](xs, 0), field[F2](xs, 1), field[F3](xs, 2), field[F4](xs, 3), field[F5](xs, 4), field[F6](xs, 5)), a => { val t = parts(a); Seq(t._1, t._2, t._3, t._4, t._5, t._6) })

  def product7[A, F1, F2, F3, F4, F5, F6, F7](name: String, n1: String, n2: String, n3: String, n4: String, n5: String, n6: String, n7: String)(make: (F1, F2, F3, F4, F5, F6, F7) => A)(parts: A => (F1, F2, F3, F4, F5, F6, F7))
    (using s1: => Schema[F1], s2: => Schema[F2], s3: => Schema[F3], s4: => Schema[F4], s5: => Schema[F5], s6: => Schema[F6], s7: => Schema[F7]): Schema[A] =
    Schema.SProduct[A](name, Vector((n1, Schema.once(s1)), (n2, Schema.once(s2)), (n3, Schema.once(s3)), (n4, Schema.once(s4)), (n5, Schema.once(s5)), (n6, Schema.once(s6)), (n7, Schema.once(s7))),
      xs => make(field[F1](xs, 0), field[F2](xs, 1), field[F3](xs, 2), field[F4](xs, 3), field[F5](xs, 4), field[F6](xs, 5), field[F7](xs, 6)), a => { val t = parts(a); Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7) })

  def product8[A, F1, F2, F3, F4, F5, F6, F7, F8](name: String, n1: String, n2: String, n3: String, n4: String, n5: String, n6: String, n7: String, n8: String)(make: (F1, F2, F3, F4, F5, F6, F7, F8) => A)(parts: A => (F1, F2, F3, F4, F5, F6, F7, F8))
    (using s1: => Schema[F1], s2: => Schema[F2], s3: => Schema[F3], s4: => Schema[F4], s5: => Schema[F5], s6: => Schema[F6], s7: => Schema[F7], s8: => Schema[F8]): Schema[A] =
    Schema.SProduct[A](name, Vector((n1, Schema.once(s1)), (n2, Schema.once(s2)), (n3, Schema.once(s3)), (n4, Schema.once(s4)), (n5, Schema.once(s5)), (n6, Schema.once(s6)), (n7, Schema.once(s7)), (n8, Schema.once(s8))),
      xs => make(field[F1](xs, 0), field[F2](xs, 1), field[F3](xs, 2), field[F4](xs, 3), field[F5](xs, 4), field[F6](xs, 5), field[F7](xs, 6), field[F8](xs, 7)), a => { val t = parts(a); Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8) })

  def product9[A, F1, F2, F3, F4, F5, F6, F7, F8, F9](name: String, n1: String, n2: String, n3: String, n4: String, n5: String, n6: String, n7: String, n8: String, n9: String)(make: (F1, F2, F3, F4, F5, F6, F7, F8, F9) => A)(parts: A => (F1, F2, F3, F4, F5, F6, F7, F8, F9))
    (using s1: => Schema[F1], s2: => Schema[F2], s3: => Schema[F3], s4: => Schema[F4], s5: => Schema[F5], s6: => Schema[F6], s7: => Schema[F7], s8: => Schema[F8], s9: => Schema[F9]): Schema[A] =
    Schema.SProduct[A](name, Vector((n1, Schema.once(s1)), (n2, Schema.once(s2)), (n3, Schema.once(s3)), (n4, Schema.once(s4)), (n5, Schema.once(s5)), (n6, Schema.once(s6)), (n7, Schema.once(s7)), (n8, Schema.once(s8)), (n9, Schema.once(s9))),
      xs => make(field[F1](xs, 0), field[F2](xs, 1), field[F3](xs, 2), field[F4](xs, 3), field[F5](xs, 4), field[F6](xs, 5), field[F7](xs, 6), field[F8](xs, 7), field[F9](xs, 8)), a => { val t = parts(a); Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9) })

  def product10[A, F1, F2, F3, F4, F5, F6, F7, F8, F9, F10](name: String, n1: String, n2: String, n3: String, n4: String, n5: String, n6: String, n7: String, n8: String, n9: String, n10: String)(make: (F1, F2, F3, F4, F5, F6, F7, F8, F9, F10) => A)(parts: A => (F1, F2, F3, F4, F5, F6, F7, F8, F9, F10))
    (using s1: => Schema[F1], s2: => Schema[F2], s3: => Schema[F3], s4: => Schema[F4], s5: => Schema[F5], s6: => Schema[F6], s7: => Schema[F7], s8: => Schema[F8], s9: => Schema[F9], s10: => Schema[F10]): Schema[A] =
    Schema.SProduct[A](name, Vector((n1, Schema.once(s1)), (n2, Schema.once(s2)), (n3, Schema.once(s3)), (n4, Schema.once(s4)), (n5, Schema.once(s5)), (n6, Schema.once(s6)), (n7, Schema.once(s7)), (n8, Schema.once(s8)), (n9, Schema.once(s9)), (n10, Schema.once(s10))),
      xs => make(field[F1](xs, 0), field[F2](xs, 1), field[F3](xs, 2), field[F4](xs, 3), field[F5](xs, 4), field[F6](xs, 5), field[F7](xs, 6), field[F8](xs, 7), field[F9](xs, 8), field[F10](xs, 9)), a => { val t = parts(a); Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10) })

  def product11[A, F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11](name: String, n1: String, n2: String, n3: String, n4: String, n5: String, n6: String, n7: String, n8: String, n9: String, n10: String, n11: String)(make: (F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11) => A)(parts: A => (F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11))
    (using s1: => Schema[F1], s2: => Schema[F2], s3: => Schema[F3], s4: => Schema[F4], s5: => Schema[F5], s6: => Schema[F6], s7: => Schema[F7], s8: => Schema[F8], s9: => Schema[F9], s10: => Schema[F10], s11: => Schema[F11]): Schema[A] =
    Schema.SProduct[A](name, Vector((n1, Schema.once(s1)), (n2, Schema.once(s2)), (n3, Schema.once(s3)), (n4, Schema.once(s4)), (n5, Schema.once(s5)), (n6, Schema.once(s6)), (n7, Schema.once(s7)), (n8, Schema.once(s8)), (n9, Schema.once(s9)), (n10, Schema.once(s10)), (n11, Schema.once(s11))),
      xs => make(field[F1](xs, 0), field[F2](xs, 1), field[F3](xs, 2), field[F4](xs, 3), field[F5](xs, 4), field[F6](xs, 5), field[F7](xs, 6), field[F8](xs, 7), field[F9](xs, 8), field[F10](xs, 9), field[F11](xs, 10)), a => { val t = parts(a); Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11) })

  def product12[A, F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12](name: String, n1: String, n2: String, n3: String, n4: String, n5: String, n6: String, n7: String, n8: String, n9: String, n10: String, n11: String, n12: String)(make: (F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12) => A)(parts: A => (F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12))
    (using s1: => Schema[F1], s2: => Schema[F2], s3: => Schema[F3], s4: => Schema[F4], s5: => Schema[F5], s6: => Schema[F6], s7: => Schema[F7], s8: => Schema[F8], s9: => Schema[F9], s10: => Schema[F10], s11: => Schema[F11], s12: => Schema[F12]): Schema[A] =
    Schema.SProduct[A](name, Vector((n1, Schema.once(s1)), (n2, Schema.once(s2)), (n3, Schema.once(s3)), (n4, Schema.once(s4)), (n5, Schema.once(s5)), (n6, Schema.once(s6)), (n7, Schema.once(s7)), (n8, Schema.once(s8)), (n9, Schema.once(s9)), (n10, Schema.once(s10)), (n11, Schema.once(s11)), (n12, Schema.once(s12))),
      xs => make(field[F1](xs, 0), field[F2](xs, 1), field[F3](xs, 2), field[F4](xs, 3), field[F5](xs, 4), field[F6](xs, 5), field[F7](xs, 6), field[F8](xs, 7), field[F9](xs, 8), field[F10](xs, 9), field[F11](xs, 10), field[F12](xs, 11)), a => { val t = parts(a); Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12) })

  def product13[A, F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13](name: String, n1: String, n2: String, n3: String, n4: String, n5: String, n6: String, n7: String, n8: String, n9: String, n10: String, n11: String, n12: String, n13: String)(make: (F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13) => A)(parts: A => (F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13))
    (using s1: => Schema[F1], s2: => Schema[F2], s3: => Schema[F3], s4: => Schema[F4], s5: => Schema[F5], s6: => Schema[F6], s7: => Schema[F7], s8: => Schema[F8], s9: => Schema[F9], s10: => Schema[F10], s11: => Schema[F11], s12: => Schema[F12], s13: => Schema[F13]): Schema[A] =
    Schema.SProduct[A](name, Vector((n1, Schema.once(s1)), (n2, Schema.once(s2)), (n3, Schema.once(s3)), (n4, Schema.once(s4)), (n5, Schema.once(s5)), (n6, Schema.once(s6)), (n7, Schema.once(s7)), (n8, Schema.once(s8)), (n9, Schema.once(s9)), (n10, Schema.once(s10)), (n11, Schema.once(s11)), (n12, Schema.once(s12)), (n13, Schema.once(s13))),
      xs => make(field[F1](xs, 0), field[F2](xs, 1), field[F3](xs, 2), field[F4](xs, 3), field[F5](xs, 4), field[F6](xs, 5), field[F7](xs, 6), field[F8](xs, 7), field[F9](xs, 8), field[F10](xs, 9), field[F11](xs, 10), field[F12](xs, 11), field[F13](xs, 12)), a => { val t = parts(a); Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13) })

  def product14[A, F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14](name: String, n1: String, n2: String, n3: String, n4: String, n5: String, n6: String, n7: String, n8: String, n9: String, n10: String, n11: String, n12: String, n13: String, n14: String)(make: (F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14) => A)(parts: A => (F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14))
    (using s1: => Schema[F1], s2: => Schema[F2], s3: => Schema[F3], s4: => Schema[F4], s5: => Schema[F5], s6: => Schema[F6], s7: => Schema[F7], s8: => Schema[F8], s9: => Schema[F9], s10: => Schema[F10], s11: => Schema[F11], s12: => Schema[F12], s13: => Schema[F13], s14: => Schema[F14]): Schema[A] =
    Schema.SProduct[A](name, Vector((n1, Schema.once(s1)), (n2, Schema.once(s2)), (n3, Schema.once(s3)), (n4, Schema.once(s4)), (n5, Schema.once(s5)), (n6, Schema.once(s6)), (n7, Schema.once(s7)), (n8, Schema.once(s8)), (n9, Schema.once(s9)), (n10, Schema.once(s10)), (n11, Schema.once(s11)), (n12, Schema.once(s12)), (n13, Schema.once(s13)), (n14, Schema.once(s14))),
      xs => make(field[F1](xs, 0), field[F2](xs, 1), field[F3](xs, 2), field[F4](xs, 3), field[F5](xs, 4), field[F6](xs, 5), field[F7](xs, 6), field[F8](xs, 7), field[F9](xs, 8), field[F10](xs, 9), field[F11](xs, 10), field[F12](xs, 11), field[F13](xs, 12), field[F14](xs, 13)), a => { val t = parts(a); Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14) })

  def product15[A, F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15](name: String, n1: String, n2: String, n3: String, n4: String, n5: String, n6: String, n7: String, n8: String, n9: String, n10: String, n11: String, n12: String, n13: String, n14: String, n15: String)(make: (F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15) => A)(parts: A => (F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15))
    (using s1: => Schema[F1], s2: => Schema[F2], s3: => Schema[F3], s4: => Schema[F4], s5: => Schema[F5], s6: => Schema[F6], s7: => Schema[F7], s8: => Schema[F8], s9: => Schema[F9], s10: => Schema[F10], s11: => Schema[F11], s12: => Schema[F12], s13: => Schema[F13], s14: => Schema[F14], s15: => Schema[F15]): Schema[A] =
    Schema.SProduct[A](name, Vector((n1, Schema.once(s1)), (n2, Schema.once(s2)), (n3, Schema.once(s3)), (n4, Schema.once(s4)), (n5, Schema.once(s5)), (n6, Schema.once(s6)), (n7, Schema.once(s7)), (n8, Schema.once(s8)), (n9, Schema.once(s9)), (n10, Schema.once(s10)), (n11, Schema.once(s11)), (n12, Schema.once(s12)), (n13, Schema.once(s13)), (n14, Schema.once(s14)), (n15, Schema.once(s15))),
      xs => make(field[F1](xs, 0), field[F2](xs, 1), field[F3](xs, 2), field[F4](xs, 3), field[F5](xs, 4), field[F6](xs, 5), field[F7](xs, 6), field[F8](xs, 7), field[F9](xs, 8), field[F10](xs, 9), field[F11](xs, 10), field[F12](xs, 11), field[F13](xs, 12), field[F14](xs, 13), field[F15](xs, 14)), a => { val t = parts(a); Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15) })

  def product16[A, F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16](name: String, n1: String, n2: String, n3: String, n4: String, n5: String, n6: String, n7: String, n8: String, n9: String, n10: String, n11: String, n12: String, n13: String, n14: String, n15: String, n16: String)(make: (F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16) => A)(parts: A => (F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14, F15, F16))
    (using s1: => Schema[F1], s2: => Schema[F2], s3: => Schema[F3], s4: => Schema[F4], s5: => Schema[F5], s6: => Schema[F6], s7: => Schema[F7], s8: => Schema[F8], s9: => Schema[F9], s10: => Schema[F10], s11: => Schema[F11], s12: => Schema[F12], s13: => Schema[F13], s14: => Schema[F14], s15: => Schema[F15], s16: => Schema[F16]): Schema[A] =
    Schema.SProduct[A](name, Vector((n1, Schema.once(s1)), (n2, Schema.once(s2)), (n3, Schema.once(s3)), (n4, Schema.once(s4)), (n5, Schema.once(s5)), (n6, Schema.once(s6)), (n7, Schema.once(s7)), (n8, Schema.once(s8)), (n9, Schema.once(s9)), (n10, Schema.once(s10)), (n11, Schema.once(s11)), (n12, Schema.once(s12)), (n13, Schema.once(s13)), (n14, Schema.once(s14)), (n15, Schema.once(s15)), (n16, Schema.once(s16))),
      xs => make(field[F1](xs, 0), field[F2](xs, 1), field[F3](xs, 2), field[F4](xs, 3), field[F5](xs, 4), field[F6](xs, 5), field[F7](xs, 6), field[F8](xs, 7), field[F9](xs, 8), field[F10](xs, 9), field[F11](xs, 10), field[F12](xs, 11), field[F13](xs, 12), field[F14](xs, 13), field[F15](xs, 14), field[F16](xs, 15)), a => { val t = parts(a); Seq(t._1, t._2, t._3, t._4, t._5, t._6, t._7, t._8, t._9, t._10, t._11, t._12, t._13, t._14, t._15, t._16) })

  /** one case of a sealed hierarchy: its name on the wire, its schema,
   * and the class that recognises it when encoding */
  final class Variant[A] private[Schemas] (val name: String, val schema: () => Schema[? <: A], val is: A => Boolean)

  def variant[A, C <: A](name: String)(using s: => Schema[C], tag: ClassTag[C]): Variant[A] =
    new Variant[A](name, Schema.once(s), a => tag.runtimeClass.isInstance(a))

  /** a sealed hierarchy, as its cases: the first whose class matches
   * the value encodes it */
  def sum[A](name: String)(variants: Variant[A]*): Schema[A] = {
    val vs = variants.toVector
    Schema.SSum[A](name, vs.map(v => (v.name, v.schema)), a => vs.indexWhere(_.is(a)))
  }

  // generator: productN takes N names, an N-ary `make`, a projection to
  // an N-tuple (the value itself for N = 1), and N by-name implicit
  // field schemas; position i of `make` reads field[Fi](xs, i - 1).
}
