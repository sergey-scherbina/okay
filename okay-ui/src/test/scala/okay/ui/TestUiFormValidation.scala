package okay.ui

import okay.*
import okay.given
import okay.Direct.{*, given}

/**
 * A FORM, VALIDATED, WITH ITS ERRORS PUT BACK INTO THE TREE.
 *
 * Three pieces of this library meeting on one screen: a `Ui` is a
 * value, `Validated` collects every problem instead of the first, and
 * an optic writes each message next to the field it belongs to. The
 * validation itself is a `direct` block, which needs no monad because
 * the three checks do not depend on each other.
 */
class TestUiFormValidation extends munit.FunSuite {

  type Errors = Vector[(String, String)]   // field key -> message
  private given Semigroup[Errors] with
    def combine(x: Errors, y: Errors): Errors = x ++ y

  final case class Signup(name: String, email: String, age: Int)

  /** the form as a value */
  private val blank = Ui.Form(Vector(
    Ui.Input("", "name", "Name"),
    Ui.Input("", "email", "Email"),
    Ui.Input("", "age", "Age")), "Sign up", "signup")

  // ---- the checks, each naming the field it reports against

  private def nonEmpty(k: String, in: Map[String, String]): Validated[Errors, String] =
    in.getOrElse(k, "") match
      case "" => Validated.Invalid(Vector(k -> "cannot be empty"))
      case s => Validated.Valid(s)

  private def hasAt(k: String, in: Map[String, String]): Validated[Errors, String] =
    val s = in.getOrElse(k, "")
    if s.contains("@") then Validated.Valid(s)
    else Validated.Invalid(Vector(k -> "must contain @"))

  private def number(k: String, in: Map[String, String]): Validated[Errors, Int] =
    in.getOrElse(k, "").toIntOption match
      case Some(n) if n > 0 => Validated.Valid(n)
      case Some(n) => Validated.Invalid(Vector(k -> s"must be positive, got $n"))
      case None => Validated.Invalid(Vector(k -> "must be a whole number"))

  /** THE VALIDATION: a direct block at a carrier with no monad. No
   * check uses another's answer, so the block needs only Applicative
   * and every problem survives. */
  private def validate(in: Map[String, String]): Validated[Errors, Signup] = direct:
    val name = nonEmpty("name", in)
    val email = hasAt("email", in)
    val age = number("age", in)
    Signup(name, email, age)

  /** THE WRITE-BACK: each message goes under the field it names,
   * through the traversal that finds a node by key */
  private def withErrors(tree: Ui, errs: Errors): Ui =
    errs.foldLeft(tree) { case (t, (k, msg)) =>
      Ui.key(k).modify(field =>
        Ui.Column(Vector(field, Ui.Text(msg, Style(dim = true)))))(t)
    }

  /** and the whole screen: keep the edits, show what is wrong */
  private def screen(in: Map[String, String]): Ui =
    val filled = in.foldLeft(blank) { case (t, (k, v)) =>
      Ui.key(k).modify { case Ui.Input(_, key, lab, kind, live) => Ui.Input(v, key, lab, kind, live)
                         case other => other }(t)
    }
    validate(in) match
      case Validated.Valid(_) => filled
      case Validated.Invalid(es) => withErrors(filled, es)

  test("every problem is reported at once, each against its own field") {
    assertEquals(validate(Map("name" -> "", "email" -> "nope", "age" -> "x")),
      Validated.Invalid(Vector(
        "name" -> "cannot be empty",
        "email" -> "must contain @",
        "age" -> "must be a whole number")))
  }

  test("the messages land under the fields they name, in the tree") {
    val ui = screen(Map("name" -> "", "email" -> "a@b", "age" -> "-1"))
    // the email field kept its value and gained no message
    val texts = Ui.everywhere.toVector(ui).collect { case Ui.Text(s, _) => s }
    assertEquals(texts, Vector("cannot be empty", "must be positive, got -1"))
    val inputs = Ui.everywhere.toVector(ui).collect { case Ui.Input(v, k, _, _, _) => k -> v }
    assertEquals(inputs, Vector("name" -> "", "email" -> "a@b", "age" -> "-1"))
  }

  test("THE BOUNDARY: the focus function cannot be written in this block") {
    // An optic's traverseOf takes a plain function, and a mark UNDER a
    // lambda is what direct's v1 refuses — "the hard corner", its own
    // spec's words. So the per-focus check is an ordinary method here
    // (`nonEmpty`, `hasAt`, `number`) and not a nested block.
    //
    // Measured 2026-09-18 rather than assumed: the same walk written
    // with marks inside the lambda is refused, and a NESTED direct
    // block is refused for a second reason worth keeping apart — its
    // own binds are dependent, which genuinely needs flatMap.
    assert(compileErrors("""
      val v: Validated[Errors, Ui] = Direct.direct {
        Ui.everywhere.traverseOf { (u: Ui) =>
          val t = nonEmpty("name", Map.empty)
          Ui.Text(t)
        }(blank)
      }
    """).nonEmpty, "marks under a lambda compile now — direct's v1 corner has moved")
  }

  test("what the block DOES buy: no type lambda, and the carrier from context") {
    // `traverseOf` infers its F from the expected type, inside a block
    // and outside one alike — checked both ways, because the obvious
    // guess is that the block helps and it does not. What the block
    // buys is the three checks reading as three lines.
    val walked: Validated[Errors, Ui] =
      Ui.everywhere.traverseOf(u => Validated.Valid(u): Validated[Errors, Ui])(blank)
    assertEquals(walked, Validated.Valid(blank))
  }

  test("a good form is returned unchanged, with the edits kept") {
    val in = Map("name" -> "Ada", "email" -> "ada@example.com", "age" -> "36")
    assertEquals(validate(in), Validated.Valid(Signup("Ada", "ada@example.com", 36)))
    assertEquals(Ui.everywhere.toVector(screen(in)).collect { case Ui.Text(s, _) => s }, Vector.empty)
  }
}
