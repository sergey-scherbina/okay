package okay.kernel

/**
 * SemVer 2.0 without build metadata — the unit of every contract here
 * (specs/kernel.md). A pre-release orders BELOW its release
 * (`1.2.0-rc.1 < 1.2.0`); two pre-releases compare as strings, which is
 * all a plugin set ever needs of them.
 */
final case class Version(major: Int, minor: Int, patch: Int = 0, pre: String = "")
  extends Ordered[Version]:
  require(major >= 0 && minor >= 0 && patch >= 0, s"a version part is negative: $this")

  def compare(that: Version): Int =
    val c = Ordering[(Int, Int, Int)].compare((major, minor, patch), (that.major, that.minor, that.patch))
    if c != 0 then c
    else if pre == that.pre then 0
    else if pre.isEmpty then 1
    else if that.pre.isEmpty then -1
    else pre.compareTo(that.pre)

  override def toString: String =
    s"$major.$minor.$patch" + (if pre.isEmpty then "" else s"-$pre")

object Version:
  private val Shape = """(\d+)(?:\.(\d+))?(?:\.(\d+))?(?:-([0-9A-Za-z.-]+))?""".r

  /** `1`, `1.2`, `1.2.3`, `1.2.3-rc.1`; anything else is refused by name */
  def parse(s: String): Either[String, Version] = s.trim match
    case Shape(ma, mi, pa, pre) =>
      try Right(Version(ma.toInt, Option(mi).fold(0)(_.toInt), Option(pa).fold(0)(_.toInt),
        Option(pre).getOrElse("")))
      catch case _: NumberFormatException => Left(s"not a version: '$s' (a part is too large)")
    case _ => Left(s"not a version: '$s' (expected 1, 1.2, 1.2.3 or 1.2.3-pre)")

  /** for a literal the code states: a bad one is a programming error */
  def apply(s: String): Version = parse(s).fold(e => throw IllegalArgumentException(e), identity)

/**
 * What a requirer accepts. `^1.2` is the default: same major, at least
 * 1.2 — and on 0.x same MINOR, since before 1.0 a minor is allowed to
 * break (SemVer §4), which is also what npm and Cargo mean by caret.
 */
enum Range:
  case Caret(from: Version)
  case Exact(at: Version)
  case Between(from: Version, below: Version)
  case Any

  def accepts(v: Version): Boolean = this match
    case Caret(f) =>
      v >= f && v.major == f.major && (f.major > 0 || v.minor == f.minor)
    case Exact(a) => v == a
    case Between(f, b) => v >= f && v < b
    case Any => true

  override def toString: String = this match
    case Caret(f) => s"^$f"
    case Exact(a) => s"=$a"
    case Between(f, b) => s">=$f <$b"
    case Any => "*"

object Range:
  private val Between2 = """>=\s*(\S+)\s+<\s*(\S+)""".r

  /** `^1.2`, `=1.2.3`, `>=1.2 <2`, `*` */
  def parse(s: String): Either[String, Range] = s.trim match
    case "*" => Right(Any)
    case t if t.startsWith("^") => Version.parse(t.drop(1)).map(Caret(_))
    case t if t.startsWith("=") => Version.parse(t.drop(1)).map(Exact(_))
    case Between2(f, b) => for a <- Version.parse(f); z <- Version.parse(b) yield Between(a, z)
    case _ => Left(s"not a range: '$s' (expected ^1.2, =1.2.3, >=1.2 <2 or *)")

  def apply(s: String): Range = parse(s).fold(e => throw IllegalArgumentException(e), identity)
