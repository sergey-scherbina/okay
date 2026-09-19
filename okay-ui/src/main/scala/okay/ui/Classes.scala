package okay.ui

/**
 * WHAT A STYLE TOKEN IS CALLED, said once.
 *
 * `Style` carries six tokens and each becomes a class name. That
 * mapping was written twice — in `React.elem`, which the scriptless
 * road renders through, and in `LiveJs.build`, which the socket road
 * runs in the browser — and two spellings of one table is the shape
 * of a defect that shows up as a page that looks right one way and
 * wrong the other.
 *
 * So the table is here, once, and both roads read it: `React.elem`
 * calls `of`, and `LiveJs` EMITS the JavaScript from the same rows
 * through okay-js, the way it already emits the vocabulary rather
 * than typing it.
 *
 * **The bound is deliberate.** Only the part that is genuinely a NAME
 * TABLE lives here. A `Table`'s column percentages and a `Box`'s
 * weights are arithmetic, and arithmetic in a table is how a table
 * becomes a program — those stay hand-written on both sides, and what
 * keeps them honest is `TestLiveJsDom`, which executes the browser
 * client and compares the DOM it built with what `React.elem`
 * describes.
 */
object Classes:

  /** the prefix every class in this vocabulary carries */
  val Prefix = "okay-"

  /**
   * A FLAG: on or off, and the class it means when on.
   *
   * The name is the field's, which is also the JSON field's, because
   * `Protocol` derives `Schema[Style]` from the case class — so the
   * browser reads `st.bold` for the same reason Scala reads
   * `style.bold`.
   */
  val flags: Vector[(String, Style => Boolean)] = Vector(
    "bold" -> (_.bold),
    "dim" -> (_.dim))

  /**
   * AN ENUM: a field, the value that means "say nothing", and how to
   * read it. The class is the prefix, the field and the value —
   * `okay-tone-danger`.
   */
  final case class Choice(field: String, none: String, read: Style => String)

  val choices: Vector[Choice] = Vector(
    Choice("tone", "plain", _.tone.toString.toLowerCase),
    Choice("size", "normal", _.size.toString.toLowerCase),
    Choice("kind", "prose", _.kind.toString.toLowerCase),
    Choice("align", "start", _.align.toString.toLowerCase))

  /** every class a style means, in the order both roads write them */
  def of(style: Style): Vector[String] =
    flags.collect { case (name, on) if on(style) => Prefix + name } ++
      choices.flatMap { c =>
        val v = c.read(style)
        Option.when(v != c.none)(s"$Prefix${c.field}-$v")
      }
