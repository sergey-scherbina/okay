package okay.ui

/**
 * One table for the six style tokens (ui-class-table).
 *
 * The value of this is bounded and worth stating: the browser client
 * no longer spells these names a second time, so they cannot drift.
 * What keeps the REST of that client honest is `TestLiveJsDom`, which
 * executes it — a shared table could never check a `Table`'s column
 * percentages, because those are arithmetic and a table that holds
 * arithmetic is a program.
 */
class TestClasses extends munit.FunSuite:

  test("a plain style says nothing at all") {
    assertEquals(Classes.of(Style.none), Vector.empty)
  }

  test("each token on its own") {
    assertEquals(Classes.of(Style(bold = true)), Vector("okay-bold"))
    assertEquals(Classes.of(Style(dim = true)), Vector("okay-dim"))
    assertEquals(Classes.of(Style(tone = Tone.Danger)), Vector("okay-tone-danger"))
    assertEquals(Classes.of(Style(size = Size.Large)), Vector("okay-size-large"))
    assertEquals(Classes.of(Style(kind = Kind.Ident)), Vector("okay-kind-ident"))
    assertEquals(Classes.of(Style(align = Align.End)), Vector("okay-align-end"))
  }

  test("a default value says nothing, which is what keeps a class list short") {
    assertEquals(Classes.of(Style(tone = Tone.Plain, size = Size.Normal,
      kind = Kind.Prose, align = Align.Start)), Vector.empty)
  }

  test("every token at once, in the order both roads write them") {
    assertEquals(
      Classes.of(Style(bold = true, dim = true, tone = Tone.Danger,
        size = Size.Large, kind = Kind.Number, align = Align.End)),
      Vector("okay-bold", "okay-dim", "okay-tone-danger", "okay-size-large",
        "okay-kind-number", "okay-align-end"))
  }

  test("React reads the table rather than spelling it again") {
    val e = React.elem(Ui.Text("x", Style(bold = true, kind = Kind.Ident)))
    assertEquals(e.props, Vector("className" -> "okay-bold okay-kind-ident"))
  }

  // ---- the generated half -------------------------------------------

  test("live.js pushes a class for every row of the table, and no other") {
    val js = LiveJs.source
    Classes.flags.foreach { (name, _) =>
      assert(js.contains(s"""if (st.$name)"""), s"live.js does not read st.$name")
      assert(js.contains(s"""cls.push("${Classes.Prefix}$name")"""),
        s"live.js does not push ${Classes.Prefix}$name")
    }
    Classes.choices.foreach { c =>
      assert(js.contains(s"""st.${c.field} !== "${c.none}""""),
        s"live.js does not know ${c.field}'s default is ${c.none}")
      assert(js.contains(s"""cls.push("${Classes.Prefix}${c.field}-" + st.${c.field})"""),
        s"live.js does not build a ${c.field} class")
    }
  }

  test("a token ADDED to the table appears in the client without anybody editing it") {
    // the property, stated as the count: six rows, six pushes, and a
    // seventh row would be a seventh push on its own
    // a plain count, not a regex: `(` opens a group
    val pushes = LiveJs.source.sliding("cls.push(".length).count(_ == "cls.push(")
    assertEquals(pushes, Classes.flags.size + Classes.choices.size)
  }

  test("the emitted JavaScript is ESCAPED, because the printer wrote it") {
    // every class name reaches the client as a quoted literal rather
    // than as text concatenated into a script
    assert(LiveJs.source.contains("""cls.push("okay-bold")"""), LiveJs.source.take(0))
  }

  test("the CSS in Html dresses every class the table can produce") {
    val css = Html.css
    Classes.flags.foreach((name, _) =>
      assert(css.contains(s".${Classes.Prefix}$name"), s"no rule for ${Classes.Prefix}$name"))
    // the enums are per value, so the sheet is checked for the ones
    // this vocabulary actually has
    Vector("okay-tone-danger", "okay-size-large", "okay-kind-ident", "okay-align-end")
      .foreach(c => assert(css.contains(s".$c"), s"no rule for $c"))
  }
