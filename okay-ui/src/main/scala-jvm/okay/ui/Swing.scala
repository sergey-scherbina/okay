package okay.ui

import okay.*
import okay.given
import java.awt.{Color, Component, Container, Font}
import javax.swing.*
import javax.swing.event.{DocumentEvent, DocumentListener}

/**
 * The JVM's own toolkit as a Backend (ui-native-toolkits): the "out
 * of the box" native leg of specs/frontend.md — a desktop window with
 * no browser and no wire, zero dependencies, over the SAME seam the
 * DOM backend uses. Built exactly as `Dom` is: the tree is the plan,
 * `Ui.patch` keeps a MIRROR (events interpret against a value, not a
 * widget), `React.event` is the one pure interpretation, and a patch
 * path walks `getComponents` index for index — every Ui child builds
 * exactly ONE root component (an Input's label wrapper is a leaf's
 * root; a Scroll's child is its viewport's view).
 *
 * Headless-testable: Swing components exist without a display; only
 * `window` needs one.
 */
object Swing {

  /** a Backend over a container that will hold the tree's one root
   * component; `Ui.diffing(Swing.backend(panel))` is the Host */
  def backend(root: Container): Backend = new Backend:
    private val feed = Channel[Event]()
    private var mirror: Ui = Ui.Text("")
    // a patch that sets a value fires the same listeners a user does;
    // while applying, nothing is a user
    private var applying = false

    def events: Source[Event] = Writer.of(feed)

    def apply(p: Patch): Unit ! Async = async {
      applying = true
      try applyNow(p) finally applying = false
      mirror = Ui.patch(mirror, p)
      root.revalidate(); root.repaint()
    }

    private def kids(c: Component): Array[Component] = c match
      case sp: JScrollPane => Array(sp.getViewport.getView)
      case ct: Container => ct.getComponents
      case _ => Array.empty
    private def at(path: List[Int]): Component =
      path.foldLeft(root.getComponent(0))((c, i) => kids(c)(i))
    private def replaceIn(parent: Component, i: Int, fresh: Component): Unit = parent match
      case sp: JScrollPane => sp.setViewportView(fresh)
      case ct: Container =>
        ct.remove(i); val _ = ct.add(fresh, i)

    private def applyNow(p: Patch): Unit = p match
      case Patch.Replace(Nil, ui) =>
        root.removeAll(); val _ = root.add(build(ui))
      case Patch.Replace(path, ui) => replaceIn(at(path.init), path.last, build(ui))
      case Patch.SetText(path, s) => at(path) match
        case l: JLabel => l.setText(s)
        case _ => ()
      case Patch.SetValue(path, v) => text(at(path)).foreach(_.setText(v))
      case Patch.SetChecked(path, on) => at(path) match
        case c: JCheckBox => c.setSelected(on)
        case _ => ()
      case Patch.SetSelected(path, i) => at(path) match
        case c: JComboBox[?] => c.setSelectedIndex(i)
        case _ => ()
      case Patch.Remove(path, i) => at(path) match
        case ct: Container => ct.remove(i)
        case _ => ()
      case Patch.Reorder(path, order) => at(path) match
        case ct: Container =>
          val snapshot = ct.getComponents
          ct.removeAll()
          order.foreach(i => ct.add(snapshot(i)))
        case _ => ()
      case Patch.Insert(path, i, ui) => at(path) match
        case ct: Container => val _ = ct.add(build(ui), i)
        case _ => ()

    /** a leaf's editable text component: the node itself, or the one
     * inside its label wrapper */
    private def text(c: Component): Option[javax.swing.text.JTextComponent] = c match
      case t: javax.swing.text.JTextComponent => Some(t)
      case ct: Container => ct.getComponents.collectFirst { case t: javax.swing.text.JTextComponent => t }
      case _ => None

    private def emit(key: String, kind: String, value: String): Unit =
      if !applying then React.event(mirror, key, kind, value).foreach(feed.offer)

    /** the tree, built — the only builder; level S arrives lowered
     * (`Ui.diffing` lowers), Form is level L */
    private def build(ui: Ui): Component = ui match
      case Ui.Text(s, style) =>
        val l = new JLabel(if s.contains('\n') then "<html>" + s.replace("\n", "<br>") + "</html>" else s)
        val base = l.getFont
        val bold = style.bold || style.tone == Tone.Emphasis
        val size = style.size match
          case Size.Small => base.getSize2D * 0.85f
          case Size.Normal => base.getSize2D
          case Size.Large => base.getSize2D * 1.4f
        l.setFont(base.deriveFont(if bold then Font.BOLD else Font.PLAIN, size))
        if style.dim || style.tone == Tone.Muted then l.setForeground(Color.GRAY)
        if style.tone == Tone.Danger then l.setForeground(Color.RED)
        l
      case Ui.Row(children, _) => panel(BoxLayout.X_AXIS, children.map(build), 0)
      case Ui.Column(children, _) => panel(BoxLayout.Y_AXIS, children.map(build), 0)
      case Ui.Box(children, dir, _, _, pad, _) =>
        // weights are BoxLayout's natural sizes here (a GridBag would
        // need a second index space); pad is a border
        panel(if dir == Dir.Horizontal then BoxLayout.X_AXIS else BoxLayout.Y_AXIS, children.map(build), pad * 8)
      case Ui.Form(fields, submit, key) =>
        panel(BoxLayout.Y_AXIS, fields.map(build) :+ build(Ui.Button(submit, key, Role.Primary)), 0)
      case Ui.Scroll(child, _) => new JScrollPane(build(child))
      case Ui.Image(_, alt) => new JLabel(s"[image: $alt]")
      case Ui.Button(label, key, role) =>
        val b = new JButton(label)
        if role == Role.Danger then b.setForeground(Color.RED)
        if role == Role.Active then b.setFont(b.getFont.deriveFont(Font.BOLD))
        b.addActionListener(_ => emit(key, "click", ""))
        b
      case Ui.Input(value, key, label, kind, _) =>
        val field: javax.swing.text.JTextComponent = kind match
          case InputKind.Secret => new JPasswordField(value, 12)
          case InputKind.Multiline => new JTextArea(value, 3, 20)
          case _ => new JTextField(value, 12)
        field.getDocument.addDocumentListener(new DocumentListener:
          def insertUpdate(e: DocumentEvent): Unit = emit(key, "input", field.getText)
          def removeUpdate(e: DocumentEvent): Unit = emit(key, "input", field.getText)
          def changedUpdate(e: DocumentEvent): Unit = emit(key, "input", field.getText))
        if label.isEmpty then field
        else panel(BoxLayout.X_AXIS, Vector(new JLabel(label + ": "), field), 0)
      case Ui.Check(on, key, label) =>
        val c = new JCheckBox(label, on)
        c.addItemListener(_ => emit(key, "change", ""))
        c
      case Ui.Select(options, selected, key) =>
        val c = new JComboBox[String](options.toArray)
        if options.nonEmpty then c.setSelectedIndex(math.min(math.max(selected, 0), options.length - 1))
        c.addActionListener(_ => emit(key, "change", Option(c.getSelectedItem).map(_.toString).getOrElse("")))
        c
      case semantic => build(Ui.lower(semantic, Set.empty))

    private def panel(axis: Int, children: Vector[Component], pad: Int): JPanel =
      val p = new JPanel()
      p.setLayout(new BoxLayout(p, axis))
      if pad > 0 then p.setBorder(BorderFactory.createEmptyBorder(pad, pad, pad, pad))
      children.foreach(p.add)
      p

  /** the Host: the core diff over the Swing backend */
  def host(root: Container): Host = Ui.diffing(backend(root))

  /** a window running an application — the one thing here that needs
   * a display: `Swing.window("okay")(Ui.run(0)(view)(update)(_))` */
  def window[A](title: String)(app: Host => A ! Async): A ! Async =
    val frame = new JFrame(title)
    val panel = new JPanel(new java.awt.BorderLayout())
    frame.add(panel)
    frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
    frame.setSize(480, 360)
    frame.setVisible(true)
    app(host(panel)).map { a => frame.dispose(); a }
}
