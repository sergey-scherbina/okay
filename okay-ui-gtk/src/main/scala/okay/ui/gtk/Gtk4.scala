package okay.ui.gtk

import scala.scalanative.unsafe.*

/**
 * The handful of GTK 4 calls a level-L renderer needs — nothing more.
 * Every widget is a `Ptr[Byte]` (a GtkWidget*), every gboolean a
 * CInt, every string a CString; the linking flags come from
 * `pkg-config --libs gtk4` in build.sbt.
 */
@extern
object Gtk4 {
  type Widget = Ptr[Byte]
  type Callback = CFuncPtr2[Ptr[Byte], Ptr[Byte], Unit]
  type NotifyCallback = CFuncPtr3[Ptr[Byte], Ptr[Byte], Ptr[Byte], Unit]
  type IdleCallback = CFuncPtr1[Ptr[Byte], CInt]

  // ---- init and the main loop
  def gtk_init(): Unit = extern
  def gtk_init_check(): CInt = extern
  def g_main_context_iteration(context: Ptr[Byte], mayBlock: CInt): CInt = extern
  def g_idle_add(fn: IdleCallback, data: Ptr[Byte]): CUnsignedInt = extern

  // ---- signals and types
  def g_signal_connect_data(instance: Ptr[Byte], signal: CString, handler: CVoidPtr,
                            data: Ptr[Byte], destroy: Ptr[Byte], flags: CInt): CUnsignedLong = extern
  def g_type_name_from_instance(instance: Ptr[Byte]): CString = extern
  /** C varargs: a signal with no arguments beyond the instance */
  def g_signal_emit_by_name(instance: Ptr[Byte], signal: CString, rest: Any*): Unit = extern

  // ---- the widget tree
  def gtk_widget_get_first_child(w: Widget): Widget = extern
  def gtk_widget_get_next_sibling(w: Widget): Widget = extern
  def gtk_widget_set_hexpand(w: Widget, on: CInt): Unit = extern
  def gtk_widget_set_vexpand(w: Widget, on: CInt): Unit = extern
  def gtk_widget_set_margin_start(w: Widget, m: CInt): Unit = extern
  def gtk_widget_set_margin_end(w: Widget, m: CInt): Unit = extern
  def gtk_widget_set_margin_top(w: Widget, m: CInt): Unit = extern
  def gtk_widget_set_margin_bottom(w: Widget, m: CInt): Unit = extern
  def gtk_widget_add_css_class(w: Widget, cls: CString): Unit = extern
  def gtk_widget_activate(w: Widget): CInt = extern

  // ---- box
  def gtk_box_new(orientation: CInt, spacing: CInt): Widget = extern
  def gtk_box_append(box: Widget, child: Widget): Unit = extern
  def gtk_box_remove(box: Widget, child: Widget): Unit = extern
  def gtk_box_insert_child_after(box: Widget, child: Widget, sibling: Widget): Unit = extern
  def gtk_box_reorder_child_after(box: Widget, child: Widget, sibling: Widget): Unit = extern

  // ---- leaves
  def gtk_label_new(s: CString): Widget = extern
  def gtk_label_set_text(l: Widget, s: CString): Unit = extern
  def gtk_label_get_text(l: Widget): CString = extern
  def gtk_button_new_with_label(s: CString): Widget = extern
  def gtk_button_get_label(b: Widget): CString = extern
  def gtk_entry_new(): Widget = extern
  def gtk_entry_set_visibility(e: Widget, visible: CInt): Unit = extern
  def gtk_entry_set_placeholder_text(e: Widget, s: CString): Unit = extern
  def gtk_editable_set_text(e: Widget, s: CString): Unit = extern
  def gtk_editable_get_text(e: Widget): CString = extern
  def gtk_check_button_new_with_label(s: CString): Widget = extern
  def gtk_check_button_set_active(c: Widget, on: CInt): Unit = extern
  def gtk_check_button_get_active(c: Widget): CInt = extern
  def gtk_check_button_get_label(c: Widget): CString = extern
  def gtk_drop_down_new_from_strings(strings: Ptr[CString]): Widget = extern
  def gtk_drop_down_set_selected(d: Widget, i: CUnsignedInt): Unit = extern
  def gtk_drop_down_get_selected(d: Widget): CUnsignedInt = extern
  def gtk_scrolled_window_new(): Widget = extern
  def gtk_scrolled_window_set_child(sw: Widget, child: Widget): Unit = extern
  def gtk_scrolled_window_get_child(sw: Widget): Widget = extern
  def gtk_viewport_get_child(vp: Widget): Widget = extern

  // ---- a window
  def gtk_window_new(): Widget = extern
  def gtk_window_set_title(w: Widget, s: CString): Unit = extern
  def gtk_window_set_child(w: Widget, child: Widget): Unit = extern
  def gtk_window_set_default_size(w: Widget, width: CInt, height: CInt): Unit = extern
  def gtk_window_present(w: Widget): Unit = extern
  def gtk_window_destroy(w: Widget): Unit = extern
}

object GtkConst {
  val HORIZONTAL = 0
  val VERTICAL = 1
}
