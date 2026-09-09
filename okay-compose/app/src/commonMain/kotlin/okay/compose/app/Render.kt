// Level L, drawn with Compose. The tree is the state; every widget
// reads its value from the tree and reports its event to `act`,
// which applies the hybrid rule and re-renders — the same loop the
// browser's live.js and okay's own Wire.client run.
package okay.compose.app

import androidx.compose.foundation.layout.*
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.input.PasswordVisualTransformation
import androidx.compose.ui.text.input.VisualTransformation
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import okay.compose.protocol.*

@Composable
fun Render(u: Ui, act: (Event) -> Unit, modifier: Modifier = Modifier) {
    when (u) {
        is Ui.Text -> Text(
            u.s,
            modifier = modifier,
            fontWeight = if (u.style.bold || u.style.tone == Tone.Emphasis) FontWeight.Bold else FontWeight.Normal,
            color = when (u.style.tone) {
                Tone.Danger -> MaterialTheme.colors.error
                Tone.Muted -> Color.Gray
                else -> if (u.style.dim) Color.Gray else Color.Unspecified
            },
            fontSize = when (u.style.size) { Size.Small -> 12.sp; Size.Normal -> 14.sp; Size.Large -> 20.sp },
        )
        is Ui.Row -> Row(modifier, horizontalArrangement = Arrangement.spacedBy(4.dp)) { u.children.forEach { Render(it, act) } }
        is Ui.Column -> Column(modifier) { u.children.forEach { Render(it, act) } }
        is Ui.Box -> {
            // weights divide the main axis; gap and pad are character units, ~8dp each
            val weighted = u.weights.size == u.children.size && u.weights.all { it > 0 }
            val pad = Modifier.padding((u.pad * 8).dp)
            if (u.dir == Dir.Horizontal) Row(modifier.then(pad), horizontalArrangement = Arrangement.spacedBy((u.gap * 8).dp)) {
                u.children.forEachIndexed { i, c -> Render(c, act, if (weighted) Modifier.weight(u.weights[i].toFloat()) else Modifier) }
            } else Column(modifier.then(pad), verticalArrangement = Arrangement.spacedBy((u.gap * 8).dp)) {
                u.children.forEachIndexed { i, c -> Render(c, act, if (weighted) Modifier.weight(u.weights[i].toFloat()) else Modifier) }
            }
        }
        is Ui.Image -> Text("[image: ${u.alt}]", modifier)   // a src is a URL the app has no loader for yet
        is Ui.Button -> Button(
            onClick = { act(Event.Pressed(u.key)) },
            modifier = modifier,
            colors = when (u.role) {
                Role.Danger -> ButtonDefaults.buttonColors(backgroundColor = MaterialTheme.colors.error)
                Role.Primary, Role.Active -> ButtonDefaults.buttonColors()
                Role.Plain -> ButtonDefaults.outlinedButtonColors()
            },
        ) { Text(u.label) }
        is Ui.Input -> OutlinedTextField(
            value = u.value,
            onValueChange = { act(Event.Edited(u.key, it)) },
            modifier = modifier,
            label = if (u.label.isEmpty()) null else ({ Text(u.label) }),
            singleLine = u.kind != InputKind.Multiline,
            visualTransformation = if (u.kind == InputKind.Secret) PasswordVisualTransformation() else VisualTransformation.None,
        )
        is Ui.Check -> Row(modifier, verticalAlignment = androidx.compose.ui.Alignment.CenterVertically) {
            Checkbox(checked = u.on, onCheckedChange = { act(Event.Toggled(u.key, it)) })
            Text(u.label)
        }
        is Ui.Select -> {
            var open by remember { mutableStateOf(false) }
            Box(modifier) {
                OutlinedButton(onClick = { open = true }) { Text(u.options.getOrNull(u.selected) ?: "") }
                DropdownMenu(expanded = open, onDismissRequest = { open = false }) {
                    u.options.forEachIndexed { i, o ->
                        DropdownMenuItem(onClick = { open = false; act(Event.Chosen(u.key, i)) }) { Text(o) }
                    }
                }
            }
        }
        is Ui.Scroll -> Column(modifier.verticalScroll(rememberScrollState())) { Render(u.child, act) }
        is Ui.Form -> Column(modifier) {
            u.fields.forEach { Render(it, act) }
            Render(Ui.Button(u.submit, u.key, Role.Primary), act)
        }
        // level S never arrives: this client claims nothing, the server lowers
        is Ui.Items -> Column(modifier) { u.items.forEach { Render(it, act) } }
        is Ui.Table -> Column(modifier) {
            Row { u.header.forEach { Text(it, Modifier.weight(1f), fontWeight = FontWeight.Bold) } }
            u.rows.forEach { r -> Row { r.forEach { Render(it, act, Modifier.weight(1f)) } } }
        }
        is Ui.Tabs -> Column(modifier) {
            Row { u.labels.forEachIndexed { i, l -> Render(Ui.Button(l, "${u.key}\$tab$i", if (i == u.selected) Role.Active else Role.Plain), act) } }
            u.pages.getOrNull(u.selected)?.let { Render(it, act) }
        }
        is Ui.Modal -> Column(modifier.padding(8.dp)) { Text(u.title, fontWeight = FontWeight.Bold); Render(u.body, act) }
        is Ui.Disclosure -> Column(modifier) {
            Render(Ui.Button(u.title, u.key, if (u.open) Role.Active else Role.Plain), act)
            if (u.open) Render(u.body, act)
        }
    }
}
