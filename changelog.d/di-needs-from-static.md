## di-needs-from-static - a module asks the place for what it opens, and the needs are read off the asking

ROADMAP P13 item 4, taken at the operator's word ("займись okay-di").
module-facts had put the declaration where the thing opens —
`.needs(Need.Volume(dir))` beside `FileStore.open(file)` — and the
entry filed the same day said what was still wrong with it: a
declaration is a CLAIM beside the code, and the two can drift. The
incident that lane found was exactly that shape: a container writing
its board to a path the deployment believed was memory.

**`Provision[+A]`** (okay-deploy) is the deployment's vocabulary as an
effect signature: `Volume(dir)` answers the mounted `Path`,
`Database(…)` answers its URL, `Port(n)` answers the number, and each
case carries its `Need`. **`Needs.provisioned`** takes a `Static`
spine over it, declares `spine.leaves.map(_.need)` as the same
`Declared` fact a hand-written `.needs` writes — at construction,
before anything opens, through a `Conf ?=> Module` chain — and opens
the thing with the place's ANSWER. The path a store opens is now the
path the deployment mounts, because the spine gives the module no
other way to get one. `declared`, `DemoDeploy`, every renderer:
unchanged. The place is a handler, `Provision.local` by default (a
volume where it was asked for, a port as its number, a database from
the `*_URL` setting every target already writes, and an error that
NAMES the setting when it is absent); a test brings its own and reads
the file under the temp dir it answered.

The demo's store is converted and declares nothing by hand;
`TestDemoDeploy` reads the volume off it unchanged. Four tests in
`TestNeeds`: the derived need with nothing opened, the path opened
equal to the place's answer, two leaves (a port and a volume) in one
spine, and the named-setting error.

**Found on the way, the second sighting of a recorded trap.** A
companion extension `map` on `Static` LOST to the lexical `map` for
`Id` that `okay.given` brings, and the lambda's argument was typed as
the spine itself (`value resolve is not a member of Static[…]`).
`lexical-extension-beats-companion` had recorded the cure — a member
beats an extension — so `Static.map` is a member, with the reason on
it.

Not done, and said so: `Select` in a provisioning spine (a choice the
place decides, both arms named as needs) has no consumer; the demo's
":memory:" choice is on the CONFIG, which is a value before the spine
is built, so a Scala `if` is the honest spelling.

Commits: this one.
