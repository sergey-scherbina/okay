## kdf-to-password-hash - the Kdf effect is PasswordHash

okay-rust's effect for Argon2id was named `Kdf` (key derivation
function), which is opaque to anyone who does not know the term. It is
now `PasswordHash`, named for what a program asks rather than how it is
computed. The handlers are otherwise the same: `PasswordHash.rust` (FFM),
`.wasm` (Chicory), `.native` (Scala Native) and `.using(f)`. The
platform trait, the golden suite and the tests are renamed to match, and
so are the docs (docs/rust.md, docs/modules/okay-rust.md) and the spec.
Earlier changelog entries keep the old name as the record of what landed
then.

Also fixed here: since rust-native made okay-rust a cross project, its
forked JVM tests start in `okay-rust/.jvm`. Every Live test then looked
for its crate in a directory that did not exist ("Cannot run program
cargo (in directory okay-rust/kernels/argon2)"). rust-native's gate ran
the default suites and the Native check, not the JVM Live ones, which is
why it passed. `Kernels.dir(name)` now finds `kernels/` from the platform
directory, the module, or the repository, and all 11 JVM Live tests are
green again.
