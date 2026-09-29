## resume-late-withdraw — the foreign-thread handoff withdrawn: it broke merge's per-side order

`DriveTask.resumeLate` (f50932cbe, ebdfd0ec7) sent a fiber answered from
outside the pool home instead of running it on the answering thread. It
made the cap-64 elementwise merge 0.92x Loom, and it broke
`TestMergeOrder` (a sibling's gate and bisect): a partitioned channel
routes a send by its THREAD, and a producer moved to another worker on
every resume wrote its next run into another part. Withdrawn: the hook,
`resumeHere` and `DriveTask`'s `home` are gone and a late answer resumes
in place as before; TestMergeOrder green at OKAY_MERGE_ROUNDS=400, drive
and scheduler laws green. The cap-64 merge is back to 1.12x Loom; the
way back is backlog `channel-route-per-producer`. recscan's Freer$Bind
pattern taken byte for byte from a sibling's unlanded
source-zip-lost-pairs, so either lands first.
