## scalus-events-mode: rollbacks as rows in `format("cardano")`

`mode = events` (specs/scalus.md §6, stage 3): every block as it arrives
(`confirmations` still chooses how deep), and a rollback that reaches a
shown block as a ROW — `struct<seq, event: applied | rolled_back,
rollbackTo: struct<blockNo, hash>, row: <the table's row>>`. The
consumer deletes rows above `rollbackTo.blockNo` and reads on.

The driver appends each event to an okay-persist `FileStore` topic
(`Ack.Durable`) BEFORE Spark sees it, and an offset is the journal
sequence — a re-run batch reads the same records even when the chain
has since orphaned a block no relay would serve again. A restarted
query resumes after the newest block still standing in the journal.

Tested against a FAKE RELAY (okay-scalus tests): the recorded preprod
headers and blocks served by a scripted chain that answers the client's
requests and can roll back — "five blocks, back to 2, then 3 and 4
again" streams as `+0 +1 +2 +3 +4 <2 +3 +4`, seq 0..7; a range planned
twice reads identical records; the journal replayed resumes at block 2.
The same fake relay pins the rollback through `CardanoFollower` alone.

Docs: okay-scalus-spark page (the mode, `mode`/`journal` options, an
analysed snippet), the guide's §5, spec results.
