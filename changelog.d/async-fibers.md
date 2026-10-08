## Async on the machine: fibers

Lane async-fibers. `AsyncCont.spawn`/`fork`/`join`, `par` (the first failure
cancels the sibling), `race` (the first success wins, the loser cancelled),
`timeout` (the program's own failure comes through at once) and `sleep`, on the
platform's own `Scheduler`: a fiber runs one classic Await whose registration
starts the machine's cancellable drive. With async-cancel this is what okay-stream
needs of Async on the machine.
