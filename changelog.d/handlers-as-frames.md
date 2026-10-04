## handlers-as-frames — PROBE, refuted: handlers only as frames lose to the folds

Operator, 2026-10-04 ("Начинай"): could every handler run only as its frame on
`Delimited`, deleting the second implementation (the fold) of each? Probed with
tail-resumptive State/Writer answered in place by the machine (no capture), all
handlers forced onto their frames: stateSmall 4.34x, stateHandle 1.79x,
writerTell 2.03x, mixedList 1.48x master's folds (history.d handlers-as-frames).
The code was not landed; the folds stay. specs/cont-atm.md Results says what
would reopen it.
