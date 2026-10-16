## core-pin-small-runs — okay-core pinned at 21a4447: small runs 3.4× faster

okay-std's `HandlerBenchmark.stateSmall` (100 small `State.run`s) took 5.41 µs on
the core road against classic's 1.44, which is 3.76×. Three kernel changes, in
okay-core, bring it to 1.61 µs, 1.11×. First, a handler's `ret` that is a value
goes on at once at the answer outside, with no `Lift` and its level (eb11938).
Second, a run whose top is ONE `Stateful` handler goes through a loop of its own
in `Machine.run`: the state is a parameter, and the machine is entered as the run
stands at the first node the loop does not take (825980a, 7735003). Third, the
facade's head path `given here`/`Paths.one` is one shared `In.Here` and `One`;
before, it was two new objects per operation (21a4447). okay-core's own lanes
and `MixedBenchmark` stay within noise. okay-core's tests pass on JVM, JS and
Native (256, 253, 256, words 45). This lane compiles the whole build with no
warnings, and okay, okay-std, okay-async, okay-stream, okay-tx and okay-durable
pass (895 tests). `affected` sees no change for a submodule pin, so these
suites were named by hand.
