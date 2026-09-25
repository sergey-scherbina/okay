## foreign-facade-4b - Holds[M] and Methods[M]: object handles behind the facade, where a handle lives deciding the runtime

Stage 4b of specs/foreign-facade.md. `Holds[-M]` is hold / a function
with the handle as its first argument / release, for Python and R;
`Methods[-M]` (a held object's method and attribute) is Python's only —
R's objects have no methods, so R has no instance. The handle's type is
the instance's `Ref`, exposed through the refined aliases `Holds.Py`,
`Holds.R`, `Methods.Py` (a `given` cannot carry a refinement), so a
handle from `Holds` is what `Methods` takes. Python's instances run on
`PyWorkers`, the pool that routes a call naming a handle to the worker
holding it; R keeps a module's handles on one worker of its own. The
bodies `holds` (two objects, each with its own state, released) and
`methods` are green over python3 here; R's `holds` skips (not
installed). Stage 4 is complete: Calls, Frames, Streams, Programs,
Holds, Methods, Speaks.
