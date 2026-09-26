/**
 * `okay.py` is the name this package had until foreign-package-name
 * (2026-09-26): it served Python first, and then every language through the
 * one engine, and is `okay.foreign` now. Kept for a release, so code written
 * against `okay.py` compiles unchanged — every name here is the one in
 * `okay.foreign`, the same type and the same object. New code imports
 * `okay.foreign`.
 */
package okay.py

type Address = okay.foreign.Address
val Address = okay.foreign.Address
val ArrowFrames = okay.foreign.ArrowFrames
type Condition = okay.foreign.Condition
val Condition = okay.foreign.Condition
val Foreign = okay.foreign.Foreign
type ForeignEval[+A] = okay.foreign.ForeignEval[A]
val ForeignEval = okay.foreign.ForeignEval
val ForeignGateway = okay.foreign.ForeignGateway
type ForeignWorker = okay.foreign.ForeignWorker
val ForeignWorker = okay.foreign.ForeignWorker
type FrameTables = okay.foreign.FrameTables
val Go = okay.foreign.Go
val GoWorker = okay.foreign.GoWorker
val HaskellWorker = okay.foreign.HaskellWorker
type Holding[+A] = okay.foreign.Holding[A]
val Holding = okay.foreign.Holding
val Hs = okay.foreign.Hs
val Jvm = okay.foreign.Jvm
type Pool[E] = okay.foreign.Pool[E]
val Pools = okay.foreign.Pools
val Py = okay.foreign.Py
val PyCodec = okay.foreign.PyCodec
type PyEnv = okay.foreign.PyEnv
val PyEnv = okay.foreign.PyEnv
type PyEval[+A] = okay.foreign.PyEval[A]
val PyFacade = okay.foreign.PyFacade
type PyFrame = okay.foreign.PyFrame
val PyFrame = okay.foreign.PyFrame
type PyModule = okay.foreign.PyModule
val PyModule = okay.foreign.PyModule
type PyNode = okay.foreign.PyNode
val PyNode = okay.foreign.PyNode
type PyParam = okay.foreign.PyParam
val PyParam = okay.foreign.PyParam
type PyRef = okay.foreign.PyRef
val PyRef = okay.foreign.PyRef
type PySig = okay.foreign.PySig
val PySig = okay.foreign.PySig
val PyStream = okay.foreign.PyStream
type PySubprocess = okay.foreign.PySubprocess
val PySubprocess = okay.foreign.PySubprocess
type PyValue = okay.foreign.PyValue
val PyValue = okay.foreign.PyValue
type PyWorkers = okay.foreign.PyWorkers
val PyWorkers = okay.foreign.PyWorkers
val Rs = okay.foreign.Rs
val RustWorker = okay.foreign.RustWorker
type Shape = okay.foreign.Shape
val Shape = okay.foreign.Shape
type SupervisedWorker = okay.foreign.SupervisedWorker
type ToPy[A] = okay.foreign.ToPy[A]
val ToPy = okay.foreign.ToPy
val Ts = okay.foreign.Ts
val TsFacade = okay.foreign.TsFacade
val TsWorker = okay.foreign.TsWorker
type WireLink = okay.foreign.WireLink
val WireLink = okay.foreign.WireLink
type WireSession = okay.foreign.WireSession
val WireSession = okay.foreign.WireSession
type WorkerCommand = okay.foreign.WorkerCommand
val WorkerCommand = okay.foreign.WorkerCommand
