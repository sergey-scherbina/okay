package okay.ts

import scala.scalajs.js
import okay.{!, Async, effect, pure}
import okay.codec.Json

/**
 * Where a durable flow's answers are kept (typescript-types T10): one
 * entry per step, in order, per flow. An entry is `{ name, args, answer }`
 * as JSON — what the program asked and what it was told — so a replay can
 * check that it is asking the SAME thing before it is handed the answer.
 *
 * `append` answers only once the entry is STORED: `Ts.durable` continues the
 * program after that, so a step the program has acted on is a step that
 * survives a reload.
 */
trait Journal:
  /** the flow's entries so far, in step order */
  def load(flow: String): Vector[Json] ! Async
  /** entry `step` of `flow`, stored before this answers */
  def append(flow: String, step: Int, entry: Json): Unit ! Async
  /** forget the flow */
  def clear(flow: String): Unit ! Async

object Journal:

  /** in this page only: a test's journal, or a flow that need not survive */
  def memory(): Journal = new Journal:
    private val flows = scala.collection.mutable.Map.empty[String, Vector[Json]]
    def load(flow: String): Vector[Json] ! Async = okay.async(flows.getOrElse(flow, Vector.empty))
    def append(flow: String, step: Int, entry: Json): Unit ! Async =
      okay.async(flows.update(flow, flows.getOrElse(flow, Vector.empty).take(step) :+ entry))
    def clear(flow: String): Unit ! Async = okay.async(flows.remove(flow): Unit)

  /**
   * The browser's IndexedDB, database `name`, one object store `steps`
   * keyed by `[flow, step]` — an array key, so one flow's steps are one
   * key range, read back in step order. A write answers on the
   * transaction's `complete`, which is when IndexedDB says it is durable.
   * Reached through `js.Dynamic` because the DOM's IndexedDB types are not
   * in this build; every call is one the IndexedDB standard names.
   */
  def indexedDb(name: String): Journal = new Journal:
    private var opened: Option[js.Dynamic] = None

    private def request[A](make: => js.Dynamic)(done: js.Dynamic => A): A ! Async =
      effect[Async, A](Async.Await[A] { k =>
        val r = make
        r.onsuccess = (_: js.Any) => k(Right(done(r)))
        r.onerror = (_: js.Any) => k(Left(js.JavaScriptException(r.error)))
        () => ()
      })

    private def db: js.Dynamic ! Async = opened match
      case Some(d) => pure(d)
      case None =>
        effect[Async, js.Dynamic](Async.Await[js.Dynamic] { k =>
          val r = js.Dynamic.global.indexedDB.open(name, 1)
          r.onupgradeneeded = (_: js.Any) => { val _ = r.result.createObjectStore("steps") }
          r.onsuccess = (_: js.Any) => { opened = Some(r.result); k(Right(r.result)) }
          r.onerror = (_: js.Any) => k(Left(js.JavaScriptException(r.error)))
          () => ()
        })

    private def range(flow: String): js.Dynamic =
      js.Dynamic.global.IDBKeyRange.bound(js.Array[js.Any](flow, 0), js.Array[js.Any](flow, Double.PositiveInfinity))

    private def completed(tx: js.Dynamic): Unit ! Async =
      effect[Async, Unit](Async.Await[Unit] { k =>
        tx.oncomplete = (_: js.Any) => k(Right(()))
        tx.onerror = (_: js.Any) => k(Left(js.JavaScriptException(tx.error)))
        tx.onabort = (_: js.Any) => k(Left(js.JavaScriptException(tx.error)))
        () => ()
      })

    def load(flow: String): Vector[Json] ! Async =
      db.flatMap(d => request(d.transaction("steps", "readonly").objectStore("steps").getAll(range(flow))) { r =>
        // each value is an entry's text; the array, read as JSON, is
        // a JSON array of those texts — typed, with no count to cast
        Json.parse(String.valueOf(js.JSON.stringify(r.result))) match
          case Json.JArr(texts) => texts.collect { case Json.JStr(t) => Json.parse(t) }
          case _ => Vector.empty
      })

    def append(flow: String, step: Int, entry: Json): Unit ! Async =
      db.flatMap { d =>
        val tx = d.transaction("steps", "readwrite")
        val _ = tx.objectStore("steps").put(Json.print(entry), js.Array[js.Any](flow, step))
        completed(tx)
      }

    def clear(flow: String): Unit ! Async =
      db.flatMap { d =>
        val tx = d.transaction("steps", "readwrite")
        val _ = tx.objectStore("steps").delete(range(flow))
        completed(tx)
      }
