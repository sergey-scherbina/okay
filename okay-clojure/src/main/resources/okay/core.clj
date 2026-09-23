(ns okay.core
  "okay's effects from Clojure (specs/clojure.md, stage 2).

  A program is DATA: `(done v)`, or `(step op k)` — one operation for okay
  and `k`, a function from its answer to the rest of the program. okay's
  driver (`okay.clojure.Program`, Scala) walks it: each operation runs
  under the okay program's handlers, and `k` is called with the answer —
  more than once, if a handler resumes more than once (Choose). Nothing is
  hidden in a lazy seq and no thread is involved.

    (require '[okay.core :as ok])

    (defn above [t]                         ; a stage: keep what beats t
      (ok/mlet [m (ok/await)]
        (if (nil? m) (ok/done nil)
            (ok/mlet [_ (if (> m t) (ok/tell m) (ok/done nil))]
              (above t)))))

  `mlet` is a monadic let in cats' shape: each binding's expression is a
  program, its name the answer, and the body is a program again."
  (:refer-clojure :exclude [await]))

;; the two shapes of a program, and the stage's own two operations
(defrecord Done [value])
(defrecord Step [op k])
(defrecord Tell [value])
(defrecord Lift [f])

(defn done
  "a program that has answered `v`"
  [v] (->Done v))

(defn step
  "a program that asks okay `op`, then continues with `(k answer)`"
  [op k] (->Step op k))

(defn bind
  "the program `p`, then `(f its-answer)`"
  [p f]
  (if (instance? Done p)
    (f (:value p))
    (let [k (:k p)]
      (->Step (:op p) (fn [x] (bind (k x) f))))))

(defn perform
  "perform an okay operation (an okay.clojure.Ops value, or your own) in
  the okay program's row; answers what the operation answers"
  [op] (->Step op done))

(defn await
  "the next input of a stage, or nil at its end"
  [] (->Step ::await done))

(defn tell
  "emit one output of a stage"
  [x] (->Step (->Tell x) done))

(defn lift
  "run `(f)` - blocking Clojure or Java code - as ONE step, answering what
  it returns. Where the okay program's row carries Async, cancelling the
  fiber INTERRUPTS it, whatever the scheduler (interop-lift-cancellation)."
  [f] (->Step (->Lift f) done))

(defmacro mlet
  "a monadic let over programs: `(mlet [x p, y q] body)` is
  `(bind p (fn [x] (bind q (fn [y] body))))` — body is a program"
  [bindings & body]
  (if (empty? bindings)
    `(do ~@body)
    (let [[sym expr & more] bindings]
      `(bind ~expr (fn [~sym] (mlet [~@more] ~@body))))))
