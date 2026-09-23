(ns okay.clojure.programs
  "Clojure programs the okay tests run (TestProgram), in okay.core"
  (:require [okay.core :as ok])
  (:import [okay.clojure Ops]))

(defn running-sum
  "a running sum, iteratee style: await until nil, tell each sum"
  [acc]
  (ok/mlet [m (ok/await)]
    (if (nil? m)
      (ok/done nil)
      (let [s (+ acc m)]
        (ok/mlet [_ (ok/tell s)]
          (running-sum s))))))

(defn above-threshold
  "a stage that also performs: keeps what beats the threshold it ASKS"
  []
  (ok/mlet [m (ok/await)]
    (if (nil? m)
      (ok/done nil)
      (ok/mlet [t (ok/perform (Ops/ask))
                _ (if (> m t) (ok/tell m) (ok/done nil))]
        (above-threshold)))))

(def reader-state
  "Reader and State, in mlet order: env * 1000 + the state after +1"
  (ok/mlet [env (ok/perform (Ops/ask))
            s   (ok/perform (Ops/get))
            _   (ok/perform (Ops/set (inc s)))
            s2  (ok/perform (Ops/get))]
    (ok/done (+ (* env 1000) s2))))

(def raising (ok/perform (Ops/raise "boom from clojure")))

(def choosing
  "MULTI-SHOT: Choose resumes the continuation once per branch"
  (ok/mlet [a (ok/perform (Ops/choose [1 2]))
            b (ok/perform (Ops/choose [10 20]))]
    (ok/done (+ a b))))

(def sleep-twice
  (ok/mlet [a (ok/perform (Ops/sleep 20))
            b (ok/perform (Ops/sleep 20))]
    (ok/done (+ a b))))

(defn count-to
  "a long loop, for the stack"
  [i n]
  (if (>= i n)
    (ok/done nil)
    (ok/mlet [_ (ok/tell i)] (count-to (inc i) n))))

;; interop-lift-cancellation: blocking Java, lifted, observable — after its
;; sleep it leaves a mark the test reads, so "the work stopped" is told
;; apart from "the fiber was reported finished while its thread slept on"
(defn mark-after [key ms]
  (ok/lift (fn [] (Thread/sleep (long ms)) (System/setProperty key "woke") "woke")))
