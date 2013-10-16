(ns monads.types
  (:require [clojure.string :as str]
            [babbage.monoid :as m]))

(set! *warn-on-reflection* true)

(defprotocol MRun
  (mrun [this m]))

(defprotocol Monad
  (mreturn [this o])
  (bind [this m f]))

(defmacro defprotocol+
  "Define a protocol, and a predicate for testing for satisfaction.
   The predicate will be the name of the protocol, lowercased and with
   a ? appended."
  [nm & args]
  (let [nm? (symbol (str (str/lower-case nm) "?"))]
    `(do (defprotocol ~nm ~@args)
         (defn ~nm? [o#] (satisfies? ~nm o#)))))

(defprotocol+ MonadPlus
  (mplus [this lr])
  (mzero [this]))

;; the remaining protocols don't necessarily need to be here---in
;; principle, these and any others should be definable more or less
;; anywhere.

(defprotocol+ MonadState
  (get-state [this])
  (put-state [this o]))

(defprotocol+ MonadTrans
  (lift [this comp])
  (inner [this]))

(defprotocol+ MonadFail
  (fail [this msg]))

(defprotocol+ MonadError
  (catch-error [this comp handler])
  (throw-error [this error]))

(defprotocol+ MonadReader
  (ask [me])
  (local [me f comp]))

(defprotocol+ MonadWriter
  (tell [me w])
  (listen [me c])
  (pass [me c]))

(extend-protocol MRun
  Object
  (mrun [this _] this)
  nil
  (mrun [this _] nil))

(deftype Return [v]
  Object
  (toString [this]
    (with-out-str (print v)))
  MRun
  (mrun [_ m] (mreturn m v)))

(deftype Mplus [l r]
  Object
  (toString [this]
    (with-out-str (print l r)))
  MRun
  (mrun [_ m] (mplus m [l r])))

(deftype Returned [v]
  Object
  (toString [this]
    (with-out-str (print v)))
  MRun
  (mrun [_ m] (v m)))

(deftype Bind [comp f]
  Object
  (toString [this]
    (with-out-str (print [comp f])))
  MRun
  (mrun [_ m] (bind m (mrun comp m) f)))

(defmacro if-instance [cls obj then else]
  `(if (instance? ~cls ~obj)
     (let [~(with-meta obj {:tag cls}) ~obj]
       ~then)
     ~else))

(defmacro when-instance [cls obj & forms]
  `(when (instance? ~cls ~obj)
    (let [~(with-meta obj {:tag cls}) ~obj]
      ~@forms)))

(defmacro cond-instance [obj & cls-and-forms]
  (when (seq cls-and-forms)
    (if (= 1 (count cls-and-forms))
      (first cls-and-forms)
      `(if-instance  ~(first cls-and-forms) ~obj
         ~(second cls-and-forms)
         (cond-instance ~obj ~@(nnext cls-and-forms))))))

(deftype Pair [fst snd]
  clojure.lang.Seqable
  (seq [_] (list fst snd))
  clojure.lang.Indexed
  (nth [_ i]
    (case i
      0 fst
      1 snd
      (throw (IndexOutOfBoundsException.))))
  (nth [_ i notfound]
    (case i
      0 fst
      1 snd
      notfound))
  clojure.lang.Counted
  (count [_] 2)
  Object
  (equals [this other]
    (when-instance Pair other
                   (and (= (.fst this) (.fst other))
                        (= (.snd this) (.snd other)))))
  (toString [this]
    (with-out-str (print [fst snd]))))

(deftype Triple [f s t]
  clojure.lang.Seqable
  (seq [_] (list f s t))
  clojure.lang.Indexed
  (nth [_ i nf]
    (case i
      0 f
      1 s
      2 t
      nf))
  (nth [_ i]
    (case i 0 f 1 s 2 t (throw (IndexOutOfBoundsException.))))
  clojure.lang.Counted
  (count [_] 3)
  Object
  (equals [this other]
    (when-instance Triple other
                   (and (= f (.f other))
                        (= s (.s other))
                        (= t (.t other)))))
  (toString [this]
    (with-out-str (print [f s t]))))


(defn fst [o]
  (cond-instance o
      Pair (.fst o)
      Triple (.f o)))
(defn snd [o]
  (cond-instance o
      Pair (.snd o)
      Triple (.s o)))

(defn thd [^Triple o]
  (.t o))

(deftype Either [v type]
  Object
  (equals [this other]
    (when-instance Either other
                   (and (= (.v this) (.v other))
                        (= (.type this) (.type other)))))
  (toString [this]
    (with-out-str (print [type v]))))

(defn right? [^Either o]
  (= :right (.type o)))
(defn left? [^Either o]
  (= :left (.type o)))

(defn right [x]
  (Either. x :right))
(defn left [x]
  (Either. x :left))

(defn either [onleft onright ^Either e]
  ((case (.type e)
     :right onright
     :left onleft) (.v e)))

(defn from-right [^Either e]
  (either (fn [_] (throw (Exception. "from-right on left value!"))) identity e))

(defn from-left [^Either e]
  (either identity (fn [_] (throw (Exception. "from-left on right value!"))) e))

(deftype Just [v]
  Object
  (equals [this other]
    (when-instance Just other
                   (= (.v this) (.v other))))
  (toString [this]
    (with-out-str (print v))))

(def nothing nil)
(def nothing? nil?)

(defn just? [v]
  (instance? Just v))

(defn from-just [^Just v]
  (cond
   (just? v) (.v v)
   (nothing? v) (throw (Exception. "Can't get something from nothing!"))
   :else (throw (Exception. (str v " is neither something nor nothing!")))))

(def just #(Just. %))

(defn maybe [on-nothing on-just m]
  (if m
    (on-just (from-just m))
    on-nothing))

(extend-protocol m/Monoid
  clojure.lang.PersistentVector$ChunkedSeq
  (mempty? [self] (empty? self))
  (mempty [self] [])
  (value [self] self)
  (<> [self o] (concat self o)))
