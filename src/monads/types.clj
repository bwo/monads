(ns monads.types)

(set! *warn-on-reflection* true)

(defprotocol MRun
  (mrun [this m]))

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
  (mrun [_ m] ((:return m) v)))

(deftype Mplus [l r]
  Object
  (toString [this]
    (with-out-str (print l r)))
  MRun
  (mrun [_ m] ((-> m :monadplus :mplus) [l r])))

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
  (mrun [_ m] ((:bind m) (mrun comp m) f)))

(defmacro if-instance [cls obj then else]
  `(if (instance? ~cls ~obj)
     (let [~(with-meta obj {:tag cls}) ~obj]
       ~then)
     ~else))

(defmacro when-instance [cls obj & forms]
  `(when (instance? ~cls ~obj)
    (let [~(with-meta obj {:tag cls}) ~obj]
      ~@forms)))

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

(defn fst [^Pair o] (.fst o))
(defn snd [^Pair o] (.snd o))


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

