(ns doppelkopf-reframe.db
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [clojure.set :as set]
            [reagent.core :as reagent :refer [atom]]
            [re-frame.core :refer [reg-event-db
                                   reg-sub
                                   after
                                   subscribe]]
            [cljs.reader]
            [schema.core :as s :include-macros true]))


(def schema {:spieler      {:names [s/Str]
                            :fuenf s/Bool}
             :route        (s/enum :spielerauswahl :spielverlauf)
             :spieleingabe {:gewinner        [s/Bool]
                            :aussetzer       [s/Bool]
                            :toggleGewinner  [s/Bool]
                            :toggleAussetzer [s/Bool]
                            :spielwert       s/Int
                            :abrechenbar     s/Bool
                            :bockrunden      s/Int}
             :spiele       [{:gewinner            #{s/Int}
                             :aussetzer           #{s/Int}
                             :spielwert           s/Int
                             :punkte              [s/Int]
                             :bockrunden          s/Int
                             :aktuelle-bockrunden (s/pair s/Int "doppelbock" s/Int "bock")}]
             })

(defn check-and-throw
  "throw an exception if db doesn't match the schema."
  [a-schema db]
  (if-let [problems (s/check a-schema db)]
    (throw (js/Error. (str "schema check failed: " problems)))))

;; after an event handler has run, this middleware can check that
;; it the value in app-db still correctly matches the schema.
(def check-schema-mw (after (partial check-and-throw schema)))


(def lsk "doppelkopf-reframe")

(defn state->ls!
  [db]
  (.setItem js/localStorage lsk (str db)))

(defn ls->state
  []
  (some->> (.getItem js/localStorage lsk)
           (cljs.reader/read-string)                        ;; stored as an EDN map.
           ))

(def ->ls (after state->ls!))


(def doppelkopf-middleware [check-schema-mw                 ;; after ever event handler make sure the schema is still valid
                            ->ls                            ;; write to localstore each time
                            ])


(def initialStateVierSpieler
  {:gewinner        [false false false false false],
   :toggleGewinner  [true true true true false],
   :aussetzer       [false false false false true],
   :toggleAussetzer [false false false false false],
   :spielwert       0,
   :abrechenbar     false,
   :bockrunden      0})

(def initialStateFuenfSpieler
  {:gewinner        [false false false false false],
   :toggleGewinner  [true true true true true],
   :aussetzer       [false false false false false],
   :toggleAussetzer [true true true true true],
   :spielwert       0,
   :abrechenbar     false,
   :bockrunden      0})


(def default-db
  {:spieler
                 {:names ["Spieler1" "Spieler2" "Spieler3" "Spieler4" "Spieler5"]
                  :fuenf true}
   :spieleingabe initialStateFuenfSpieler
   :spiele       []
   :route        :spielerauswahl})


(defn toggle-fuenf-spieler-modus [db fuenfspieler]
  (let [fuenfspielerdb (get-in db [:spieler :fuenf])]
    (if-not (= fuenfspieler fuenfspielerdb)
      (-> db
          (assoc-in [:spieler :fuenf] fuenfspieler)
          (assoc :spieleingabe (if fuenfspieler initialStateFuenfSpieler initialStateVierSpieler))
          )
      db)))


(defn update-aussetzer-toggle-und-abrechenbar-state [db]
  (let [fuenfspieler (get-in db [:spieler :fuenf])
        gewinner (get-in db [:spieleingabe :gewinner])
        anzahlgewinner (-> gewinner (frequencies) (get true 0))
        togglegewinner-korrigiert (mapv #(or %1 (< anzahlgewinner 3)) gewinner)
        aussetzer (get-in db [:spieleingabe :aussetzer])
        aussetzer-korrigiert (mapv #(if %1 false %2) gewinner aussetzer)
        toggleaussetzer-korrigiert (mapv #(and fuenfspieler (not %1)) gewinner)
        aussetzer-vorhanden (some identity aussetzer-korrigiert)
        abrechenbar (and (pos? anzahlgewinner) (or (not fuenfspieler) aussetzer-vorhanden))]
    (-> db
        (assoc-in [:spieleingabe :aussetzer] aussetzer-korrigiert)
        (assoc-in [:spieleingabe :toggleGewinner] togglegewinner-korrigiert)
        (assoc-in [:spieleingabe :toggleAussetzer] toggleaussetzer-korrigiert)
        (assoc-in [:spieleingabe :abrechenbar] (boolean abrechenbar)))
    ))

(defn toggle-gewinner [db spieler]
  (if (get-in db [:spieleingabe :toggleGewinner spieler])
    (-> db
        (update-in [:spieleingabe :gewinner spieler] not)
        (update-aussetzer-toggle-und-abrechenbar-state))
    db))

(defn toggle-aussetzer [db spieler]
  (if (get-in db [:spieleingabe :toggleAussetzer spieler])
    (let [war-aussetzer (get-in db [:spieleingabe :aussetzer spieler])
          aussetzer-korrigiert (mapv #(if war-aussetzer false (= spieler %)) (range 5))]
      (-> db
          (assoc-in [:spieleingabe :aussetzer] aussetzer-korrigiert)
          (update-aussetzer-toggle-und-abrechenbar-state)))
    db))

(defn add-bockrunde [db]
  (update-in db [:spieleingabe :bockrunden] inc))

(defn reset-bockrunden [db]
  (assoc-in db [:spieleingabe :bockrunden] 0))


(defn set-spielwert [db value]
  (let [parsed-value (if (= value "") 0 (js/parseInt value))]
    (if (and (number? parsed-value) (not (js/isNaN parsed-value)))
      (assoc-in db [:spieleingabe :spielwert] parsed-value)
      db)))

(defn- vlast [v]
  (let [s (count v)]
    (when (pos? s) (nth v (dec s)))))

(defn aktuelle-bockrunden [spiele]
  (let [last-spiel (vlast spiele)]
    (if last-spiel (:aktuelle-bockrunden last-spiel) [0 0])))

(defn- berechne-neue-bockrunden [[bis-doppel bis-einzel] neue-bockrunden fuenf solospiel]
  (let [[basis-doppel basis-einzel] (if solospiel
                                      [bis-doppel bis-einzel]
                                      (if (pos? bis-doppel)
                                        [(dec bis-doppel) bis-einzel]
                                        (if (pos? bis-einzel)
                                          [0 (dec bis-einzel)]
                                          [0 0])))

        bockrunde1 (+ basis-doppel basis-einzel)
        bockrunde2 basis-doppel
        anzahl-bockrunden (if fuenf 5 4)
        doppel (* (quot neue-bockrunden 2) anzahl-bockrunden)
        einzel (* (rem neue-bockrunden 2) anzahl-bockrunden)
        neu-bockrunde1 (+ bockrunde1 (+ doppel (if (<= bockrunde1 bockrunde2) einzel 0)))
        neu-bockrunde2 (+ bockrunde2 (+ doppel (if-not (<= bockrunde1 bockrunde2) einzel 0)))]
    (if (> neu-bockrunde1 neu-bockrunde2)
      [neu-bockrunde2 (- neu-bockrunde1 neu-bockrunde2)]
      [neu-bockrunde1 (- neu-bockrunde2 neu-bockrunde1)])))

(defn- spieleingabe->spiel [e]
  (let [gewinner (set (keep-indexed #(when %2 %1) (:gewinner e)))
        aussetzer (set (keep-indexed #(when %2 %1) (:aussetzer e)))
        verlierer (set/difference (set (range 5)) gewinner aussetzer)
        anzahlgewinner (count gewinner)
        anzahlverlierer (count verlierer)
        punktzahl (:spielwert e)
        gewinnerpunkte (if (= anzahlgewinner 1) (* 3 punktzahl) punktzahl)
        verliererpunkte (- 0 (/ (* gewinnerpunkte anzahlgewinner) anzahlverlierer))
        punkte (mapv #(cond
                       (gewinner %) gewinnerpunkte
                       (verlierer %) verliererpunkte
                       :else 0) (range 5))]
    {:gewinner            gewinner
     :aussetzer           aussetzer
     :spielwert           punktzahl
     :punkte              punkte
     :bockrunden          (:bockrunden e)
     :aktuelle-bockrunden [0 0]}))

(defn- spiel->spieleingabe [s]
  {:gewinner        (mapv #(contains? (:gewinner s) %) (range 5))
   :toggleGewinner  [true true true true true],
   :aussetzer       (mapv #(contains? (:aussetzer s) %) (range 5)),
   :toggleAussetzer [true true true true true],
   :spielwert       (:spielwert s),
   :abrechenbar     true,
   :bockrunden      (:bockrunden s)})

;Test
(defn spiel-abrechnen [db]
  (let [spieleingabe (:spieleingabe db)
        neues-spiel (spieleingabe->spiel spieleingabe)
        solospiel (not= (count (:gewinner neues-spiel)) 2)
        bockrunden (:bockrunden neues-spiel)
        fuenfspieler (get-in db [:spieler :fuenf])
        bisherige-aktuelle-bockrunden (aktuelle-bockrunden (:spiele db))
        neue-aktuelle-bockrunden (berechne-neue-bockrunden bisherige-aktuelle-bockrunden bockrunden fuenfspieler solospiel)
        neues-spiel-mit-bockrunden (assoc neues-spiel :aktuelle-bockrunden neue-aktuelle-bockrunden)]
    (-> db
        (update :spiele conj neues-spiel-mit-bockrunden)
        (assoc :spieleingabe (if fuenfspieler initialStateFuenfSpieler initialStateVierSpieler)))))

(defn spielstand [spiele fuenf-spieler]
  (loop [result []
         last-punkte (if fuenf-spieler [0 0 0 0 0] [0 0 0 0])
         [head & rest] spiele]
    (if head
      (let [new-last-punkte (map + last-punkte (:punkte head))]
        (recur (conj result (assoc head :punkte new-last-punkte)) new-last-punkte rest))
      result)))


(defn letztes-spiel-aendern [db]
  (let [spieleingabe (spiel->spieleingabe (vlast (:spiele db)))
        spiele (vec (butlast (:spiele db)))]
    (-> db
        (assoc :spieleingabe spieleingabe)
        (assoc :spiele spiele)
        (update-aussetzer-toggle-und-abrechenbar-state))))

(defn init-db []
  (if-let [storage (ls->state)]
    storage
    default-db))

(reg-event-db
  :init-db
  check-schema-mw
  (fn [_ _]
    (init-db)))

(reg-event-db
  :fuenf-spieler-modus
  doppelkopf-middleware
  (fn [db [_ fuenfspieler]]
    (println "fuenfspieler: " fuenfspieler)
    (toggle-fuenf-spieler-modus db fuenfspieler)))

(reg-event-db
  :spieler-name
  doppelkopf-middleware
  (fn [db [_ index name]]
    (assoc-in db [:spieler :names index] name)))

(reg-event-db
  :toggle-gewinner
  doppelkopf-middleware
  (fn [db [_ spieler]]
    (toggle-gewinner db spieler)))

(reg-event-db
  :toggle-aussetzer
  doppelkopf-middleware
  (fn [db [_ spieler]]
    (toggle-aussetzer db spieler)))


(reg-event-db
  :add-bockrunde
  doppelkopf-middleware
  (fn [db _]
    (add-bockrunde db)))

(reg-event-db
  :reset-bockrunden
  doppelkopf-middleware
  (fn [db _]
    (reset-bockrunden db)))

(reg-event-db
  :set-spielwert
  doppelkopf-middleware
  (fn [db [_ value]]
    (set-spielwert db value)))

(reg-event-db
  :spiel-abrechnen
  doppelkopf-middleware
  (fn [db _]
    (let [new-db (spiel-abrechnen db)]
      (println "spiele:" (:spiele new-db))
      new-db)))

(reg-event-db
  :letztes-spiel-aendern
  doppelkopf-middleware
  (fn [db _]
    (letztes-spiel-aendern db)))

(reg-event-db
  :delete-ls
  doppelkopf-middleware
  (fn [db _]
    default-db))



(reg-sub
  :spieler
  (fn [db [_]]
    (:spieler db)))

(reg-sub
  :spiele
  (fn [db [_]]
    (:spiele db)))

(reg-sub
  :fuenf
  (fn [db [_]]
    (get-in db [:spieler :fuenf])))

(reg-sub
  :names
  (fn [db [_]]
    (get-in db [:spieler :names])))

(reg-sub
  :spieler-names
  :<- [:fuenf]
  :<- [:names]
  (fn [[fuenf names] [_]]
     (if fuenf names (butlast names))))


(reg-sub
  :spieleingabe
  (fn [db [_]]
    (:spieleingabe db)))

(reg-sub
  :aktuelle-bockrunden
  :<- [:spiele]
  (fn [spiele [_]]
    (aktuelle-bockrunden spiele)))

(reg-sub
  :spielstand
  :<- [:spiele]
  :<- [:fuenf]
  (fn [[spiele fuenf] [_]]
    (spielstand spiele fuenf)))





(comment
  (count [1 2])
  (vlast [1 2])
  (vlast [])
  (spiel-abrechnen {:spiele       []
                    :spieleingabe {
                                   :gewinner   [true, true, true, false, false]
                                   :aussetzer  [false, false, false, true, false]
                                   :spielwert  1
                                   :bockrunden 1}
                    })

  (berechne-neue-bockrunden [0 5] 0 true false)

  (defn lazy-test
    ([end] (lazy-test 0 end))
    ([current end]
     (if (= current end)
       nil
       (lazy-seq (cons current (lazy-test (inc current) end)))))
    )

  (lazy-test 3)

  (spielstand [{:punkte [1 1 1 1 1]} {:punkte [1 1 1 1 1]}])

  (spiel->spieleingabe {
                        :gewinner   #{0 1}
                        :aussetzer  #{4}
                        :spielwert  1
                        :bockrunden 1
                        })

  (schema.core/validate
    schema
    {:route        :spielerauswahl
     :spieler      {:names ["rene"] :fuenf true}
     :spieleingabe initialStateFuenfSpieler
     :spiele       []})

  (schema.core/validate
    schema
    default-db)

  (-> [true true false false] (frequencies) (get true))

  (map drop-last [[1 2 3 4 5]])
  )
