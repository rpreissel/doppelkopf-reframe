(ns hello-world-reframe.db
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [clojure.set :as set]
            [reagent.core :as reagent :refer [atom]]
            [re-frame.core :refer [register-handler
                                   register-sub
                                   dispatch
                                   dispatch-sync
                                   subscribe]]))


(def initialStateVierSpieler
  {:gewinner        [false false false false false],
   :toggleGewinner  [true true true true false],
   :aussetzer       [false false false false true],
   :toggleAussetzer [false false false false false],
   :spielwert       0,
   :abrechenbar     false,
   :bockrunden      0 })

(def initialStateFuenfSpieler
  {:gewinner        [false false false false false],
   :toggleGewinner  [true true true true true],
   :aussetzer       [false false false false false],
   :toggleAussetzer [true true true true true],
   :spielwert       0,
   :abrechenbar     false,
   :bockrunden      0 })


(def default-db
  {:spieler
   { :names ["Spieler1" "Spieler2" "Spieler3" "Spieler4" "Spieler5" ]
     :fuenf true}
   :spieleingabe initialStateFuenfSpieler
   :spiele []
   :route :spielerauswahl})


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
        anzahlgewinner (reduce #(+ %1 (if %2 1 0)) 0 gewinner)
        togglegewinner-korrigiert (map #(or %1 (< anzahlgewinner 3)) gewinner)
        aussetzer (get-in db [:spieleingabe :aussetzer])
        aussetzer-korrigiert (map #(if %1 false %2) gewinner aussetzer)
        toggleaussetzer-korrigiert (map #(and fuenfspieler (not %1)) gewinner)
        aussetzer-vorhanden (some identity aussetzer-korrigiert)
        abrechenbar (and (> anzahlgewinner 0) (or (not fuenfspieler) aussetzer-vorhanden))]
    (-> db
        (assoc-in [:spieleingabe  :aussetzer] (vec aussetzer-korrigiert))
        (assoc-in [:spieleingabe  :toggleGewinner] (vec togglegewinner-korrigiert))
        (assoc-in [:spieleingabe  :toggleAussetzer] (vec toggleaussetzer-korrigiert))
        (assoc-in [:spieleingabe  :abrechenbar] (not (nil? abrechenbar))))
    ))

(defn toggle-gewinner [db spieler]
  (if-let [toggleable (get-in db [:spieleingabe :toggleGewinner spieler])]
    (-> db
        (update-in [:spieleingabe :gewinner spieler] not)
        (update-aussetzer-toggle-und-abrechenbar-state))
    db))

(defn toggle-aussetzer [db spieler]
  (if-let [toggleable (get-in db [:spieleingabe :toggleAussetzer spieler])]
    (let [aussetzer (get-in db [:spieleingabe :aussetzer])
          aussetzer-korrigiert (map-indexed #(and (= %1 spieler) (not %2)) aussetzer) ]
      (-> db
          (assoc-in [:spieleingabe :aussetzer] (vec aussetzer-korrigiert))
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
    (when (> s 0) (nth v (dec s)))))

(defn aktuelle-bockrunden [db]
  (let [last-spiel (vlast (:spiele db))]
    (if last-spiel (:aktuelle-bockrunden last-spiel) [0 0])))

(defn- berechne-neue-bockrunden [[bis-doppel bis-einzel] neue-bockrunden fuenf solospiel]
  (let [[basis-doppel basis-einzel] (if solospiel
                                      [bis-doppel bis-einzel]
                                      (if (> bis-doppel 0)
                                        [(dec bis-doppel) bis-einzel]
                                        (if (> bis-einzel 0)
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
  (let [gewinner-flags    (:gewinner e)
        aussetzer-flags   (:aussetzer e)
        gewinner          (set (keep-indexed #(if %2 %1) gewinner-flags))
        aussetzer         (set (keep-indexed #(if %2 %1) aussetzer-flags))
        verlierer         (set/difference (set (range 5)) gewinner aussetzer)
        anzahlgewinner    (count gewinner)
        anzahlverlierer   (count verlierer)
        punktzahl         (:spielwert e)
        gewinnerpunkte    (if (= anzahlgewinner 1) (* 3 punktzahl) punktzahl)
        verliererpunkte   (- 0 (/ (* gewinnerpunkte anzahlgewinner) anzahlverlierer))
        punkte            (vec (map #(cond
                                       (contains? gewinner %) gewinnerpunkte
                                       (contains? verlierer %) verliererpunkte
                                       :else 0) (range 5)))
        bockrunden        (:bockrunden e)]
    {:gewinner gewinner
     :aussetzer aussetzer
     :spielwert punktzahl
     :punkte punkte
     :bockrunden bockrunden
     :aktuelle-bockrunden [0 0]}))

(defn- spiel->spieleingabe [s]
   {:gewinner        (vec (map #(contains? (:gewinner s) %) (range 5)))
    :toggleGewinner  [true true true true true],
    :aussetzer       (vec (map #(contains? (:aussetzer s) %) (range 5))),
    :toggleAussetzer [true true true true true],
    :spielwert       (:spielwert s),
    :abrechenbar     true,
    :bockrunden      (:bockrunden s)})

(defn spiel-abrechnen [db]
  (let [spieleingabe                  (:spieleingabe db)
        neues-spiel                   (spieleingabe->spiel spieleingabe)
        solospiel                     (not= (count (:gewinner neues-spiel)) 2)
        bockrunden                    (:bockrunden neues-spiel)
        fuenfspieler                  (get-in db [:spieler :fuenf])
        bisherige-aktuelle-bockrunden (aktuelle-bockrunden db)
        neue-aktuelle-bockrunden (berechne-neue-bockrunden bisherige-aktuelle-bockrunden bockrunden fuenfspieler solospiel)
        neues-spiel-mit-bockrunden    (assoc neues-spiel :aktuelle-bockrunden neue-aktuelle-bockrunden)]
    (-> db
        (update :spiele conj neues-spiel-mit-bockrunden)
        (assoc :spieleingabe (if fuenfspieler initialStateFuenfSpieler initialStateVierSpieler)))))

(defn add-punkte [spiele]
  (loop [result []
         last-punkte [0 0 0 0 0]
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

(defn spielstand [db]
  (add-punkte (:spiele db)))

(register-handler
  :init-db
  (fn [_ _]
    default-db))

(register-handler
  :fuenf-spieler-modus
  (fn [db [_ fuenfspieler]]
    (println "fuenfspieler: " fuenfspieler)
    (toggle-fuenf-spieler-modus db fuenfspieler)))

(register-handler
  :spieler-name
  (fn [db [_ index name]]
    (assoc-in db [:spieler :names index] name)))

(register-handler
  :toggle-gewinner
  (fn [db [_ spieler]]
    (toggle-gewinner db spieler)))

(register-handler
  :toggle-aussetzer
  (fn [db [_ spieler]]
    (toggle-aussetzer db spieler)))


(register-handler
  :add-bockrunde
  (fn [db _]
    (add-bockrunde db)))

(register-handler
  :reset-bockrunden
  (fn [db _]
    (reset-bockrunden db)))

(register-handler
  :set-spielwert
  (fn [db [_ value]]
    (set-spielwert db value)))

(register-handler
  :spiel-abrechnen
  (fn [db _]
    (let [new-db  (spiel-abrechnen db)]
      (println "spiele:" (:spiele new-db))
      new-db)))

(register-handler
  :letztes-spiel-aendern
  (fn [db _]
    (letztes-spiel-aendern db)))



(register-sub
  :spieler
  (fn  [db]
    (reaction
      (:spieler @db))))

(register-sub
  :spieler-names
  (fn  [db]
    (reaction
      (let [fuenf (get-in @db [:spieler :fuenf])
            names (get-in @db [:spieler :names]) ]
        (if fuenf names (butlast names))))))


(register-sub
  :spieleingabe
  (fn  [db]
    (reaction
      (:spieleingabe @db))))

(register-sub
  :aktuelle-bockrunden
  (fn  [db]
    (reaction
      (aktuelle-bockrunden @db))))

(register-sub
  :spielstand
  (fn  [db]
    (reaction
      (spielstand @db))))




(comment
  (count [1 2])
  (vlast [1 2])
  (vlast [])
  (spiel-abrechnen {:spiele []
                    :spieleingabe {
                                    :gewinner [true, true, true, false, false]
                                    :aussetzer [false, false, false, true, false]
                                    :spielwert 1
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

  (add-punkte [{:punkte [1 1 1 1 1]} {:punkte [1 1 1 1 1]}])

  (spiel->spieleingabe {
     :gewinner #{0 1}
     :aussetzer #{4}
     :spielwert 1
     :bockrunden 1
     })
  )
