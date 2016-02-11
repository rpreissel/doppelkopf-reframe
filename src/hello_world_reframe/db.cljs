(ns hello-world-reframe.db
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [reagent.core :as reagent :refer [atom]]
            [re-frame.core :refer [register-handler
                                   register-sub
                                   dispatch
                                   dispatch-sync
                                   subscribe]]))


(def initialStateVierSpieler
  {:gewinner        [false false false false false],
   :toggleGewinner  [true true true true false],
   :aussetzer       [false false false false false],
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
   :route :spielerauswahl})


(defn toggle-fuenf-spieler-modus [db fuenfspieler]
  (let [fuenfspielerdb (get-in db [:spieler :fuenf])]
    (when-not (= fuenfspieler fuenfspielerdb)
      (-> db
          (assoc-in [:spieler :fuenf] fuenfspieler)
          (assoc :spieleingabe (if fuenfspieler initialStateFuenfSpieler initialStateVierSpieler))
    ))))


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
    )
  )

(defn toggle-gewinner [db spieler]
  (when-let [toggleable (get-in db [:spieleingabe :toggleGewinner spieler])]
    (-> db
        (update-in [:spieleingabe :gewinner spieler] not)
        (update-aussetzer-toggle-und-abrechenbar-state))
    ))

(defn toggle-aussetzer [db spieler]
  (when-let [toggleable (get-in db [:spieleingabe :toggleAussetzer spieler])]
    (let [aussetzer (get-in db [:spieleingabe :aussetzer])
          aussetzer-korrigiert (map-indexed #(and (= %1 spieler) (not %2)) aussetzer) ]
      (-> db
          (assoc-in [:spieleingabe :aussetzer] (vec aussetzer-korrigiert))
          (update-aussetzer-toggle-und-abrechenbar-state))
      )))

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

