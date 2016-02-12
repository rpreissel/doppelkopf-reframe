(ns hello-world-reframe.spielverlauf
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [re-frame.core :refer [register-handler
                                   register-sub
                                   dispatch
                                   subscribe]]
            [reagent.core :as reagent :refer [as-element]]
            [hello-world-reframe.bootstrap :as bs]))


(defn- spielerbutton [index name active toggleable dispatch-key ]
  [bs/button {:key index :bsStyle (if active "primary" "default") :disabled (not toggleable)
              :onClick #(dispatch [dispatch-key index])} name])

(defn- spielerbuttons [label spieler states toggles dispatch-key]
  (letfn [(create-button [index name]
                         ^{:key index}[spielerbutton index name (states index) (toggles index) dispatch-key])]
    [:div.form-group
     [:label label]
     [bs/button-toolbar
      (map-indexed create-button spieler)]]))


(defn- bockrunden [value]
  (let [label (cond
                (= value 0) "Keine Bockrunde"
                (= value 1) "1 Bockrunde"
                :else (str value " Bockrunden"))]
    [:div.form-group
     [:label "Bockrunden"]
     [bs/button-toolbar
      [bs/button {:bsStyle "default" :onClick #(dispatch [:add-bockrunde])} label]
      [bs/button {:bsStyle "default" :onClick #(dispatch [:reset-bockrunden])} "Bockrunden zurücksetzen"]]]))

(defn- spielwert [value]
  (letfn [(create-button [value]
                         ^{:key value} [bs/button {:bsStyle "default" :onClick #(dispatch [:set-spielwert (str value)])} value])]
    [:div.form-group
     [:label "Spielwert"]
     [:div.row
      [:div.col-sm-2
       [:input.text-right {:type "text" :value value :onChange #(dispatch [:set-spielwert (-> % .-target .-value)])}]]]
     [:div.row
      [bs/button-toolbar {:class "col-sm-8"}
       (map create-button (drop 1 (range 9)))]]]))


(defn- spieleingabe [spieler spieleingabe mit-aussetzer]
  [:div
   [spielerbuttons "Gewinner" spieler (:gewinner spieleingabe) (:toggleGewinner spieleingabe) :toggle-gewinner]
   (when mit-aussetzer
     [spielerbuttons "Aussetzer" spieler (:aussetzer spieleingabe) (:toggleAussetzer spieleingabe) :toggle-aussetzer])
   [bockrunden (:bockrunden spieleingabe)]
   [spielwert (:spielwert spieleingabe)]
   ])

(defn spielverlauf
  []
  (let [spieler-names-rx (subscribe [:spieler-names])
        spieleingabe-rx (subscribe [:spieleingabe])]
    (fn []
      (let [mit-aussetzer (some identity (:toggleAussetzer @spieleingabe-rx))]
        [:div
         [bs/panel {:header (as-element [:h3 "Aktionen"]) :bsStyle "info"}
          [bs/button-toolbar
           [bs/button {:bsStyle "primary"} "Letztes Spiel ändern"]
           [bs/button {:bsStyle "primary" :onClick #(dispatch [:route-to :spielerauswahl])} "Einstellungen"]
           ]]
         [bs/panel {:header (as-element [:h3 "Eingabe"]) :bsStyle "info"}
          [:div [spieleingabe @spieler-names-rx @spieleingabe-rx mit-aussetzer]]
          ]
         ]
        )
      )
    ))


(comment

  (let [eingabe (subscribe [:spieleingabe])
        spieleeingabe @eingabe]
    spieleeingabe)

  (get-in {:a [0 1 2]} [:a 1])

  ([1 2 3] 0)

  (defn dummy []
    (+ 1 2) )

  (dummy)

  (and (number? js/NaN) (not (js/isNaN js/NaN)))

  (= js/NaN js/NaN)
  )
