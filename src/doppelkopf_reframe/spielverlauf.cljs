(ns doppelkopf-reframe.spielverlauf
  (:require [re-frame.core :refer [dispatch
                                   subscribe]]
            [reagent.core :as reagent :refer [as-element]]
            [doppelkopf-reframe.bootstrap :as bs]
            [doppelkopf-reframe.util :refer [listen]]))


(defn- spielerbuttons [label spieler states toggles dispatch-key]
  [:div.form-group
   [:label label]
   [bs/button-toolbar
    (for [[index name] (map vector (range) spieler)]
      [bs/button {:key      index
                  :bsStyle  (if (states index) "primary" "default")
                  :disabled (not (toggles index))
                  :onClick  #(dispatch [dispatch-key index])} name])]])


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
  (let [create-button (fn [value]
                        ^{:key value} [bs/button {:bsStyle "default" :onClick #(dispatch [:set-spielwert (str value)])} value])]
    [:div.form-group
     [:label "Spielwert"]
     [:div.row
      [:div.col-sm-2
       [:input.text-right {:type "text" :value value :onChange #(dispatch [:set-spielwert (-> % .-target .-value)])}]]]
     [:div.row
      [bs/button-toolbar {:class "col-sm-8"}
       (map create-button (range 1 9))]]]))

(defn- spielabrechnen [abrechenbar]
  [bs/button-toolbar
   [bs/button {:bsStyle "primary" :disabled (not abrechenbar) :onClick #(dispatch [:spiel-abrechnen])} "Spiel abrechnen"]])



(defn- spieleingaben [spieler spieleingabe mit-aussetzer]
  [:div
   [spielerbuttons "Gewinner" spieler (:gewinner spieleingabe) (:toggleGewinner spieleingabe) :toggle-gewinner]
   (when mit-aussetzer
     [spielerbuttons "Aussetzer" spieler (:aussetzer spieleingabe) (:toggleAussetzer spieleingabe) :toggle-aussetzer])
   [bockrunden (:bockrunden spieleingabe)]
   [spielwert (:spielwert spieleingabe)]
   [spielabrechnen (:abrechenbar spieleingabe)]])

(defn- ergebnistabelle [spieler spielstand]
  (letfn [(create-row [index row]
            [:tr.text-center {:key index}
             [:td (inc index)]
             (map-indexed #(create-column %1 %2 row) (:punkte row))])
          (create-column [index value row]
            (let [style (cond
                          (contains? (:gewinner row) index) "gewinner"
                          (contains? (:aussetzer row) index) ""
                          :else "verlierer")]
              [:td {:key index :class style} value]))]
    [bs/table {:striped true :bordered true :condensed true :responsive true}
     [:thead
      [:tr
       [:th.text-center "Nr."]
       (map-indexed #(identity [:th.text-center {:key %1} %2]) spieler)]]
     [:tbody
      (map-indexed create-row spielstand)]]))


(defn spielverlauf  []
  (let [spieler-names (listen [:spieler-names])
        spieleingabe (listen [:spieleingabe])
        [doppelbockspiele bockspiele] (listen [:aktuelle-bockrunden])
        spielstand (listen [:spielstand])
        mit-aussetzer (listen [:fuenf])
        eingabe-titel (cond
                        (> doppelbockspiele 0) (str "Eingabe (" doppelbockspiele " Doppelbock- / " bockspiele " Bockspiele)")
                        (> bockspiele 0) (str "Eingabe (" bockspiele " Bockspiele)")
                        :else "Eingabe")
        eingabe-style (cond
                        (> doppelbockspiele 0) "danger"
                        (> bockspiele 0) "warning"
                        :else "info")]
    [:div
     [bs/panel {:header (as-element [:h3 "Aktionen"]) :bsStyle "info"}
      [bs/button-toolbar
       [bs/button {:bsStyle "primary" :disabled (empty? spielstand) :onClick #(dispatch [:letztes-spiel-aendern])} "Letztes Spiel ändern"]
       [bs/button {:bsStyle "primary" :onClick #(dispatch [:route-to :spielerauswahl])} "Einstellungen"]
       ]]
     [bs/panel {:header (as-element [:h3 eingabe-titel]) :bsStyle eingabe-style}
      [:div [spieleingaben spieler-names spieleingabe mit-aussetzer]]
      ]
     [bs/panel {:header (as-element [:h3 "Spielverlauf"]) :bsStyle "info"}
      [:div [ergebnistabelle spieler-names spielstand]]
      ]
     ]
    )
  )


(comment

  (let [eingabe (subscribe [:spieleingabe])
        spieleeingabe @eingabe]
    spieleeingabe)

  (get-in {:a [0 1 2]} [:a 1])

  ([1 2 3] 0)

  (defn dummy []
    (+ 1 2))

  (dummy)

  (and (number? js/NaN) (not (js/isNaN js/NaN)))

  (= js/NaN js/NaN)
  )
