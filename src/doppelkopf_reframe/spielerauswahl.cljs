(ns doppelkopf-reframe.spielerauswahl
  (:require [re-frame.core :refer [dispatch]]
            [reagent.core :as reagent :refer [as-element]]
            [doppelkopf-reframe.bootstrap :as bs]
            [doppelkopf-reframe.util :refer [listen]]))


(defn- spieler-name [index name]
  [:div.form-group {:key index}
   [:label.player-label (str "Spieler " (inc index) ": ")]
   [:input.form-control.player-input {:type     "text"
                                      :value    name
                                      :onChange #(dispatch [:spieler-name index (-> % .-target .-value)])}]])

(defn spielerauswahl
  []
  (let [fuenfspieler (listen [:fuenf])
        vierspieler (not fuenfspieler)
        spielernames (listen [:names])]
    [:div
     [bs/panel {:header (as-element [:h3 "Spielerauswahl"]) :bsStyle "info"}
      [:div.form-group
       [:label.checkbox-inline
        [:input {:type     "checkbox"
                 :checked  vierspieler
                 :onChange #(dispatch [:fuenf-spieler-modus false])}] "4 Spieler"]
       [:label.checkbox-inline
        [:input {:type     "checkbox"
                 :checked  fuenfspieler
                 :onChange #(dispatch [:fuenf-spieler-modus true])}] "5 Spieler"]]
      [:div.form-inline.form-group
       (map-indexed spieler-name spielernames)]
      [:div
       [bs/button {:bsStyle "primary"
                   :onClick #(dispatch [:route-to :spielverlauf])} "Zum Spiel"]]
      ]
     [bs/panel {:header (as-element [:h3 "Administration"]) :bsStyle "warning"}
      [:div
       [bs/button {:bsStyle "danger"
                   :onClick #(dispatch [:delete-ls])} "Daten löschen"]]]]
    )
  )



(comment

  (re-frame.core/dispatch-sync [:fuenf-spieler-modus false])
  (re-frame.core/dispatch-sync [:spieler-name 0 "rene"])
  )
