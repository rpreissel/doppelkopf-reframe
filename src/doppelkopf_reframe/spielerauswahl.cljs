(ns doppelkopf-reframe.spielerauswahl
  (:require [re-frame.core :refer [register-handler
                                   register-sub
                                   dispatch
                                   subscribe]]
            [reagent.core :as reagent :refer [as-element]]
            [doppelkopf-reframe.bootstrap :as bs]))


(defn- spieler-name [index name]
  [:div.form-group {:key index}
   [:label.player-label (str "Spieler " (inc index) ": ")]
   [:input.form-control.player-input {:type     "text"
                                      :value    name
                                      :onChange #(dispatch [:spieler-name index (-> % .-target .-value)])}]])

(defn spielerauswahl
  []
  (let [spieler (subscribe [:spieler])]
    (fn []
      (let [fuenfspieler (:fuenf @spieler)
            vierspieler (not fuenfspieler)
            spielernames (:names @spieler)]
        [:div
         [bs/panel {:header (as-element [:h3 "Spielerauswahl"]) :bsStyle "info"}
          [:div.form-group
           [:label.checkbox-inline
            [:input {:type     "checkbox"
                     :value    4
                     :checked  vierspieler
                     :onChange #(dispatch [:fuenf-spieler-modus false])} "4 Spieler"]]
           [:label.checkbox-inline
            [:input {:type     "checkbox"
                     :value    5
                     :checked  fuenfspieler
                     :onChange #(dispatch [:fuenf-spieler-modus true])} "5 Spieler"]]]
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
    ))


(comment

  (re-frame.core/dispatch-sync [:fuenf-spieler-modus false])
  (re-frame.core/dispatch-sync [:spieler-name 0 "rene"])
  )
