(ns doppelkopf-reframe.router
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [re-frame.core :refer [register-handler
                                   register-sub
                                   subscribe]]))


(register-sub
  :route
  (fn [db]
    (reaction
      (if-let [route (:route @db)]
        route
        :default))))

(register-handler
  :route-to
  (fn [db [_ route]]
    (assoc db :route route)))


(defn route-panel
  [routes]
  (let [route-key (subscribe [:route])]
    (fn [routes]
      (let [raw-route (@route-key routes)
            route (if (keyword? raw-route)
                    (raw-route routes)
                    raw-route)]
        route))))
