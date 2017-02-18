(ns doppelkopf-reframe.router
  (:require [re-frame.core :refer [reg-event-db
                                   reg-sub]]
            [doppelkopf-reframe.util :refer [listen]]))


(reg-sub
  :route
  (fn [db [_]]
    (if-let [route (:route db)]
      route
      :default)))

(reg-event-db
  :route-to
  (fn [db [_ route]]
    (assoc db :route route)))


(defn route-panel
  [routes]
  (let [route-key (listen [:route])
        raw-route (route-key routes)
        route (if (keyword? raw-route)
                (raw-route routes)
                raw-route)]
    route))
