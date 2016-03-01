(ns doppelkopf-reframe.core
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [reagent.core :as reagent :refer [atom]]
            [re-frame.core :refer [register-handler
                                   path
                                   register-sub
                                   dispatch
                                   dispatch-sync
                                   subscribe]]
            [doppelkopf-reframe.router :as router :refer [route-panel]]
            [doppelkopf-reframe.bootstrap :as bs]
            [doppelkopf-reframe.spielerauswahl :refer [spielerauswahl]]
            [doppelkopf-reframe.spielverlauf :refer [spielverlauf]]
            [doppelkopf-reframe.db :as db]))

(enable-console-print!)




(def routes
  {:init           [:div "Initializing..."]
   :spielerauswahl [spielerauswahl]
   :spielverlauf   [spielverlauf],
   :default        :init})

(defn hello-world []
  [:div.container-fluid
   [bs/page-header "Doppelkopf-App"]
   [route-panel routes]])


(dispatch [:init-db])


(reagent/render-component [hello-world]
                          (. js/document (getElementById "app")))


(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
  )


(comment

  (defonce app-state (atom {:text "Hello world!"}))

  (defn my-div [props text]
    (println text)
    [:div props text])

  (defn my-div-2 [props text]
    (println "outer: " text)
    (fn [props text]
      (println "inner: " text)
      [:div props text]))


  (map #(identity [:h2 (str "hallo:" %)]) (range 4))
  (vector :h2 (str "hallo"))

  (if-let [a 5]
    1
    2)

  (dispatch-sync [:route-to :r1])

  )

