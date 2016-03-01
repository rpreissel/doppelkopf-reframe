(ns hello-world-reframe.bootstrap
  (:require [reagent.core :as reagent]
            [cljsjs.react-bootstrap]))

(def page-header (reagent/adapt-react-class (aget js/ReactBootstrap "PageHeader")))

(def button (reagent/adapt-react-class (aget js/ReactBootstrap "Button")))

(def button-toolbar (reagent/adapt-react-class (aget js/ReactBootstrap "ButtonToolbar")))

(def panel (reagent/adapt-react-class (aget js/ReactBootstrap "Panel")))

(def table (reagent/adapt-react-class (aget js/ReactBootstrap "Table")))
