(ns doppelkopf-reframe.util
  (:require [re-frame.core :refer [subscribe]]))


(defn listen
  [query-v]
  @(subscribe query-v))
