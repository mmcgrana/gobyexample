
(ns bounder.core
  (:require [bounder.html :as html]
            [domina :refer [value set-value! single-node]]
            [domina.css :refer [sel]]
            [lowline.functions :refer [debounce]]
            [enfocus.core :refer [at]]
            [cljs.reader :as reader]
            [clojure.string :as s])
  (:require-macros [enfocus.macros :as em]))

(def filter-input 
  (single-node 
    (sel ".search input")))

(defn project-matches [query project]
  (let [words (cons (:name project)
                    (map name (:categories project)))
        to-match (->> words
                   (s/join "")
                   (s/lower-case))]
    (<= 0 (.indexOf to-match (s/lower-case query)))))

(defn apply-filter-for [projects]
 (let [query (value filter-input)]
   (html/render-projects 
     (filter (partial project-matches query)
             projects))))

(defn filter-category [projects evt]
  (let [target (.-currentTarget evt)]
    (set-value! filter-input 
                (.-innerHTML target))
    (apply-filter-for projects)))

(defn init-listeners [projects]
  (at js/document
    ["input"] (em/listen
                :keyup
                (debounce
                  (partial apply-filter-for projects)
                  500))
    [".category-links li"] (em/listen
                             :click
                             (partial filter-category projects))))

(defn init [projects-edn]
  (let [projects (reader/read-string projects-edn)]
    (init-listeners projects)
    (html/render-projects projects)
    (html/loaded)))

