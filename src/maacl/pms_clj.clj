(ns maacl.pms-clj
  (:require [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [hiccup2.core :as h]
            [weave.core :as weave])
  (:gen-class))

(comment "This is a small experiment inspired by Oskar Wickströms
  excellent work at
  https://haskell-at-work.com/episodes/2018-01-19-domain-modelling-with-haskell-data-structures.html. I
  wanted to see what would be involved in building the equivalent
  functionality in reasonably ideomatic Clojure. It is also my first
  from scratch use of Clojure spec, which was a very interesting and
  productive experience. It is amazing how little work one has to do
  to be able to generate example datastructures for testing. The
  generated examples helped me find a subtle bug in the tree pretty
  printer, that would have been hard to find without."  "I would love
  any feedback on the code."

         "The purpose of the code is to model a very simple project
  management system and implement simple reporting for same. Hopefully
  the specs makes the code fairly self-explanatory :-)")

(defrecord Sale [amount])
(defrecord Purchase [amount])

(s/def :project/id pos-int?)
(s/def :project/name (s/and string? seq))
(s/def :project/prj-list (s/and (s/coll-of ::project :gen-max 5) seq))

;; A project is either a simple project or a group of projects.
(s/def ::project
  (s/or :prj (s/keys :req-un [:project/name :project/id])
        :prj-group (s/keys :req-un [:project/name :project/prj-list])))

(s/def ::money decimal?)
(s/def :budget/income ::money)
(s/def :budget/expenditure ::money)
(s/def ::budget (s/keys :req-un [:budget/income :budget/expenditure]))

(s/def ::transaction (s/or :sale #(instance? % Sale)
                           :purchase #(instance? % Purchase)))

(s/def :report/budget-profit ::money)
(s/def :report/net-profit ::money)
(s/def :report/difference ::money)
(s/def ::report (s/keys :req-un [:report/budget-profit :report/net-profit :report/difference]))

;; This is a simple pretty-printer for a project structure. 
;; I was somewhat surprised that I couldn't find a generic tree pretty printer, but maybe I missed it.
(defmulti pp-project (fn [p & [_]] (:id p)))
(defmethod pp-project nil [{:keys [name prj-list]
                            {:keys [budget-profit net-profit difference]} :report}
                           & [indent]]
  (let [indent (or indent "")]
    (str name " - " "Budg.p.: " budget-profit " Net.p.: " net-profit " Diff.: " difference "\n"
         (apply str
                (for [p (butlast prj-list)]
                  (str indent "|\n" indent "+-"
                       (pp-project p (str indent "| "))
                       "\n")))
         indent "|\n" indent "`-"
         (pp-project (last prj-list) (str indent "  ")))))

(defmethod pp-project :default [{:keys [id name] {:keys [budget-profit net-profit difference]} :report} & [_]]
  (str " " name " [" id "] " "Budg.p.: " budget-profit " Net.p.: " net-profit " Diff.: " difference))

(defn- format-amount [v]
  (when v
    [:span {:class (cond (pos? v) "positive" (neg? v) "negative" :else "zero")} (str v)]))

(defn- report-fields [budget-profit net-profit difference]
  (list "Budg.p.: "  (format-amount budget-profit)
        " Net.p.: "  (format-amount net-profit)
        " Diff.: "   (format-amount difference)))

;; Generates a hiccup HTML tree from a project structure.
;; Dispatches on :id — nil means a project group (has :prj-list), :default means a leaf project.
(defmulti project->html (fn [p] (:id p)))

(defmethod project->html nil [{:keys [name prj-list]
                               {:keys [budget-profit net-profit difference]} :report}]
  [:div {:class "project-group"}
   [:h3 name]
   (when (or budget-profit net-profit difference)
     (into [:p {:class "report"}] (report-fields budget-profit net-profit difference)))
   [:ul
    (for [p prj-list]
      [:li (project->html p)])]])

(defmethod project->html :default [{:keys [id name]
                                    {:keys [budget-profit net-profit difference]} :report}]
  [:div {:class "project"}
   [:strong (str name " [" id "]")]
   (when (or budget-profit net-profit difference)
     (into [:span {:class "report"} " — "] (report-fields budget-profit net-profit difference)))])

(defn render-project-html
  "Renders a project structure as a full HTML document string."
  [p]
  (str
   "<!DOCTYPE html>"
   (h/html
    [:html {:lang "en"}
     [:head
      [:meta {:charset "utf-8"}]
      [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
      [:title "Project Report"]
      [:style (slurp (io/resource "report.css"))]]
     [:body
      (project->html p)]])))

;; get-budget and get-transactions just produce dummy budgets and transaction lists, ignoring the project id provided.
(defn get-budget [_]
  {:income (bigdec (/ (rand-int 1000000) 100)) :expenditure (bigdec (/ (rand-int 1000000) 100))})

(defn get-transactions [_]
  [(->Sale (bigdec (/ (rand-int 400000) 100))) (->Purchase (bigdec (/ (rand-int 400000) 100)))])

;; Transactable is a bad name, but I couldn't come up with a good alternative.
(defprotocol Transactable
  (transact [t]))

(extend-protocol Transactable
  Sale
  (transact [t]
    (:amount t))
  Purchase
  (transact [t]
    (-' (:amount t))))

(defn calculate-report [{:keys [income expenditure]} transactions]
  (let [budget-profit (- income expenditure)
        net-profit (transduce (map transact) + transactions)]
    {:budget-profit budget-profit
     :net-profit net-profit
     :difference (- net-profit budget-profit)}))

;; This is the top-leve reporting function which returns a project structure enriched with :report key/values at all levels of the structure.
(defmulti calculate-project-report :prj-list)

(defmethod calculate-project-report nil [p]
  (assoc p :report
         (calculate-report (get-budget p) (get-transactions p))))

(defmethod calculate-project-report :default [p]
  (let [reported-prj-list (map calculate-project-report (:prj-list p))]
    (assoc p :report
           (transduce (map :report) (partial merge-with +) reported-prj-list)
           :prj-list reported-prj-list)))

;; This is a hard coded example.
(def some-project
  {:name "Sweden"
   :prj-list [{:name "Stockholm"
               :prj-list [{:id 1 :name "Djurgaarden"}
                          {:id 2 :name "Skaergaarden"}]}
              {:id 3
               :name "Gothenborg"}
              {:name "Malmo"
               :prj-list [{:name "Malmo City"
                           :prj-list [{:id 41 :name "Fosie1"}
                                      {:id 42 :name "Fosie2"}
                                      {:name "Fosie3"
                                       :prj-list [{:id 31 :name "Djurgaarden"}
                                                  {:id 32 :name "Skaergaarden"}]}
                                      {:id 5 :name "Rosengaard"}]}
                          {:name "Limhamn"
                           :prj-list [{:id 6 :name "Kalkbrottet"}
                                      {:id 7 :name "Sibbarp"}]}]}
              {:id 4
               :name "Eskilstuna"}]})

(defn view []
  (weave/push-html!
   [:html {:lang "en"}
    [:head
     [:meta {:charset "utf-8"}]
     [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
     [:title "Project Report"]
     [:style (slurp (io/resource "report.css"))]]
    [:body
     [:div {:id "report-container"}]]])
  (while
   true
    (Thread/sleep 500)
    (weave/push-html!
     [:div {:id "report-container"}
      (project->html (calculate-project-report (first (gen/sample (s/gen ::project) 1))))])))

(defn -main []
  (weave/run #'view {:tailwind false}))


(comment
  (-main)
  (print (pp-project (calculate-project-report some-project)))

  ;; This will generate an print example project structures incl. reporting.
  (print (pp-project (calculate-project-report (first (gen/sample (s/gen ::project) 1)))))

  ;; Render as HTML
  (println (render-project-html (calculate-project-report some-project)))

  (spit "project-report.html"
        (render-project-html (calculate-project-report some-project))))
