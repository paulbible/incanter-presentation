(ns incanter-test.core
  (:require [incanter.core]
            [incanter.io]
            [incanter.charts]
            [incanter.stats]
            [incanter.datasets]
            [incanter.optimize]))

(use '(incanter core io charts stats datasets optimize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Good place to start http://incanter.org/docs/data-sorcery-new.pdf
;;  
;;  Many examples take from this presentaiton.
;;

;;;;; Basic structure, dataset, like data-frames in R
(dataset ["x1" "x2" "x3"]
         [[1 2 3]
          [4 5 6]
          [7 8 9]])

(to-dataset [{"x1" 1, "x2" 2, "x3" 3}
             {"x1" 4, "x2" 5, "x3" 6}
             {"x1" 7, "x2" 8, "x3" 9}])

(to-dataset [[1 2 3]
             [4 5 6]
             [7 8 9]])

; like cbind in R
(conj-cols [1 4 7] [2 5 8] [3 6 9])

; like rbind in R
(conj-rows [1 2 3] [4 5 6] [7 8 9])

; incanter built-in datasets, 
(get-dataset :cars)


;;;;; Reading data from a file
(def quotes-fname "data/HistoricalQuotes_T.csv")

(read-dataset quotes-fname
              :delim \,
              :header true)

;; A dataset is a map
; load the cars data set incanter.datasets
(def cars-ds
  (get-dataset :cars))

(keys cars-ds)

(:column-names cars-ds)

; selecting columns
($ :speed cars-ds)

($ :dist cars-ds)

; selecting with sel
(sel cars-ds :cols :speed)

(sel cars-ds :cols :dist)

(sel cars-ds :rows (range 5))

(sel cars-ds :rows 0 :cols 0)

;; sel can be useful if you need to select rows
;;  or if you need to use thread first instread of thread last


; with-data macro, lets you bind the data argument
(with-data cars-ds
  ($ :dist))

(with-data cars-ds
  [(mean ($ :speed))
   (sd   ($ :speed))])


;select multiple columns or subsets
(:column-names (get-dataset :iris))

(with-data (get-dataset :iris)
  ($ [:Sepal.Length :Sepal.Width]))

; view function creates a JTable of the data table puts in a JFrame
; :all key selects all the columns of table
(defn show-1 []
  (with-data (get-dataset :iris)
    (view ($ :all))))


; row selection
($where {:Species "setosa"}
    (get-dataset :iris))

(with-data (get-dataset :iris)
  ($where {:Species "setosa"}))

; with contraints
($where {:Petal.Width {:gt 1.0, :lt 1.5}}
 (get-dataset :iris))

; functions as constriants
($where (fn [row]
          (> (:Petal.Width row) 2))
        (get-dataset :iris))

($where (fn [row]
          (and 
            (> (:Petal.Width row) 2)
            (> (:Sepal.Length row) 7)))
        (get-dataset :iris))

; in combination, a subset becomes the new 'data'
($ [:Sepal.Length :Petal.Width]
   ($where (fn [row]
             (and 
               (> (:Petal.Width row) 2)
               (> (:Sepal.Length row) 7)))
           (get-dataset :iris)))

(with-data ($where (fn [row]
             (and 
               (> (:Petal.Width row) 2)
               (> (:Sepal.Length row) 7)))
           (get-dataset :iris))
  ($ [:Sepal.Length :Petal.Width]))


; sorting, :desc -> descending, :asc -> ascending)
(->> (get-dataset :iris)
  ($where (fn [row]
            (and 
              (> (:Petal.Width row) 2)
              (> (:Sepal.Length row) 7))))
  ($order [:Petal.Width] :asc))

; summary values, $rollup
(->> (get-dataset :iris)
  ($rollup mean :Petal.Width :Species))

(->> (get-dataset :iris)
  ($rollup sd :Sepal.Length :Species))


;;;;;; plotting data, incanter.charts
(defn show-2 []
  (with-data (get-dataset :iris)
    (let [x ($ :Sepal.Length)
          y ($ :Sepal.Width)]
      (view (scatter-plot x y)))))

(defn show-3 []
  (view (scatter-plot :Sepal.Length
                      :Sepal.Width
                      :data (get-dataset :iris))))

(defn show-4 []
(view (scatter-plot :Sepal.Length
                    :Sepal.Width
                    :data (get-dataset :iris)
                    :group-by :Species)))

(defn show-5 []
  (view (scatter-plot :Sepal.Length :Sepal.Width
                      :data (get-dataset :iris)
                      :group-by :Species
                      :title "Fisher Iris Data"
                      :x-label "Sepal Length (cm)"
                      :y-label "Sepal Width (cm)")))

;; saving a plot
(save (scatter-plot :Sepal.Length :Sepal.Width
                    :data (get-dataset :iris)
                    :group-by :Species
                    :title "Fisher Iris Data"
                    :x-label "Sepal Length (cm)"
                    :y-label "Sepal Width (cm)")
      "./iris-plot.png")

; function plot
(defn show-6 []
  (view (function-plot sin -4 4)))

(defn show-7 []
  (view (function-plot sin -10 10)))

(defn show-8 []
  (view (function-plot #(sin (* 5 %)) -4 4)))

; plotting mutliple functions
(defn show-9 []
  (view (doto (function-plot #(pow % 2) 0 10)
          (add-function #(* 1 %) 0 10))))

; Cool derivative feature, incanter.optimize
(defn show-10 []
  (let [cubic (fn [x] (+ (* x x x) (* 2 x x) (* 2 x) 3))]
    (doto (function-plot cubic -10 10)
      (add-function (derivative cubic) -10 10)
      (view))))

(defn show-11 []
  (let [quadradic (fn [x] (+ (* 2 x x) (* 2 x) 3))]
    (doto (function-plot quadradic -10 10)
      (add-function (derivative quadradic) -10 10)
      (view))))

(defn show-12 []
  (let [quadradic (fn [x] (+ (* 2 x x) (* 2 x) 3))]
    (doto (function-plot quadradic -10 10)
      (add-function (derivative quadradic) -10 10)
      (add-function (derivative (derivative quadradic)) -10 10)
      (view))))

;;;;; regression, linear model, incanter.stats
(defn show-13 []
  (view (scatter-plot :Sepal.Length :Sepal.Width
                      :data ($where {:Species "setosa"} (get-dataset :iris))
                      :group-by :Species
                      :title "Fisher Iris Data"
                      :x-label "Sepal Length (cm)"
                      :y-label "Sepal Width (cm)")))

;;linear-model, ordinary least squares regression, incanter.stats
;; "create a linear model where y = x"
;; like R's 'lm' function

;; create some data
(def x (->> (get-dataset :iris)
         ($where {:Species "setosa"})
         ($ :Petal.Length)))

(def y (->> (get-dataset :iris)
         ($where {:Species "setosa"})
         ($ :Petal.Width)))

(defn show-14 []
  (let [lm (linear-model y x)]
    (doto (scatter-plot x y)
      (view)
      (add-lines x (:fitted lm)))))
  
(defn show-15 []
  (with-data ($where {:Species "setosa"} (get-dataset :iris))
    (let [lm (linear-model ($ :Sepal.Width) ($ :Sepal.Length))]
      (doto (scatter-plot :Sepal.Length :Sepal.Width
                          :group-by :Species
                          :title "Fisher Iris Data"
                          :x-label "Sepal Length (cm)"
                          :y-label "Sepal Width (cm)")
        (add-lines :Sepal.Length (:fitted lm))
        (view)))))

;; :coefs gives the explanatory variables of the model 
(defn show-16 []
  (with-data ($where {:Species "setosa"} (get-dataset :iris))
       (let [lm (linear-model ($ :Sepal.Width) ($ :Sepal.Length))]
         (:coefs lm))))


;;Non-linear regression,
;; taken from http://data-sorcery.org/2009/06/06/fitting-non-linear-models/
(def nl-data (to-matrix (get-dataset :chwirut)))
(def x (sel nl-data :cols 1))
(def y (sel nl-data :cols 0))
(def nl-plot (scatter-plot x y :legend true))

(defn show-nl []
  (view nl-plot))

;; our data looks to be of the form:
;  y = exp(-b1*x)/(b2+b3*x) + e

; this function models this equation
(defn f [theta x]
  (let [[b1 b2 b3] theta]
    (div (exp (mult (minus b1) x)) (plus b2 (mult b3 x)))))

; need a starting point, a guess basically, if completely unsure just use 1s
(def start1 [0.1 0.01 0.02])

; the formula using our start values is not a good fit
(add-lines nl-plot x (f start1 x))

; now we fit a non-linear model, remember y = model of x's, y before x common error
(def nlm1 (non-linear-model f y x start1))

(add-lines nl-plot x (:fitted nlm1))

(defn f2 [theta x]
  (let [[b1 b3] theta]
    (div (exp (mult (minus b1) x)) (mult b3 x))))

(def nlm2 (non-linear-model f2 y x [0.1 0.02]))
(add-lines nl-plot x (:fitted nlm2))
(defn show-nl2 []
  (view nl-plot))



;;;;;; Distributions ;;;;;;;
;; https://github.com/incanter/incanter/wiki/Probability-Distributions
(sample-normal 10)
(sample-normal 10 :mean 25)

;; shape of the distributions
(defn show-17 []
  (let [x (range -3 3 0.01)]
    (doto (xy-plot x (pdf-normal x) 
                   :title "Normal PDF"
                   :y-label "Density"
                   :legend true)
      (add-lines x (pdf-normal x :sd (sqrt 0.2)))
      (add-lines x (pdf-normal x :sd (sqrt 5.0)))
      (add-lines x (pdf-normal x :mean -2 :sd (sqrt 0.5)))
      view)))

;; pdf and cdf
(defn show-18 []
  (let [x (range -3 3 0.01)]
    (doto (xy-plot x (pdf-normal x) 
                   :title "Normal PDF"
                   :y-label "Density"
                   :legend true)
      (add-lines x (cdf-normal x))
      view)))

;; histograms
(defn show-hist []
  (view (histogram (sample-normal 1000))))

(defn show-hist2 []
  (let [x (range -3 3 0.01)]
    (doto (histogram (sample-normal 1000) :density true)
      (add-lines x (pdf-normal x))
      view)))


;; statistical tests
(def treatment1
  (sample-normal 100 :mean 5))

(def treatment2
  (sample-normal 100 :mean 5))

(def t-result
  (t-test treatment1 :y treatment2))

(keys t-result)
(:p-value t-result)

;; experimenting with the t-test
(let [t1 (sample-normal 100 :mean 5)
      t2 (sample-normal 100 :mean 5.25)
      significant? (fn [res] (> (:p-value res) 0.05))]
  (significant?
    (t-test t1 :y t2)))



;;;;; Matrix operations ;;;;;
(matrix [[1 2 3]
         [2 3 4]
         [5 6 7]])
(def mat
  (matrix [[1 2 3]
           [4 5 6]
           [7 8 9]]))

(defn matrix-plot [m]
  (scatter-plot (sel m :cols 0)
                (sel m :cols 1)))

(comment 
(let [x (sample-normal 1000 :sd 0.5)
      y (sample-normal 1000 :sd 2)
      mat (bind-columns x y)]
  (doto (matrix-plot mat)
    (set-x-range -10 10)
    (set-y-range -10 10)
    (view)
    (add-lines [-10 10] [-10 10])))
)

;; a function to generate a rotation matrix
(defn rot-mat-2d [theta]
  (matrix [[(cos theta) (- (sin theta)) ]
           [(sin theta) (cos theta)]]))

;; rotate a 2d-normal distribution
(defn show-19 []
  (let [x (sample-normal 1000 :sd 0.5)
        y (sample-normal 1000 :sd 2)
        mat (bind-columns x y)]
    (doto (matrix-plot (mmult mat (rot-mat-2d (* 0.25 3.14))))
      (set-x-range -10 10)
      (set-y-range -10 10)
      (view))))

(def mat
  (let [x (sample-normal 1000 :sd 0.5)
      y (sample-normal 1000 :sd 2)]
    (mmult 
      (bind-columns x y)
      (rot-mat-2d (* 0.25 3.14159)))))

(defn show-20 []
  (doto (matrix-plot mat)
    (set-x-range -10 10)
    (set-y-range -10 10)
    (view)))



;;;;;  Correlation and covariance
(def iris-mat
  (to-matrix ($ [:not :Species] (get-dataset :iris))))

(correlation iris-mat)
(covariance iris-mat)


;;;;;;;;; PCA
;; revealing hidden structure in data
(def pca 
  (principal-components iris-mat))

;; the rotation is the set of principle components 
(def pc1 (sel (:rotation pca) :cols 0))
(def pc2 (sel (:rotation pca) :cols 1))

(def x1 (mmult iris-mat pc1))
(def x2 (mmult iris-mat pc2))

(defn show-21 []
  (view
    (scatter-plot x1 x2
                  :group-by ($ [:Species] (get-dataset :iris)))))


;; PCA is the eigenvalue decomposition of the correlation matrix
(def pca2 (decomp-eigenvalue (correlation iris-mat)))

(def ev1 (sel (:vectors pca2) :cols 0))
(def ev2 (sel (:vectors pca2) :cols 1))

(def x3 (mmult iris-mat ev1))
(def x4 (mmult iris-mat ev2))

(defn show-22 []
  (view
    (scatter-plot x3 x4
                  :group-by ($ [:Species] (get-dataset :iris)))))


;; when scale matters use the covarience matrix
(defn random-2d-normal [{:keys [sd1 sd2 theta]
                         :or {sd1 1 sd2 1 theta 0.25}}]
  (let [x (sample-normal 1000 :sd sd1)
        y (sample-normal 1000 :sd sd2)]
    (mmult 
      (bind-columns x y)
      (rot-mat-2d (* theta 3.14159)))))

(defn show-23 []
  (let [m (random-2d-normal {:sd1 0.5 :sd2 2 :theta (* 0.25 3.14)})]
    (doto (matrix-plot m)
      (set-x-range -10 10)
      (set-y-range -10 10)
      (view))))

(defn show-24 []
  (let [m (random-2d-normal {:sd1 0.5 :sd2 2 :theta (* 0.25 3.14)})
        eigen (decomp-eigenvalue (covariance m))]
    (doto (matrix-plot m)
      (set-x-range -10 10)
      (set-y-range -10 10)
      (prn eigen)
      (view))))

;; function to plot the principle component lines
(defn add-axis-n [chart evs n]
  (let [vec (sel (:vectors evs) :cols n)
        x   (first vec)
        y   (second vec)]
    (add-lines chart [0 x] [0 y])))
  
;; plot the data and add a line representing the PCA
(defn show-25 [rads]
  (let [m (random-2d-normal {:sd1 0.5 :sd2 2 :theta (* rads 3.14)})
        eigen (decomp-eigenvalue (covariance m))]
    (doto (matrix-plot m)
      (set-x-range -10 10)
      (set-y-range -10 10)
      (add-axis-n eigen 0)
      (add-axis-n eigen 1)
      (view))))






"incanter-test"