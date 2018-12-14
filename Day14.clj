(ns day14
    (:use [clojure.repl]))
    
(defn next-tick [{:keys [i j scores] :as full}]
    (let [x (+ (scores i) (scores j))
          n (count scores)
          new-scores (if (< x 10)
                         (assoc scores n x)
                         (assoc scores  n (int (/ x 10)), (inc n) (mod x 10)))
          n (if (< x 10)
                (inc n)
                (+ n 2))]
        {:i (mod (inc (+ i (new-scores i))) n)
         :j (mod (inc (+ j (new-scores j))) n)
         :scores new-scores}))
         
(def starting {:i 0, :j 1, :scores {0 3, 1 7}})

(defn get-last-x [x {:keys [scores] :as full}]
    (let [n (count scores)
          s (map #(scores %) (range (- n x 1) n))]
        (partition x 1 s)))

;; One of the 2 is the answers for the 1st part
(time (doall (->> (drop-while #(< (count (:scores %)) (+ 10 637061)) (iterate next-tick starting))
                  first
                  (get-last-x 10))))
                  
;; "Elapsed time: 1357.111632 msecs"
((3 3 1 3 8 5 1 0 1 0) (3 1 3 8 5 1 0 1 0 2))

(defn match [c [c1 c2 c3]]
    (or (= c c1)
        (= c c2)
        (= c c3)))

;; The answer for the 2nd part is either the one below, or below-1. This is because of the way the map above is constructed.
;; Not the best code, and takes about 3 mins :( but gets the job done

(time (->> (iterate next-tick starting)
           (drop-while #(not (match '(6 3 7 0 6 1) (get-last-x 6 %))))
           first
           :scores
           count
           (#(- % 6))))

;; "Elapsed time: 175296.735223 msecs"
20179082
