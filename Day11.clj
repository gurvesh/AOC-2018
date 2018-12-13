(ns day11
    (:use [clojure.repl]))

(defn loc->power [[x y] serial]
    (let [power-str (->> (+ 10 x)
                         (* y)
                         (+ serial)
                         (* (+ 10 x))
                         str)
          len-power (count power-str)
          hundreds (get power-str (- len-power 3) \0)]
        (- (Character/digit hundreds 10) 5)))
        
(defn summed-power-grid [serial]
    (reduce (fn [acc [x y]]
                (assoc acc [x y] 
                    (+ (loc->power [x y] serial)
                       (get acc [(dec x ) y] 0)
                       (get acc [x (dec y)] 0)
                       (- (get acc [(dec x) (dec y)] 0)))))
            {}
            (for [x (range 1 301)
                  y (range 1 301)]
                [x y])))

(defn region-sum [summed-table size [x y]]
    (+ (get summed-table [(dec x) (dec y)] 0)
       (get summed-table [(+ size (dec x))
                          (+ size (dec y))] 0)
       (- (get summed-table [(dec x)
                             (+ size (dec y))] 0))
       (- (get summed-table [(+ size (dec x))
                             (dec y)] 0))))
                             
(defn best [summed-table size]
    [size (reduce (fn [[best-loc max-power] loc]
                      (let [new (region-sum summed-table size loc)]
                          (if (> new max-power)
                              [loc new]
                              [best-loc max-power])))
             (for [x (range (- 301 size))
                   y (range (- 301 size))]
                 [x y]))])

;; Part 1
(time (def day11-grid' (summed-power-grid 5719)))
;; "Elapsed time: 799.034868 msecs"

(time (best day11-grid' 3))
;; "Elapsed time: 204.688922 msecs"
;; [3 [[21 34] 29]]

Part 2
(time (->> (pmap #(best day11-grid' %) (range 3 301))
           (apply max-key #(second (second %)))))

;; "Elapsed time: 6442.690661 msecs"

;; [16 [[90 244] 124]]
