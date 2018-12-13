;; Note that answers here are given in the form [y,x] and not [x,y]. This makes the sorting easier, to find which one moves first. 
;; The order of moves is left-to-right in each row, and then top-to-bottom

(ns day13
    (:use [clojure.repl]))

(def input (->> (slurp "18d13.txt")
                (clojure.string/split-lines)))

(def current->next
    {:up [-1 0]
     :down [1 0]
     :left [0 -1]
     :right [0 1]})

(def grid
    (into {} (for [y (range (count input))
                   x (range (count (first input)))]
                [[y x] (get-in input [y x])])))

(def carts (into #{} "<>^v"))

(def carts-start (filter #(carts (val %)) grid))

(def initial-cart-dirs
    (for [[loc dir] carts-start]
        [loc ({\v :down
               \^ :up
               \> :right
               \< :left} dir)]))

(def pure-grid
    (reduce (fn [grid [loc dir]]
                (assoc grid loc
                    ({:up \| :down \| :left \- :right \-} dir)))
            grid
            initial-cart-dirs))

(defn next-loc [pure-grid [loc dir intersection-count]]
    (let [icount (mod intersection-count 3)
          lookup-map {:left {\- :left
                             \\ :up
                             \/ :down
                             \+ (condp = icount
                                    0 :down
                                    1 :left
                                    2 :up)}
                      :right {\- :right
                              \\ :down
                              \/ :up
                              \+ (condp = icount
                                    0 :up
                                    1 :right
                                    2 :down)}
                      :up {\| :up
                           \\ :left
                           \/ :right
                           \+ (condp = icount
                                 0 :left
                                 1 :up
                                 2 :right)}
                      :down {\| :down
                             \\ :right
                             \/ :left
                             \+ (condp = icount
                                 0 :right
                                 1 :down
                                 2 :left)}}
          next-loc' (mapv + loc (current->next dir))
          next-char (pure-grid next-loc')
          next-dir ((lookup-map dir) next-char)]
        
        [next-loc' next-dir (if (= \+ next-char)
                                (inc intersection-count)
                                intersection-count)]))

(def initial-cart-locs
    (for [x initial-cart-dirs]
        (conj x 0)))
				
(defn detect-collision [locs]
    (let [collide-point (->> (map first locs)
                             frequencies
                             (apply max-key second))]
        (when (> (second collide-point) 1)
            (first collide-point))))

(defn next-tick [pure-grid {:keys [locs]}]
    (let [in-tick-collision-detector 
          (reduce (fn [{:keys [collision prev-locs] :as acc} loc]
                      (let [loc' (next-loc pure-grid loc)
                            collide (or (collision (first loc)) ;; Where it is, is a collision spot
                                        (detect-collision
                                            (conj prev-locs loc')))] ;; A new collision detected
                          (if collide
                              (-> (update acc :collision conj collide)
                                  (update :new-locs (partial remove #(= collide (first %))))) ;; If a new collision is detected where another cart has already moved, it needs to be removed too.
                              (-> (update acc :new-locs conj loc') ;; Obvious
                                  (update :prev-locs (partial remove #(= loc %))) ;; As it has moved - its previous spot can't cause a collision
                                  (update :prev-locs conj loc'))))) ;; But - its new location can cause a collision now              
                  {:collision #{}
                   :prev-locs locs
                   :new-locs []}
                  (sort locs))]
      {:collision (:collision in-tick-collision-detector)
       :locs (:new-locs in-tick-collision-detector)}))
			 
 ;; Part 1
(time (first (remove #(empty? (:collision %))
                     (iterate (partial next-tick pure-grid)
                              {:collision #{} :locs initial-cart-locs}))))
					 
 ;; Part 2
 (time (first (drop-while (fn [state]
                             (> (count (:locs state)) 1))
                         (remove #(empty? (:collision %))
                                 (iterate (partial next-tick pure-grid)
                                          {:collision #{} :locs initial-cart-locs})))))
