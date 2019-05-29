(ns examp.core
  (:use [clojure.repl]))

(def input (->> (slurp "/home/gurvesh/Code/clojure/examp/resources/18d15.txt")
                (clojure.string/split-lines)))

(defn loc-in [input & xs]
  (let [xs' (set xs)]
    (apply sorted-set (for [y (range (count input))
                            x (range (count input))
                            :when (xs' (get-in input [y x]))]
                        [y x]))))

(defn get-players [input-map]
  (sort (concat (input-map \E) (input-map \G))))

(defn enemies [player input-map]
  (condp = player
    \G (input-map \E)
    \E (input-map \G)))

(defn adjacent [loc]
  (map #(mapv + % loc)
       [[-1 0] [0 -1] [0 1] [1 0]]))

(def initial-map
  (into {}
        (for [x [\. \G \E]]
          [x (loc-in input x)])))

(defn get-targets [enemies input-map]
  (->> (mapcat adjacent enemies)
       (filter (input-map \.))
       sort))

(defn get-starts [loc input-map]
  (into (sorted-map)
        (for [new-loc (adjacent loc)
              :when ((input-map \.) new-loc)]
          [new-loc #{new-loc}])))

(defn next-locs [current-locs visited input-map]
  (->> (mapcat adjacent current-locs)
       (filter (input-map \.))
       (remove visited)
       (into (sorted-set))))

(defn move [loc input-map]
  (let [player (if ((input-map \G) loc) \G \E)
        enemies' (enemies player input-map)
        starting-points (get-starts loc input-map)
        visited (set (keys starting-points))
        targets (get-targets enemies' input-map)]
    (when targets
      (loop [possible-moves starting-points
             visited visited]
        (if-let [match (some visited targets)]
          (some (fn [[loc current-locs]]
                  (when (current-locs match)
                    loc))
                possible-moves)
          (let [new-visits (reduce-kv (fn [m loc current-locs]
                                        (assoc m loc
                                               (next-locs current-locs visited input-map)))
                                      (sorted-map)
                                      possible-moves)
                visited' (reduce-kv (fn [s _ new-locs]
                                      (apply conj s new-locs))
                                    visited
                                    new-visits)]
            (when (->> (vals new-visits)
                       (some #(not (empty? %))))
              (recur new-visits visited'))))))))

(defn initial-strengths [players]
  (zipmap players (repeat 200)))

(def initial-strength-map (initial-strengths (get-players initial-map)))

(defn enemy-picker [enemies strength-map]
  (->> (map (fn [loc]
              [(strength-map loc) loc])
            enemies)
       sort first second))

(defn remove-the-dead [[input-map strength-map]]
  (if-let [[loc _] (first (filter #(<= (val %) 0) strength-map))]
    (let [strength-map' (into {} (remove #(<= (val %) 0) strength-map))
          gs (disj (input-map \G) loc)
          es (disj (input-map \E) loc)
          input-map' (-> (assoc input-map \G gs)
                         (assoc \E es)
                         (update \. conj loc))]
      [input-map' strength-map'])
    [input-map strength-map]))

(defn print-board [input-map ys xs]
  (let [starting (into [] (repeat ys (into [] (repeat xs \#))))
        put-gs (reduce (fn [s loc]
                         (assoc-in s loc \G))
                       starting
                       (input-map \G))
        put-es (reduce (fn [s loc]
                         (assoc-in s loc \E))
                       put-gs
                       (input-map \E))
        put-dots (reduce (fn [s loc]
                           (assoc-in s loc \.))
                         put-es
                         (input-map \.))]
    (clojure.string/join "\n" (map (partial apply str) put-dots))))

(defn attack [loc input-map strength-map elf-strength]
  (let [player (if ((input-map \G) loc) \G \E)
        enemies' (enemies player input-map)
        adjacent-locs (set (adjacent loc))
        enemies-in-range (clojure.set/intersection enemies' adjacent-locs)]
    (when (seq enemies-in-range)
      (let [chosen-enemy (enemy-picker enemies-in-range strength-map)]
        (update strength-map chosen-enemy
                #(- % (if (= player \E) elf-strength 3)))))))

(defn player-turn [loc [input-map strength-map] elf-strength]
  (let [player (if ((input-map \G) loc) \G \E)
        enemy (if ((input-map \G) loc) \E \G)]
    (if-let [strength-map' (attack loc input-map strength-map elf-strength)]
      (remove-the-dead [input-map strength-map'])
      (if-let [loc' (move loc input-map)]
        (let [input-map' (-> (update input-map player disj loc)
                             (update player conj loc')
                             (update \. conj loc)
                             (update \. disj loc'))
              strength-map' (-> (assoc strength-map loc' (strength-map loc))
                                (dissoc loc))]
          (if-let [strength-map1 (attack' loc' input-map' strength-map' elf-strength)]
            (remove-the-dead [input-map' strength-map1])
            [input-map' strength-map']))
        [input-map strength-map]))))

(defn round [[input-map strength-map elf-strength]]
  (let [players (get-players input-map)]
    (when (and (seq (input-map \G))
               (seq (input-map \E)))
      (reduce (fn [[input-map strength-map elf-strength] loc]
                (if (strength-map loc) ;; Player is still alive
                  (conj (player-turn' loc [input-map strength-map] elf-strength)
                        elf-strength)
                  [input-map strength-map elf-strength]))
              [input-map strength-map elf-strength]
              players))))

(defn score [input-map elf-strength]
  (let [strength-map (initial-strengths (get-players input-map))]
    (loop [n 0
           [input-map strength-map elf-strength] [input-map strength-map elf-strength]]
      (if-let [[input-map' strength-map' elf-strength] (round [input-map strength-map elf-strength])]
        (recur (inc n) [input-map' strength-map' elf-strength])
        (let [elfs-standing (count (input-map \E))
              final-strengths (->> (map val strength-map)
                                   (apply +))]
          (if (= elfs-standing 10)
            [n elf-strength (* (dec n) final-strengths) (* n final-strengths)]
            [n (* (dec n) final-strengths) (* n final-strengths)]))))))
            
;; (time (score initial-map 3)) 
;; "Elapsed time: 8630.533061 msecs"
;; [106 269430 271996]  

;; (time (first (drop-while #(< (count %) 4) (pmap (partial score initial-map) (range 4 20)))))
;; "Elapsed time: 10005.349715 msecs"
;; [41 19 55160 56539]  
