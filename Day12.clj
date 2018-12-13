(def input-map
    "#..#. => #
#...# => #
.##.# => #
#.... => .
..#.. => .
#.##. => .
##... => #
##.#. => #
.#.## => #
.#.#. => .
###.. => .
#..## => .
###.# => .
...## => .
#.#.. => #
..... => .
##### => #
..### => .
..#.# => #
....# => .
...#. => #
####. => #
.#... => #
#.#.# => #
.##.. => #
..##. => .
##..# => .
.#..# => #
##.## => #
.#### => .
.###. => #
#.### => .")

(def input-map (->> input-map
                    clojure.string/split-lines
                    (reduce (fn [acc s]
                                (assoc acc (subs s 0 5) (subs s 9)))
                            {})))
                            
(def input "#....#.#....#....#######..##....###.##....##.#.#.##...##.##.#...#..###....#.#...##.###.##.###...#..#")
(def input (apply str (concat (take 30 (repeat \.)) input (take 30 (repeat \.)))))

(defn generation [input]
    (->> (partition 5 1 input)
         (map #(apply str %))
         (map #(get input-map % \.))
         (#(concat ".." % "....."))
         (apply str)))
         
(apply + (keep-indexed #(when (= %2 \#) (- %1 30)) (nth (iterate generation input) 20)))

;; We start to observe glider patterns - definitely stable about Generation 200 (didn't check all gens)

(defn score [n]
    (apply + (keep-indexed #(when (= %2 \#) (- %1 30)) (nth (iterate generation input) n))))

(= (- (score 201) (score 200)
   (- (score 202) (score 201))) ;; is true, and we can get the score on each step - and see its 98
   
(+ (score 200) (* 98 (- 50000000000 200)))
