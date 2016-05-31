(ns cljmarkovchain.core
  (:gen-class))

(require '[clojure.string :as str])

(defn updateWord
  "inserts a suffix to a word map entry"
  [sMap suffix]
  (assoc sMap suffix (inc (get sMap suffix 0))))

(defn insert
  "inserts word to word map"
  [set [word suffix]]
  (assoc set word (updateWord (get set word (hash-map)) suffix)))

(defn randKey
  [set]
  (nth (keys set) (rand-int (count (keys set))) "none"))

(defn find-first
  [f coll]
  (first (filter f coll)))

(defn weightedKey
  "Given a map of keys and weights, return a key, selecting based on weights."
  [set]
  (let [weights   (reductions + (vals set))
        total   (last weights)
        choices (map vector (keys set) weights)]
      (first
        (find-first
            #(> (second %) (rand-int total))
            choices))))

(defn walk
  [chain position n acc]
  (if (zero? n)
      acc
      (recur chain
             (randKey (get chain position (get chain (randKey chain))))
             (dec n)
             (str acc " " position))))

(defn -main
  "builds and walks markov chain"
  [& args]
  (let [tokens
        ; (str/split (slurp "./resources/input.txt") #" ")
        (re-seq #"\w+" (slurp "./resources/input.txt"))
        pairs (partition 2 1 tokens)
        markov (reduce insert (hash-map) pairs)]
    (println (walk markov (randKey markov) 1000 "start: "))))
