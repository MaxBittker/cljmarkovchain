(ns cljmarkovchain.core
  (:gen-class)
  (require [clojure.string :as str]))

(defn update-word
  "inserts a suffix to a word map entry"
  [sMap suffix]
  (assoc sMap suffix (inc (sMap suffix 0))))

(defn insert
  "inserts word to word map"
  [store [word suffix]]
  (assoc store word (update-word (store word {}) suffix)))

(defn rand-key
  [store]
  (nth (keys store) (rand-int (count (keys store)))))

(defn find-first
  "return first value that matches a predicate"
  [f coll]
  (first (filter f coll)))

(defn weighted-key
  "Given a map of keys and weights, return a key, selecting based on weights."
  [store]
  (let [weights   (reductions + (vals store))
        total   (last weights)
        choices (map vector (keys store) weights)]
      (first
        (find-first
            #(> (second %) (rand-int total))
            choices))))

(defn walk
  [chain position n acc]
  (if (zero? n)
      acc
      (recur chain
             (weighted-key (chain position (chain (rand-key chain))))
             (dec n)
             (str acc " " position))))

(defn -main
  "builds and walks markov chain"
  [& args]
  (let [tokens
        ; (str/split (slurp "./resources/input.txt") #" ")
        (re-seq #"\w+" (slurp "./resources/input.txt"))
        pairs (partition 2 1 tokens)
        chain (reduce insert {} pairs)]
    (println chain)
    (println (take 10 (repeatedly #(walk chain (rand-key chain) 15 "\n"))))))
