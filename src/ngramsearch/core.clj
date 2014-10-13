(ns ngramsearch.core
  (:require [clojure.java.io :as io]))

(defn- file->line-seq [f]
  (let [dict (with-open [r (io/reader f)]
               (doall (line-seq r)))]
    dict))

(defn- parse-word [w]
  (clojure.string/replace (.toLowerCase w)
                  #"0|1st|1|2nd|2|3|4|5|6|7|8|9|&"
                  {"0" "zero"
                   "1st" "first"
                   "1" "one"
                   "2nd" "second"
                   "2" "two"
                   "3" "three"
                   "4" "four"
                   "5" "five"
                   "6" "six"
                   "7" "seven"
                   "8" "eight"
                   "9" "nine"
                   "&" "and"}))

(defn- word->trigrams [w]
  (map (comp keyword clojure.string/join)
       (partition 3 1 (str " "
                           (parse-word w)
                           " "))))

(defn- line-seq->trigram-map
  ([l]
     (line-seq->trigram-map l (transient {})))
  ([l m]
     (if (seq l)
       (let [word (first l)]
         (letfn [(add-to-map [trigrams m]
                   (if (seq trigrams)
                     (recur (rest trigrams)
                            (assoc! m
                                    (first trigrams)
                                    (persistent!
                                     (conj!
                                      (transient (get m (first trigrams) []))
                                      word))))
                     m))]
           (recur (rest l) (add-to-map (word->trigrams (first l)) m))))
       (persistent! m))))

(defn- line-seq->word-trigram-map
  ([l]
     (line-seq->word-trigram-map l (transient {})))
  ([l m]
     (if (seq l)
       (recur (rest l)
              (assoc! m
                      (keyword (first l))
                      (word->trigrams (first l))))
       (persistent! m))))

(defn file->trigram-map [f]
  (line-seq->trigram-map (file->line-seq f)))

(defn- transpose-trigram [t]
  (str (nth t 0)
       (nth t 2)
       (nth t 1)))

(defn- search-word->weighted-trigrams [w]
  (reduce (fn [m t]
            (let [k1 (keyword t)
                  k2 (keyword (transpose-trigram t))]
              (assoc m
                  k1 (+ 2.0 (get m k1 0.0))
                  k2 (+ 0.8 (get m k2 0.0)))))
          {}
          (map clojure.string/join
               (partition 3 1 (str " "
                                   (parse-word w)
                                   " ")))))

(defn- compute-next-row [prev-row current-element other-seq pred]
  (reduce
   (fn [row [diagonal above other-element]]
     (let [update-val
           (if (pred other-element current-element)
             diagonal
             (inc (min diagonal above (peek row))))]
       (conj row update-val)))
   [(inc (first prev-row))]
   (map vector prev-row (next prev-row) other-seq)))

(defn- levenshtein-distance [a b & {p :predicate :or {p =}}]
  (peek
   (reduce
    (fn [prev-row current-element]
      (compute-next-row prev-row current-element b p))
    (map #(identity %2) (cons nil b) (range))
    a)))

(defn- first-letter-of [w]
  (.toLowerCase (str (first w))))

(defn- init-score [search-word dict]
  (let [first-letter-of-search-word (.toLowerCase (str (first search-word)))]
    (apply (partial merge-with +)
           (map (fn [[k v]]
                  (zipmap (filter (fn [w]
                                    (= first-letter-of-search-word
                                       (first-letter-of w)))
                                  (get dict k))
                          (repeat v)))
                (search-word->weighted-trigrams search-word)))))

(defn score [search-word dict]
  (let [init-scored (init-score search-word dict)]
    (take 50
          (sort-by second >
           (map (fn [[k v]]
                  (let [levenshtein-score (levenshtein-distance search-word k)]
                    [k (/ v (if (zero? levenshtein-score) 1 levenshtein-score))]))
                init-scored)))))
