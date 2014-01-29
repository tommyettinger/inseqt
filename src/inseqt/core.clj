(ns inseqt.core
  (require [clojure.core.matrix :refer :all]
           [clojure.core.matrix.operators :as mop]))

(defn unnest
  "Like clojure.core/flatten but better, stronger, faster.
  Takes any nested combination of sequential things (lists, vectors,
  etc.) and returns their contents as a single, flat, lazy sequence.
  If the argument is non-sequential (numbers, maps, strings, nil, 
  etc.), returns the original argument. Credit to steveminer.
  http://clojuredocs.org/clojure_core/clojure.core/flatten#comment_126 "
  [x]
  (letfn [(flat [coll] 
                  (lazy-seq 
                   (when-let [c (seq coll)] 
                     (let [x (first c)] 
                       (if (sequential? x) 
                         (concat (flat x) (flat (rest c))) 
                         (cons x (flat (rest c))))))))]
    (if (sequential? x) (flat x) x)))
(defn span
  [coll]
  (if (coll? coll)
    (count coll)
    0))
(defn sculpt
  "Like reshape, but it will cycle the original matrix if it isn't long enough
   to fill the requested shape (rather than throwing an error).  The matrix m
   must have at least one scalar element."
  [m sh]
  (if-let [shp (seq (remove zero? sh))]
    (reshape (take (apply * shp) (cycle (unnest [m]))) (vec shp))
    (first (reshape m [1]))))
(def each emap)
(def depth dimensionality)
(def enclose vector)
(def disclose first)

(defn interval [size]
  (if (scalar? size)
    (matrix (range size))
    (reshape (range (apply * size)) size)))
(def iota interval)
(def sum esum)

(defn bag
  "Returns a nested vector, acting like partition-all, but can take a vector or
   a width to partition.  The vector must contain at least one non-negative
   number of columns to take from the matrix and store in the result."
  ([n coll]
     (cond
       (number? n) (bag n n coll)
       (sequential? n) (loop [bagger n bags [] unbagged coll]
                         (if (seq bagger)
                           (recur (rest bagger) (conj bags (vec (take (first bagger) unbagged))) (drop (first bagger) unbagged))
                           bags))
       :else coll))
  ([n step coll]
     (vec
      (when-let [s (seq coll)]
        (let [seg (vec (take n s))]
          (cons seg (bag n step (nthrest s step))))))))

(comment
"
abs - CHECK!
bag
bins
choose
compress
concat - CHECK!
count - LIB
decode
depth - CHECK???
disclose - CHECK???
div - CHECK!
drop - LIB
each - CHECK???
enclose - CHECK???
equals - CHECK!
exp - CHECK!
find
gradeup
gradedown
inner
innermax
innermin
laminate
log
match
mod - CHECK!
minus - CHECK!
mult - CHECK!
neg
outermult
outerplus
outerdiv
outerminus
outerequals
outergte
outergt
outerlt
outerlte
outermax
outermin
pack
pi
plus
pick
pow
rake
rand
rank
ravel
raze
replicate
restructure
right
reverse
rot
shape - LIB
solve
sum
take
til
iota - CHECK!
transpose")