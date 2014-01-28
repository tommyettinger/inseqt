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
  [m sh]
  (reshape (take (apply * sh) (cycle (unnest [m]))) sh))
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