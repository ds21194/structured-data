(ns structured-data)

(defn hypotenuse [x y]
  (let [xx (* x x)
        yy (* y y)]
    (Math/sqrt (+ xx yy))))

(defn do-a-thing [x]
  (let [x2 x + x]
    (Math/pow x2 x2))
  )

(defn spiff [v]
  (if (and (get v 0) (get v 2))
    (+ (get v 0) (get v 3))
    nil)
  )

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (if (and (get v 0) (get v 2))
    (let [[x y z] v] (+ x z))
    nil)
  )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle] (Math/abs (- x2 x1))
    )
  )

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle] (Math/abs (- y2 y1)))
  )

(defn square? [rectangle]
  (= (height rectangle) (width rectangle)))

(defn area [rectangle]
  (+ (height rectangle) (width rectangle))
  )

(defn contains-point? [rectangle point]
  (let [
        [[x1 y1] [x2 y2]] rectangle
        [z1 z2] point
        ] (and (<= x1 z1 x2) (<= y1 z2 y2)))
  )

(defn contains-rectangle? [outer inner]
  (let [[p1 p2] inner]
    (and (contains-point? outer p1) (contains-point? outer p2))
    ))

(defn title-length [book]
  (count (:title book)))

(defn author-count [book]
  (count (:authors book))
  )


(defn multiple-authors? [book]
  (> (author-count book) 1))



(defn add-author [book new-author]
  (let [new-authors (conj (:authors book) new-author)]
  (assoc book :authors new-authors))
  )

(defn alive? [author]
  (not (contains? author :death-year)))

(defn element-lengths [collection]
  (map count collection)
  )

(defn second-elements [collection]
  (let [get-second (fn [x] (get x 1))]
    (map get-second collection)
    )
  )

(defn titles [books]
  (map :title books))

(defn stars [n]
  (repeat n "*")
  )

(defn monotonic? [a-seq]
  (or  (apply <= a-seq)  (apply >= a-seq)))

;TODO http://iloveponies.github.io/120-hour-epic-sax-marathon/structured-data.html Set
; TODO: exercise 20

(defn toggle [a-set elem]
  :-)

(defn contains-duplicates? [a-seq]
  :-)

(defn old-book->new-book [book]
  :-)

(defn has-author? [book author]
  :-)

(defn authors [books]
  :-)

(defn all-author-names [books]
  :-)

(defn author->string [author]
  :-)

(defn authors->string [authors]
  :-)

(defn book->string [book]
  :-)

(defn books->string [books]
  :-)

(defn books-by-author [author books]
  :-)

(defn author-by-name [name authors]
  :-)

(defn living-authors [authors]
  :-)

(defn has-a-living-author? [book]
  :-)

(defn books-by-living-authors [books]
  :-)

; %________%
