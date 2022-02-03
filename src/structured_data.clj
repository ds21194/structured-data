(ns structured-data)

(defn hypotenuse [x y]
  (let [xx (* x x)
        yy (* y y)]
    (Math/sqrt (+ xx yy))))

(defn do-a-thing [x]
  (let [x2 (+ x x)]
    (Math/pow x2 x2))
  )

(defn spiff [v]
  (if (and (get v 0) (get v 2))
    (+ (get v 0) (get v 2))
    nil)
  )

(defn cutify [v]
  (conj v "<3"))

(defn spiff-destructuring [v]
  (if (and (get v 0) (get v 2)),
    (let [[x y z] v] (+ x z))
    nil)
  )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 _] [x2 _]] rectangle]
    (Math/abs (- x2 x1)))
  )

(defn height [rectangle]
  (let [[[_ y1] [_ y2]] rectangle]
    (Math/abs (- y2 y1)))
  )

(defn square? [rectangle]
  (== (height rectangle) (width rectangle)))

(defn area [rectangle]
  (* (height rectangle) (width rectangle))
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
  (apply str (repeat n "*"))
  )

(defn monotonic? [a-seq]
  (or  (apply <= a-seq)  (apply >= a-seq)))

;TODO http://iloveponies.github.io/120-hour-epic-sax-marathon/structured-data.html Set
; TODO: exercise 20

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem))
  )

(defn contains-duplicates? [a-seq]
  (let [seq-len (count a-seq)
        set-len (count (set a-seq))]
    (not (== seq-len set-len)))
  )

(defn old-book->new-book [book]
  (let [s-authors (set (:authors book))]
    (assoc book :authors s-authors))
  )

(defn has-author? [book author]
  (contains? (:authors book) author)
  )

(defn authors [books]
  (let [authors (map :authors books)]
    (apply clojure.set/union authors))
  )

(defn all-author-names [books]
  (let [authors-set (authors books)]
    (set (map :name authors-set)))
  )

;(defn author->string [author]
;  (let [repr (vector (:name author))]
;    (if (contains? author :birth-year)
;      (conj repr "(" (:birth-year author) " - " ) ())
;    (if (contains? author :death-year)
;      (conj repr (:death-year author) ")") ())
;    (apply str repr)
;    )
;  )

(defn author->string [author]
  (let [years-lived (fn [author]
                      (if (:birth-year author)
                        (str " (" (:birth-year author) " - " (:death-year author) ")")))]
    (str (:name author) (years-lived author)))
  )

(defn authors->string [authors]
  (let [auth-repr (map author->string authors)]
    (apply str (interpose ", " auth-repr)))
  )

(defn book->string [book]
  (str (:title book)
       ", written by "
       (authors->string (:authors book)))
  )

(defn books->string [books]
  (let [books-len (count books)]
    (cond
      (== books-len 0) (str "No books.")
      (== books-len 1) (str "1 book. " (book->string (get books 0)) ".")
      :else (str
              books-len
              " Books. "
              (apply str (interpose ". " (map book->string books)))
              ".")
      )
    )
  )

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books)
  )

(defn author-by-name [name authors]
  (first (filter (fn [author] (= (:name author) name)) authors))
  )

(defn living-authors [authors]
  (filter alive? authors))

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book))))
  )

(defn books-by-living-authors [books]
  (filter has-a-living-author? books))

; %________%
