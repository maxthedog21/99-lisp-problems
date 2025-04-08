(ns html-slurper.core
  (:use seesaw.core)
  (:gen-class))






;;1
(defn get-last [l]
  (loop [curList l]
    (let [restList (rest curList)]
      (if (= restList '()) (first curList) (recur restList))
      )
    )
  )


;;2
(defn my-but-last [l]
    (loop [curList l]
    (let [restList (rest (rest curList))]
      (if (= restList '()) (list (first curList) (first (rest curList))) (recur restList))
      )
    )
  )

;;3
(defn element-at [l num]
  (loop [curList l curNum num]
    (let [cur (first curList) rList (rest curList)]
      (if (= curNum 0) cur (recur rList (dec curNum)))
      )
    )
  )

;;4
(defn num-elements [list]
  (if (= list '())
    0
  (loop [curList list num 0]
    (if (= (rest curList) '()) (+ 1 num) (recur (rest curList) (inc num)) )
    )
  )
  )


;;5
(defn reverse-list [list]
  (loop [curList list reverse-list '()]
    (let [rList (rest curList)]
      (cond
        (= rList '()) (cons (first curList) reverse-list)
        :else (recur rList (cons  (first curList) reverse-list))
        )
      )
    )
  )


;;6
(defn palindrome [list]
  (= list (reverse-list list))
  )

;;7
(defn flatten-list [list]
  (if (empty? list) '()
  (let [
        rest-list (rest list)
        cur (first list)
        ]
    (cond
      (list? cur) (concat (flatten-list cur) (flatten-list rest-list))
      :else (merge  (flatten-list rest-list)  cur )
      )
    )
  )
)

;;8
(defn compress [list]

  (loop [cur-letter (first list) old-list (rest list) new-list (conj [] cur-letter )]
    (cond
      (empty? old-list) (seq new-list)
      (not= cur-letter (first old-list)) (recur (first old-list) (rest old-list) (conj new-list (first old-list)))
      :else (recur cur-letter (rest old-list) new-list)
      )
    )
  )
({:b 1} :c)

;;target num
(defn two-sum [list target]
  (loop [
         cur-list list
         seen-map {}
         index 0
         ]
    (let [cur-num (first cur-list) wanted-num (- target cur-num)]
      (cond
        (empty? cur-list) '()
        (not= (seen-map wanted-num) nil) (cons (seen-map wanted-num) (cons index '()))
        :else (recur (rest cur-list) (assoc seen-map cur-num  index) (inc index))
        )
      )
    )
  )

;;9
(defn pack-list [list]
  (loop [
         cur-letter (first list)
         old-list (rest list)
         cur-letter-list (cons cur-letter nil)
         new-list '()
         ]
    (cond
      (empty? old-list) (concat new-list (cons cur-letter-list nil))
      (not= cur-letter (first old-list)) (recur (first old-list) (rest old-list) (cons (first old-list) nil) (concat new-list (cons cur-letter-list nil)))
      :else (recur cur-letter (rest old-list) (cons cur-letter cur-letter-list) new-list)
      )
    )
  )

;;10
(defn encode [list]
  (loop [
         cur-letter (first list)
         old-list (rest list)
         count 1
         new-list '()
         ]
    (cond
      (empty? old-list) (concat new-list (cons (cons count (cons cur-letter nil)) nil ) )
      (not= cur-letter (first old-list)) (recur (first old-list) (rest old-list) 1 (concat new-list (cons (cons count (cons cur-letter nil) ) nil) ))
      :else (recur cur-letter (rest old-list) (inc count) new-list)
      )
    )
  )

;;11
(defn encode-modified [list]
  (loop [
         cur-letter (first list)
         old-list (rest list)
         count 1
         new-list '()
         ]
    (cond
      (empty? old-list) (concat
                         new-list
                         (if (> count 1)
                                (cons (cons count (cons cur-letter nil)) nil )
                                (cons cur-letter nil)
                                )
                                )
      (not= cur-letter (first old-list)) (recur (first old-list) (rest old-list) 1 (concat
                                                                                    new-list
                                                                                    (if (> count 1)
                                (cons (cons count (cons cur-letter nil)) nil )
                                (cons cur-letter nil)
                                )
                                                                                    )
                                                )
      :else (recur cur-letter (rest old-list) (inc count) new-list)
      )
    )
  )


;;12
(defn decode [list]
  (loop [
         cur-item (first list)
         old-list (rest list)
         new-list '()
         ]
    (cond
      (nil? cur-item) new-list
      (seq? cur-item) (do
                         (recur (first old-list) (rest old-list)  (concat new-list (map (fn [x] (-> cur-item rest first)) (take (first cur-item) (range))))
                                  )
                           )
      :else (recur (first old-list) (rest old-list) (concat  new-list (cons cur-item nil)))
      )
    )
  )

;;13
(defn encode-direct [list]
  (encode-direct-helper (rest list) (first list) 1)
  )

(defn encode-direct-helper [cur-list cur-letter count]
  (let [cur-item (first cur-list) rList (rest cur-list)]
    (println cur-list)
    (cond
      (empty? cur-list)                                   (if (< count 2)
                                  (cons cur-letter nil)
                                  (cons (cons count (cons cur-letter nil)) nil)
                                    )
      (not= cur-item cur-letter) (concat
                                  (if (< count 2)
                                  (cons cur-letter nil)
                                  (cons (cons count (cons cur-letter nil)) nil)
                                    )

                                  (encode-direct-helper rList cur-item 1))
      :else (encode-direct-helper rList cur-letter (+ count 1))
      )
    )
  )


;;14
(defn duplicate [list]
  (loop [cur-list list new-list '()]
    (cond
      (empty? cur-list) new-list
      :else (let [cur-item (first cur-list) rest-list (rest cur-list)] (recur rest-list (concat new-list (cons cur-item (cons cur-item nil)) ) ))
      )
    )
  )
