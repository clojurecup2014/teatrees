(ns teatrees.game-test
  (:require [clojure.test :refer :all]
            [teatrees.game :refer :all]))


(def x-max 5)
(def y-max 5)
(def z-max 11)
(def zborder 5)

(def empty-field [])
;;    0 1 2 3 4
;; 4| 0 0 0 0 1
;; 3| 0 0 1 0 0
;; 2| 0 0 1 0 0
;; 1| 0 0 0 1 0
;; 0| 0 0 0 0 0
(def field1 [{ :x 2 :y 3 }
             { :x 4 :y 4 }
             { :x 3 :y 1 }
             { :x 2 :y 2 }])

;;    0 1 2 3 4
;; 4| 0 0 0 0 1 | This is a bottom layer. Suddenly.
;; 3| 0 0 1 0 0 |
;; 2| 0 0 1 0 0 |
;; 1| 0 0 0 1 0 |
;; 0| 0 0 0 0 0 | 
(def field2 [{ :x 2 :y 3 :z 4}
             { :x 4 :y 4 :z 4}
             { :x 3 :y 1 :z 4}
             { :x 2 :y 2 :z 4}])

;; 0 0 0 0 0 
;; 0 0 0 0 0
;; 0 1 1 0 0
;; 0 1 1 0 0
;; 0 0 0 0 0
(def square1 [{ :x 1 :y 1 }
              { :x 1 :y 2 }
              { :x 2 :y 1 }
              { :x 2 :y 2 }]) ;; Check

;;    0 1 2 3 4
;; 4| 0 0 0 0 0 | This is a bottom layer. Suddenly.
;; 3| 0 0 0 0 0 |
;; 2| 0 1 0 0 0 |
;; 1| 0 1 1 0 0 |
;; 0| 0 1 0 0 0 | 
(def arrow3 [{ :x 1 :y 0 }
             { :x 1 :y 1 }
             { :x 1 :y 2 }
             { :x 2 :y 1 }])

;; 0 0 0 0 0 
;; 0 0 0 0 0
;; 1 0 0 0 0
;; 1 1 0 0 0
;; 1 0 0 0 0
(def arrow3-lm [{ :x 0 :y 0 }
                { :x 0 :y 1 }
                { :x 0 :y 2 }
                { :x 1 :y 1 }]) ;; Check

;; 0 0 0 0 0
;; 0 0 0 0 0
;; 0 0 0 0 0
;; 1 1 0 0 0
;; 1 1 0 0 0
(def square2 [{ :x 0 :y 0 }
              { :x 0 :y 1 }
              { :x 1 :y 0 }
              { :x 1 :y 1 }]) ;; Check

;; 0 0 0 1 1
;; 0 0 0 1 1
;; 0 0 0 0 0
;; 0 0 0 0 0
;; 0 0 0 0 0
(def square3 [{ :x 3 :y 3 :z 3}
              { :x 3 :y 4 :z 3}
              { :x 4 :y 3 :z 3}
              { :x 4 :y 4 :z 3}]) ;; Check

;; 0 0 0 1 1
;; 0 0 0 1 1
;; 0 0 0 0 0
;; 0 0 0 0 0
;; 0 0 0 0 0
(def square4 [{ :x 3 :y 3 :z 7}
              { :x 3 :y 4 :z 7}
              { :x 4 :y 3 :z 7}
              { :x 4 :y 4 :z 7}]) ;; Check

;;    0 1 2 3 4
;; 4| 0 0 0 0 0
;; 3| 0 0 0 0 0
;; 2| 0 1 0 0 0
;; 1| 1 1 1 0 0
;; 0| 0 0 0 0 0
(def arrow1 [{ :x 0 :y 1 }
             { :x 1 :y 1 }
             { :x 2 :y 1 }
             { :x 1 :y 2 }]) ;; Check

;;    0 1 2 3 4
;; 4| 0 0 0 1 0
;; 3| 0 0 0 1 1
;; 2| 0 0 0 1 0
;; 1| 0 0 0 0 0
;; 0| 0 0 0 0 0
(def arrow2 [{ :x 3 :y 4 }
             { :x 3 :y 3 }
             { :x 3 :y 2 }
             { :x 4 :y 3 }]) ;; Check

(deftest can-move-test
  (testing "Empty field. Should move anywhere"
    (is (can-move? :left square1 empty-field))
    (is (can-move? :up square1 empty-field))
    (is (can-move? :down square1 empty-field))
    (is (can-move? :right square1 empty-field)))
  (testing "Empty field. Should move only right and up"
    (is (not (can-move? :left square2 empty-field)))
    (is (can-move? :up square2 empty-field))
    (is (not (can-move? :down square2 empty-field)))
    (is (can-move? :right square2 empty-field)))
  (testing "Field1. Should move only down"
    (is (not (can-move? :left arrow1 field1)))
    (is (not (can-move? :up arrow1 field1)))
    (is (can-move? :down arrow1 field1))
    (is (not (can-move? :right arrow1 field1))))
  (testing "Field1. Should move left"
    (is (can-move? :left arrow3 field1)))
  (testing "Field1. Cannot move anywhere."
    (is (not (can-move? :left arrow2 field1)))
    (is (not (can-move? :up arrow2 field1)))
    (is (not (can-move? :down arrow2 field1)))
    (is (not (can-move? :right arrow2 field1))))
  (testing "Field2. Cannot move bottom."
    (is (not (can-move? :bottom square3 field2)))
    (is (can-move? :top square4 field2))
    (is (can-move? :left square3 field2))))

(deftest move-test
  (testing "x-axis move"
    (is (= arrow3-lm (:figure (move :left arrow3 field1))))))














