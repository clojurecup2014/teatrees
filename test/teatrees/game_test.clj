(ns teatrees.game-test
  (:require [clojure.test :refer :all]
            [teatrees.game :refer :all]))

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

;; 0 0 0 0 0 
;; 0 0 0 0 0
;; 0 1 1 0 0
;; 0 1 1 0 0
;; 0 0 0 0 0
(def square1 [{ :x 1 :y 1 }
              { :x 1 :y 2 }
              { :x 2 :y 2 }
              { :x 2 :y 2 }]) ;; Check

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
(def square3 [{ :x 3 :y 3 }
              { :x 3 :y 4 }
              { :x 4 :y 3 }
              { :x 4 :y 4 }]) ;; Check

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
    (is (can-move? empty-field square1 :left))
    (is (can-move? empty-field square1 :up))
    (is (can-move? empty-field square1 :down))
    (is (can-move? empty-field square1 :right)))
  (testing "Empty field. Should move only right and up"
    (is (not (can-move? empty-field square2 :left)))
    (is (can-move? empty-field square2 :up))
    (is (not (can-move? empty-field square2 :down)))
    (is (can-move? empty-field square2 :right)))
  (testing "Field1. Should move only down"
    (is (not (can-move? field1 arrow1 :left)))
    (is (not (can-move? field1 arrow1 :up)))
    (is (can-move? field1 arrow1 :down))
    (is (not (can-move? field1 arrow1 :right))))
  (testing "Field1. Cannot move anywhere."
    (is (not (can-move? field1 arrow2 :left)))
    (is (not (can-move? field1 arrow2 :up)))
    (is (not (can-move? field1 arrow2 :down)))
    (is (not (can-move? field1 arrow2 :right)))))














