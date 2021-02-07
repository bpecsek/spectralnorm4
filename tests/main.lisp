(defpackage spectralnorm4/tests/main
  (:use :cl
        :spectralnorm4
        :rove))
(in-package :spectralnorm4/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :spectralnorm4)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))

(deftest test-target-2
  (testing "should (= (spectralnorm4 100) 1.2742199912349306d0) to be true"
    (ok (= (spectralnorm4:spectralnorm 100) 1.2742199912349306d0))))

