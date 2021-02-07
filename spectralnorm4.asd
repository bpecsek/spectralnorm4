(defpackage spectralnorm4-system
  (:use :common-lisp :asdf))

(in-package :spectralnorm4-system)

(defsystem "spectralnorm4"
  :version "0.1.0"
  :author "Bela Pecsek"
  :license "Public Domain"
  :description "Specectralnorm Common Lisp code for The Computer Language Benchmark Game"
  :depends-on ("cl-cpus")
  :components ((:module "src"
		:components ((:file "packages")
			     (:file "spectralnorm4" :depends-on ("packages")))))
  :in-order-to ((test-op (test-op "spectralnorm4/tests"))))

(defsystem "spectralnorm4/tests"
  :author "Bela Pecsek"
  :license "Public Domain"
  :description "Test system for spectralnorm4"
  :depends-on ("spectralnorm4"
               "rove")
  :components ((:module "tests"
                :components ((:file "main"))))
  :perform (test-op (op c) (asdf::symbol-call :rove :run c)))
