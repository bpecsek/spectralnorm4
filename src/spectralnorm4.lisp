;;    The Computer Language Benchmarks Game
;;    https://salsa.debian.org/benchmarksgame-team/benchmarksgame/
;;
;;    Adapted from the C (gcc) code by Sebastien Loisel
;;
;;    Contributed by Christopher Neufeld
;;    Modified by Juho Snellman 2005-10-26
;;      * Use SIMPLE-ARRAY instead of ARRAY in declarations
;;      * Use TRUNCATE instead of / for fixnum division
;;      * rearrange eval-a to make it more readable and a bit faster
;;    modified by andy

(declaim (optimize (speed 3) (safety 0) (space 0) (debug 0)))

(in-package spectralnorm4)

(deftype int16 (&optional (bits 16)) `(unsigned-byte ,bits))
(deftype d-array () '(simple-array (double-float 0d0) (*)))

(defmacro eval-a (i j)
  `(let* ((n (+ ,i ,j))
          (n+1 (1+ n)))
     (/ (float (+ (ash (* n n+1) -1) ,i 1) 0d0))))

(defun eval-At-times-u (u n Au start end)
  (declare (type int16 n start end)
           (type d-array U Au))
  (loop for i from start below end do
        (setf (aref Au i)
              (loop for j below n
                    summing (* (aref u j) (eval-A j i))
                      of-type double-float))))

(defun eval-A-times-u (u n Au start end)
  (declare (type int16 n start end)
           (type d-array u Au))
  (loop for i from start below end do
        (setf (aref Au i)
              (loop for j below n
                    summing (* (aref u j) (eval-A i j))
                      of-type double-float))))

(defparameter *thread-count* (cpus:get-number-of-processors))

#+sb-thread
(defun execute-parallel (start end function)
  (declare (optimize (speed 0)))
  (mapc #'sb-thread:join-thread
          (loop with step = (truncate (- end start) (get-thread-count))
                for index from start below end by step
                collecting (let ((start index)
                                 (end (min end (+ index step))))
                             (sb-thread:make-thread
                              (lambda () (funcall function start end)))))))

#-sb-thread
(defun execute-parallel (start end function)
  (funcall function start end))

(defun eval-AtA-times-u (u AtAu v n start end)
  (execute-parallel start end
                    (lambda (start end)
                      (eval-A-times-u u n v start end)))
  (execute-parallel start end
                    (lambda (start end)
                      (eval-At-times-u v n AtAu start end))))

(defun spectralnorm (n)
  (let ((u   (make-array (+ n 3) :element-type 'double-float :initial-element 1d0))
        (v   (make-array (+ n 3) :element-type 'double-float))
        (tmp (make-array (+ n 3) :element-type 'double-float)))
    (declare (type d-array u v tmp))
    (loop repeat 10 do
      (eval-AtA-times-u u v tmp n 0 n)
      (eval-AtA-times-u v u tmp n 0 n))
    (loop for vu of-type double-float across u
          for vi of-type double-float across v
          summing (* vu vi) into uv of-type double-float
          summing (* vi vi) into vv of-type double-float
          finally (return (sqrt (/ uv vv))))))

(defun main (&optional n-supplied)
  (let ((n (or n-supplied
               (parse-integer (or (car (cadr #+sbcl sb-ext:*posix-argv*
					                                   #+ecl ext::*command-args*)) "5500")))))
     (or (typep (* (- (* 2 n) 1) (- (* 2 n) 2)) 'fixnum)
        (error "The supplied value of 'n' breaks the optimizations in EVAL-A"))
    (format t "~11,9F~%" (spectralnorm n))))
