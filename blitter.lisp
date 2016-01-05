;;;; blitter.lisp

(in-package #:blitter)

(with-open-file (fb0 "/dev/fb0"
                     :direction :IO
                     :if-does-not-exist :ERROR
		     :element-type '(unsigned-byte 8))
  (loop for y from -1 to 1 by 0.1
     do (loop for x from -1 to 1 by 0.1
	   do (if (equal y x)
		  (progn 
		    (print "x=") (princ x)
		    (print "y=") (princ y)
		    (print "coords=") (princ (float-cart-coords x y))
		    ;(file-position fb0 (coords x y))
		    ;(write-byte #x10 fb0)
		    ;(write-byte #xFF fb0)
		    ;(write-byte #x10 fb0)
		    ))) 
       ))

(defun wipe (r g b)
  (with-open-file (fb0 "/dev/fb0"
		       :direction :IO
		       :if-does-not-exist :ERROR
		       :element-type '(unsigned-byte 8))
    (file-position fb0 :start)
    (loop for i from 1 to (* 640 480)
       do (write-byte b fb0)
	 (write-byte g fb0)
	 (write-byte r fb0))))

(loop for i from 1 to 10
   do
     (wipe #x00 #x00 #x00)
     (wipe #x50 #x50 #x50)
     )


(defun coords (x y)
  (+ (* x 3) (* 640 3 y)))

(defun cart-coords (x y)
  (coords (+ 320 x) (+ 240 y)))

(defun float-cart-coords (x y)
  (cart-coords (* 320) (* 240)))

(define-test test-cart-coords
    (let* ((letters '(:a :b :c :d))
           (loop-result
             (loop for letter in letters
                   collect letter)))
      (assert-equal loop-result '(:a :b :c :d))))
