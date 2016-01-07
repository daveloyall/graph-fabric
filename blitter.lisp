;;;; blitter.lisp

(in-package #:blitter)

(with-open-file (fb0 "~/scratch/sine.raw"
                     :direction :IO
                     :if-does-not-exist :create
		     :if-exists :supersede
		     :element-type '(unsigned-byte 8))
  (loop for y from -1 to 0.998 by 0.0035 do 
       (loop for x from -1 to 0.998 by 0.0035 do
	    (if (and (>= (+ y 0.001) (sin x)) (<= (- y 0.001) (sin x)))
		(progn 
		  ;; (print "x=") (princ x)
		  ;; (print "y=") (princ y)
		  ;; (print "coords=") (princ (float-cart-coords x y))
		  (file-position fb0 (float-cart-coords x y))
		  (write-byte #x10 fb0)
		  (write-byte #xFF fb0)
		  (write-byte #x10 fb0))))))

(defun wipe (r g b)
  (with-open-file (fb0 "/dev/fb0"
		       :direction :IO
		       :if-does-not-exist :ERROR
		       :element-type '(unsigned-byte 8))
    (file-position fb0 :start)
    (loop for i from 1 to (* 640 480) do
	 (write-byte b fb0)
	 (write-byte g fb0)
	 (write-byte r fb0))))

(loop for i from 1 to 10
   do
     (wipe #x00 #x00 #x00)
     (wipe #x50 #x50 #x50)
     )

(with-open-file (outfile "~/scratch/coords.txt"
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
  (loop for y from 0 to 479 do
       (loop for x from 0 to 639 do
	    (format outfile "~d from (coords ~d ~d)~%" (coords x y) x y))))

(with-open-file (outfile "~/scratch/cart-coords.txt"
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
  (loop for y from -240 to 239 do
       (loop for x from -320 to 319 do
	    (format outfile "~d from (cart-coords ~d ~d)~%" (cart-coords x y) x y))))

(with-open-file (outfile "~/scratch/float-cart-coords.txt"
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
  (loop for y from 0.98 to 0.998 by 0.0035 do
       (loop for x from 0.98 to 0.998 by 0.0035 do 
	    (format outfile "~A from (float-cart-coords ~d ~d)~%" (float-cart-coords x y) x y))))


(defun coords (x y)
  (round (* 3 (+ x (* 640 y)))))

(defun cart-coords (x y)
  (coords (+ 320 x) (+ 240 y)))

(defun float-cart-coords (x y)
  (cart-coords (round (* 320 x)) (round (* 240 y))))


(float-cart-coords 1.0 -0.8 )

;; (define-test letters-loop
;;     (let* ((letters '(:a :b :c :d))
;;            (loop-result
;; 	      (loop for letter in letters
;; 		 collect letter)))
;;       (assert-equal loop-result '(:a :b :c :d))))
