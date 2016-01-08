;;;; blitter.lisp

(in-package #:blitter)

(with-open-file (fb0 "/dev/fb0"
                     :direction :IO
                     :if-does-not-exist :create
		     :if-exists :supersede
		     :element-type '(unsigned-byte 8))
  (loop for y from 0 to 479 by 1 do 
       (loop for x from 0 to 639 by 1 do
	    
	  ;; (print "x=") (princ x)
	  ;; (print "y=") (princ y)
	  ;; (print "coords=") (princ (float-cart-coords x y))
	    (file-position fb0 (coords x y))
	    (write-byte #x10 fb0)
	    (if (and
		 (>=
		  (+ (second (coords-cart-float x y)) 0.05)
		  (sin (first (coords-cart-float x y))))
		 (<=
		  (- (second (coords-cart-float x y)) 0.05)
		  (sin (first (coords-cart-float x y)))))
		(write-byte #xFF fb0)
		(write-byte #x10 fb0))
	    (write-byte #x10 fb0))))

(with-open-file (fb0 "~/scratch/sine4.data"
                     :direction :IO
                     :if-does-not-exist :create
		     :if-exists :supersede
		     :element-type '(unsigned-byte 8))
  (loop for y from 0 to 479 by 1 do 
       (loop for x from 0 to 639 by 1 do
	    
	  ;; (print "x=") (princ x)
	  ;; (print "y=") (princ y)
	  ;; (print "coords=") (princ (float-cart-coords x y))
	    (file-position fb0 (coords x y))
	    (write-byte #x10 fb0)
	    (write-byte (radius-fall (abs (-
					   (sin (first (coords-cart-float x y)))
					   (second (coords-cart-float x y))))
				     ) fb0)
	    (write-byte #x10 fb0))))



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

(wipe #x90 #x90 #x90)


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

(defun radius-fall (x)
  (round (* (+
	     (atan (+ 35  (*
			   (expt x 0.8)
			   -20)))
	     (/ pi 2))
	    (/ 255 pi))))

(list (radius-fall 10)      (radius-fall 1)  (radius-fall 0.5)    (radius-fall 0.1)      (radius-fall 0.00000001))
(1 250 252 252 253)
(3 250 251 252 253)
(0 250 252 253 253)
(0 247 251 252 252)
(0 1 7 251 252)
(0 1 251 252)
(0 0 2 252)
(0 0 1 247)

(0 0 247 247)
(0 1 246 247)

(0 127 220 229)
(0 35 127 128)
(0 98 143 157)

(defun coords (x y)
  (round (* 3 (+ x (* 640 y)))))

(defun cart-coords (x y)
  (coords (+ 320 x) (+ 240 y)))

(defun float-cart-coords (x y)
  (cart-coords (round (* 320 x)) (round (* 240 y))))

(defun coords-cart-float (x y)
  (list (/ (- 320 x) -32) (/ (- 240 y) -32)))

(coords-cart-float 0 0)

(float-cart-coords 1.0 -0.8 )

;; (define-test letters-loop
;;     (let* ((letters '(:a :b :c :d))
;;            (loop-result
;; 	      (loop for letter in letters
;; 		 collect letter)))
;;       (assert-equal loop-result '(:a :b :c :d))))
