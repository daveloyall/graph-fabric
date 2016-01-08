;;;; blitter.lisp

(in-package #:blitter)

(defun coords (x y)
  (round (* 3 (+ x (* 640 y)))))

(defun coords-cart-float (x y)
  (list (/ (- 320 x) -32) (/ (- 240 y) -32)))

(defun radius-fall (x)
  (let ((n
	 (- (expt 255 2) (* (expt x 2)850000))))
    (if (>= n 0)
	(round (sqrt n))
	0)))

(defmacro each-pixel (&rest body)
  `(with-open-file (fb0
		   "/dev/fb0" ;;;This is the linux framebuffer, basically your screen's memory.
                              ;;; If you write directly to it, what is displayed on your screen
		              ;;; changes immediately.
		   ;"~/scratch/sine5.data" ;;;Gimp can open raw bitmap files. This one is RGB
		                           ;;; data, 8 bits each.
		   :direction :IO
		   :if-does-not-exist :create
		   :if-exists :supersede
		   :element-type '(unsigned-byte 8)) ; 8 bits, we read or write a byte at a time.
     (loop for y from 0 to 479 by 1 do 
	  (loop for x from 0 to 639 by 1 do
	       
	     ;; (print "x=") (princ x)
	     ;; (print "y=") (princ y)
	     ;; (print "coords=") (princ (float-cart-coords x y))
	       (file-position fb0 (coords x y))
	       (progn ,@body)))))

(each-pixel (write-byte #x10 fb0)
	    (write-byte (radius-fall (abs (-
					   (sin (first (coords-cart-float x y)))
					   (second (coords-cart-float x y))))
				     ) fb0)
	    (write-byte #x10 fb0))

;;;an alternative radius-fall function.
(defun radius-fall (x)
  (round (* (+
	     (atan (+ 70  (*
			   (expt x 0.1)
			   -200)))
	     (/ pi 2))
	    (/ 255 pi))))


;;Here's a function to clear the screen.
;;; I like to bind a call to this to an emacs keyboard macro.
;;; The macro actually switches to the slime repl, types the call,
;;; then switches back...
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

;;Call it like this:
;(wipe #x90 #x90 #x90)



;;;testing...
;(list (radius-fall 10) (radius-fall 1) (radius-fall 0.5) (radius-fall 0.1) (radius-fall 0.00000001))
;;(0 0 166 252 255)
;;(0 0 165.9066 252.04166 255.0)


;;;I don't think these two are needed anymore!
;; (defun cart-coords (x y)
;;   (coords (+ 320 x) (+ 240 y)))

;; (defun float-cart-coords (x y)
;;   (cart-coords (round (* 320 x)) (round (* 240 y))))


;;; Todo: figure out how to write tests like this one:
;; (define-test letters-loop
;;     (let* ((letters '(:a :b :c :d))
;;            (loop-result
;; 	      (loop for letter in letters
;; 		 collect letter)))
;;       (assert-equal loop-result '(:a :b :c :d))))

;;; Alternative sine-drawing function, doesn't use radius-fall and has some different
;;; structure in the if.
;; (with-open-file (fb0 "/dev/fb0"
;;                      :direction :IO
;;                      :if-does-not-exist :create
;; 		     :if-exists :supersede
;; 		     :element-type '(unsigned-byte 8))
;;   (loop for y from 0 to 479 by 1 do 
;;        (loop for x from 0 to 639 by 1 do
	    
;; 	  ;; (print "x=") (princ x)
;; 	  ;; (print "y=") (princ y)
;; 	  ;; (print "coords=") (princ (float-cart-coords x y))
;; 	    (file-position fb0 (coords x y))
;; 	    (write-byte #x10 fb0)
;; 	    (if (and
;; 		 (>=
;; 		  (+ (second (coords-cart-float x y)) 0.05)
;; 		  (sin (first (coords-cart-float x y))))
;; 		 (<=
;; 		  (- (second (coords-cart-float x y)) 0.05)
;; 		  (sin (first (coords-cart-float x y)))))
;; 		(write-byte #xFF fb0)
;; 		(write-byte #x10 fb0))
;; 	    (write-byte #x10 fb0))))
