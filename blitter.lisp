;;;; blitter.lisp

(in-package #:blitter)

(defconstant *fb-x* 1024)
(defconstant *fb-y* 768)
(defconstant *fb-bytes-per-pixel* 3)

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
     (loop for y from 0 to (- *fb-y* 1) by 1 do 
	  (loop for x from 0 to (- *fb-x* 1) by 1 do
	       
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
    (loop for i from 1 to (* *fb-x* *fb-y*) do
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

(defun draw-line ;( x1 y1 x2 y2 )
   (xy1 xy2)
  (let ((x1 (round (first  xy1)))
	(y1 (round (second xy1)))
	(x2 (round (first  xy2)))
	(y2 (round (second xy2))))
    
;  (declare (type rgb-pixel-buffer buffer))
  (declare (type integer x1 y1 x2 y2))
;  (declare (type rgb-pixel pixel))
  (let* ((dist-x (abs (- x1 x2)))
         (dist-y (abs (- y1 y2)))
         (steep (> dist-y dist-x)))
    (when steep
      (psetf x1 y1 y1 x1
             x2 y2 y2 x2))
    (when (> x1 x2)
      (psetf x1 x2 x2 x1
             y1 y2 y2 y1))
    (let* ((delta-x (- x2 x1))
           (delta-y (abs (- y1 y2)))
           (error (floor delta-x 2))
           (y-step (if (< y1 y2) 1 -1))
           (y y1))
      (loop 
        :for x :upfrom x1 :to x2
        :do (if steep 
                (paint x y #x00 #x00 #x00)
                (paint y x #x00 #x00 #x00))
            (setf error (- error delta-y))
            (when (< error 0)
              (incf y y-step)
              (incf error delta-x)))))))


(defun paint (x y red green blue)
  (with-open-file (fb0 "/dev/fb0"
		       :direction :IO
		       :if-does-not-exist :ERROR
		       :element-type '(unsigned-byte 8))
    (file-position fb0 (coords x y))
    (write-byte blue fb0)
    (write-byte green fb0)
    (write-byte red fb0)))


(dotimes (x 100)
  (paint x x #xFF #x00 #x00))

(defun draw-rectangle (xy1 xy2)
  (let ((x1 (first  xy1))
	(y1 (second xy1))
	(x2 (first  xy2))
	(y2 (second xy2)))
    
    (draw-line (list x1 y1) (list x1 y2))
    (draw-line (list x1 y2) (list x2 y2))
    (draw-line (list x2 y2) (list x2 y1))
    (draw-line (list x2 y1) (list x1 y1))))


(defun draw-rectangular-prism (x1 y1 z1 x2 y2 z2)
  (let ((corner1 (ortho-project x1 y1 z1))
	(corner2 (ortho-project x2 y1 z1))
	(corner3 (ortho-project x1 y2 z1))
	(corner4 (ortho-project x2 y2 z1))
	(corner5 (ortho-project x1 y1 z2))
	(corner6 (ortho-project x2 y1 z2))
	(corner7 (ortho-project x1 y2 z2))
	(corner8 (ortho-project x2 y2 z2)))
    (draw-rectangle corner1 corner4)
    (draw-rectangle corner5 corner8)
    (draw-line corner1 corner5)
    (draw-line corner2 corner6)
    (draw-line corner3 corner7)
    (draw-line corner4 corner8)))

(defun ortho-project (x y z)
  (list
   (+ x (* 0.5 z))				;x
   (+ y (* 0.5 z))))					;y




(progn (draw-rectangular-prism 10 10 10 100 100 100)
       (draw-rectangular-prism 14 14 14 96 96 96)
       (draw-rectangular-prism 18 18 18 92 92 92)
       (draw-rectangular-prism 22 22 22 88 88 88))

(draw-rectangle '(10 10) '(50 50))

(draw-line 10 10 30 50)

(font-a ())


