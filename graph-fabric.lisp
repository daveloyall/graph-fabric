;;;; graph-fabric.lisp

(in-package #:graph-fabric)

;;; "graph-fabric" goes here. Hacks and glory await!
(defclass cell3 ()
  ((a :initform NIL)
   (b :initform NIL)
   (c :initform NIL)
   (extra :initform ()))
  )

(setf universe ())

(setf bob (make-instance 'cell3))
(push (make-instance 'cell3) universe)

(defun remove-nth (n list)
  (delete (setf (nth n list) (gensym)) list))

(defun move-nth (n src dst)
  (let (thing (nth n src))
    ;here we put a thing in dst... dst is a place?
    ;(?? ...)

    ;and here we delete from the src.
    (delete-nth n list)
    ))


(delete-nth 4 universe)

(push bob universe)

bob

(setf  (slot-value bob 'a) (make-instance 'cell3))




universe

(defgeneric push-cell (dst src slot))

(defun random-3-slot ()
  (random-slot 3))

(defun random-slot (n)
  (nth (random n) '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))

(defmethod push-cell ((dst cell3) src slot)
  ;;; if there is something in dst, push to extra instead.
  (if (slot-value dst slot)
      (push (slot-value dst 'extra) src)
      (setf (slot-value dst slot) src)))


(defmethod push-cell :after ((cell cell3) src slot)
  (print-cell cell))

(defmethod print-cell ((cell cell3))
  (princ cell)
  (princ "(")
  (princ (slot-value cell 'a))
  (princ ",")
  (princ (slot-value cell 'b))
  (princ ",")
  (princ (slot-value cell 'c))
  (princ ",")
  (princ (slot-value cell 'extra))
  (princ ")")
  (terpri))

(push-cell (make-instance 'cell3)(make-instance 'cell3) 'a)

(print-cell bob)
