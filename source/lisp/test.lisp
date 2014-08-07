(defun hello-world ()
  (format t "hello, world!!!!!!!!!!!"))

(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

(make-cd "Roses" "Kathy Mattea" 7 t)

(defvar *db* nil)

(defun add-record (cd) (push cd *db*))

(add-record (make-cd "Roses" "Kathy Mattea" 7 t))
(add-record (make-cd "Fly" "Dixie Chicks" 8 t))
(add-record (make-cd "Home" "Dixie Chicks" 9 t))


(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a: ~10~a~%~}~%" cd)))


