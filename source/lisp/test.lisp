(defun hello-world ()
  (format t "hello, world!!!!!!!!!!!"))

(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

;(make-cd "Roses" "Kathy Mattea" 7 t)

(defvar *db* nil)

;添加记录
(defun add-record (cd) (push cd *db*))

;(add-record (make-cd "Roses" "Kathy Mattea" 7 t))
;(add-record (make-cd "Fly" "Dixie Chicks" 8 t))
;(add-record (make-cd "Home" "Dixie Chicks" 9 t))

;用户接口
(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a: ~10~a~%~}~%" cd)))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Aritist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p (prompt-read "Ripped [y/n]:"))))

(defun add-cds ()
  (loop  (add-record (prompt-for-cd))
     (if (not (y-or-n-p "Another? [y/n]: ")) (return))))

;保存数据库
(defun save-db (filename)
  (with-open-file (out filename
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

;查询数据库
(defun where (&key title artist rating (ripped nil ripped-p))  ;选择器生成器
  #'(lambda (cd)
      (and
       (if title (equal (getf cd :title) title) t)
       (if artist (equal (getf cd :artist) artist) t)
       (if rating (equal (getf cd :rating) rating) t)
       (if ripped-p (equal (getf cd :ripped) ripped) t))))

(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

;(select (where :artist "Dixie Chicks"))

;更新数据库
(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
	(mapcar ;将函数依次映射到列表里的每一个元素,返回一个新的列表
	 #'(lambda (row)
	     (when (funcall selector-fn row)
	       (if title (setf (getf row :title) title))
	       (if artist (setf (getf row :artist) artist))
	       (if rating (setf (getf row :rating) rating))
	       (if ripped-p (setf (getf row :ripped) ripped)))
	     row) *db*)))

;(update (where :artist "Dixie Chicks") :rating 11)

;删除记录
(defun delete-rows (selector-fn)
  (setf *db (remove-if selector-fn *db*)))






