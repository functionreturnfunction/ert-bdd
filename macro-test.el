;; -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;DESCRIBE/IT;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;DESCRIBE;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun describe-rebuild-body (body suite &optional result)
  (cond
   ((not body) result)
   ((-contains? '(describe it) (car body))
    (append (list (intern
                   (concat
                    (symbol-name (car body))
                    "-nested")))
            (list (cadr body))
            (list suite)
            (cddr body)))
   (t (describe-rebuild-body (car body) suite (append (cdr body) result)))))

(defmacro describe-nested (str suite &rest body)
  (let ((current-suite (append suite (list (list :description str))))
        ret)
    `(progn
       ,@(dolist (item body ret)
           (setq ret (append
                      ret
                      (list (describe-rebuild-body item current-suite))))))))

(defmacro describe (str &rest body)
  (declare (indent 1))
  (let ((suite (list (list :description str)))
        ret)
    `(progn
       ,@(dolist (item body ret)
           (setq ret (append
                      ret
                      (list (describe-rebuild-body item suite))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;IT;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro it-nested (str suite &rest body)
  (let ((current-suite (append suite (list (list :description str)))))
    `((lambda (current-suite) ,@body) ',current-suite)))

(defmacro it (str &rest body)
  (declare (indent 1))
  (error "`it' must be nested in a `describe' form"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;BEFORE/AFTER;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;HELPERS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun describe-suite (suite)
  (message (pp suite)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TEST CODE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;DESCRIBE/IT;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(describe "hey"
  (describe "ho"
    (describe "let's"
      (it "go"
        (describe-suite current-suite))))

  (describe "you"
    (describe "get"
      (describe "offa"
        (describe "my"
          (it "cloud"
            (describe-suite current-suite)))))))
