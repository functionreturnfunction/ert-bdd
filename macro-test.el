;; -*- lexical-binding: t; -*-

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
  (let* ((str (append suite (list (list :description str))))
         ret)
    `(progn
       ,@(dolist (item body ret)
           (setq ret (append
                      ret
                      (list (describe-rebuild-body item str))))))))

(defmacro describe (str &rest body)
  (declare (indent 1))
  (let ((suite (list (list :description str)))
        ret)
    `(progn
       ,@(dolist (item body ret)
           (setq ret (append
                      ret
                      (list (describe-rebuild-body item suite))))))))

(defmacro it-nested (fn suite)
  `(,fn ',suite))

(defmacro it (fn)
  (error "`it' must be nested in a `describe' form"))

(defun describe-suite (suite)
  (message (string-join (--map (plist-get it :description) suite) "*")))

(describe "hey"
  (describe "ho"
    (describe "let's"
      (describe "go"
        (it describe-suite))))

  (describe "you"
    (describe "get"
      (describe "offa"
        (describe "my"
          (describe "cloud"
            (it describe-suite)))))))
