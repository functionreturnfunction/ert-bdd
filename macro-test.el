;; -*- lexical-binding: t; -*-

(defun describe-rebuild-body (body current-stack &optional result)
  (cond
   ((not body) result)
   ((-contains? '(describe it) (car body))
    (append (list (intern
                   (concat
                    (symbol-name (car body))
                    "-nested")))
            (list (cadr body))
            (list current-stack)
            (cddr body)))
   (t (describe-rebuild-body (car body) (append (cdr body) result)))))

(defmacro describe-nested (str current-stack &rest body)
  (let* ((str (append current-stack (list str)))
         ret)
    `(progn
       ,@(dolist (item body ret)
           (setq ret (append
                      ret
                      (list (describe-rebuild-body item str))))))))

(defmacro describe (str &rest body)
  (declare (indent 1))
  (let* ((str (list str))
         ret)
    `(progn
       ,@(dolist (item body ret)
           (setq ret (append
                      ret
                      (list (describe-rebuild-body item str))))))))

(defmacro it-nested (fn current-stack)
  `(,fn ,(string-join current-stack "*")))

(defmacro it (fn)
  (ignore))

(describe "hey"
  (describe "ho"
    (describe "let's"
      (describe "go"
        (it message))))

  (describe "you"
    (describe "get"
      (describe "offa"
        (describe "my"
          (describe "cloud"
            (it message)))))))
