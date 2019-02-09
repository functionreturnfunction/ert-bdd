;; -*- lexical-binding: t; -*-

(defun my-record-rebuild-body (body current-stack &optional result)
  (cond
   ((not body) result)
   ((-contains? '(my-record my-playback) (car body))
    (append (list (intern
                   (concat
                    (symbol-name (car body))
                    "-nested")))
            (list (cadr body))
            (list current-stack)
            (cddr body)))
   (t (my-record-rebuild-body (car body) (append (cdr body) result)))))

(defmacro my-record-nested (str current-stack &rest body)
  (let* ((str (append current-stack (list str)))
         ret)
    `(progn
       ,@(dolist (item body ret)
           (setq ret (append
                      ret
                      (list (my-record-rebuild-body item str))))))))

(defmacro my-record (str &rest body)
  (declare (indent 1))
  (let* ((str (list str))
         ret)
    `(progn
       ,@(dolist (item body ret)
           (setq ret (append
                      ret
                      (list (my-record-rebuild-body item str))))))))

(defmacro my-playback-nested (fn current-stack)
  `(,fn ,(string-join current-stack "*")))

(defmacro my-playback (fn)
  (ignore))

(defun my-sing (str)
  (message str))

(my-record "hey"
  (my-record "ho"
    (my-record "let's"
      (my-record "go"
        (my-playback my-sing))))

  (my-record "you"
    (my-record "get"
      (my-record "offa"
        (my-record "my"
          (my-record "cloud"
            (my-playback my-sing)))))))
