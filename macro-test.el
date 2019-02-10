;; -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;HELPERS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ert-bdd-describe-suite (suite)
  (message (pp suite)))

(defun ert-bdd-make-suite (description)
  (list (plist-put nil :description description)))

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
    ;; (it-nested
    (append (list (intern (concat (symbol-name (car body)) "-nested")))
            ;; "should whatever"
            (list (cadr body))
            ;; '((:describe "should whatever")
            ;;   (:describe "the thing" <before and after>))
            (list suite)
            ;; (expect t))
            (cddr body)))
   ((-contains? '(before after) (car body))
    (append (list (intern
                   (concat
                    (symbol-name (car body))
                    "-nested")))
            (list (cadr body))
            (list suite)
            (cddr body)))
   (t (describe-rebuild-body (car body) suite (append (cdr body) result)))))

(defmacro describe-nested (str suite &rest body)
  (let ((current-suite (append suite (ert-bdd-make-suite str)))
        ret)
    `((lambda (current-suite)
        ,@(dolist (item body ret)
            (setq ret (append
                       ret
                       (list (describe-rebuild-body item current-suite)))))) ',current-suite)))

(defmacro describe (str &rest body)
  (declare (indent 1))
  (let ((suite (ert-bdd-make-suite str))
        ret)
    `((lambda (current-suite)
        ,@(dolist (item body ret)
            (setq ret (append
                       ret
                       (list (describe-rebuild-body item suite)))))) ',suite)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;IT;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro it-nested (str suite &rest body)
  (let ((current-suite (append suite (ert-bdd-make-suite str))))
    `((lambda (current-suite) ,@body) ',current-suite)))

(defmacro it (str &rest body)
  (declare (indent 1))
  (error "`it' must be nested in a `describe' form"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;BEFORE/AFTER;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;BEFORE;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro before-nested (key suite &rest body)
  (let ((new-key (intern (concat ":before-" (substring (symbol-name key) 1)))))
    `(setq current-suite ',(append
                            (-drop-last 1 suite)
                            (-map (lambda (pl)
                                    (plist-put pl new-key
                                               (append (plist-get new-key pl) body)))
                                  (-take-last 1 suite))))))

(defmacro before (key &rest body)
  (error "`before' must be nested in a `describe' form"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;AFTER;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro after-nested (key suite &rest body)
  (let ((new-key (intern (concat ":after-" (substring (symbol-name key) 1)))))
    `(setq current-suite ',(append
                            (-drop-last 1 suite)
                            (-map (lambda (pl)
                                    (plist-put pl new-key
                                               (append (plist-get new-key pl) body)))
                                  (-take-last 1 suite))))))

(defmacro after (key &rest body)
  (error "`after' must be nested in a `describe' form"))

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
        (ert-bdd-describe-suite current-suite))))

  (describe "you"
    (describe "get"
      (describe "offa"
        (describe "my"
          (it "cloud"
            (ert-bdd-describe-suite current-suite)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;BEFORE/AFTER;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(describe "whatevs"
  (before :each "before")
  (after :each "after")

  (it "things"
    (ert-bdd-describe-suite current-suite)))
