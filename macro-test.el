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
  (let* ((current-suite (append suite (ert-bdd-make-suite str)))

         (spec-desc (string-join (-map (lambda (pl) (plist-get pl :description))
                                       current-suite)
                                 "*"))
         (spec-name (intern (replace-regexp-in-string " " "-" spec-desc))))
    `((lambda (current-spec)
        (ert-deftest ,spec-name ()
          ,spec-desc
          ,@(append
             (thread-last current-suite
               (-map (lambda (pl) (plist-get pl :before-each)))
               (-filter #'identity)
               (apply #'append))
             body
             (thread-last current-suite
               (-map (lambda (pl) (plist-get pl :after-each)))
               (-filter #'identity)
               (apply #'append)
               reverse))))
      ',current-suite)))

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
  (let ((new-key (intern (concat ":before-" (substring (symbol-name key) 1))))
        (all-but-last (-drop-last 1 suite))
        (last (car (-take-last 1 suite))))
    `(setq current-suite ',(append
                            all-but-last
                            (plist-put last new-key
                                       (append (plist-get last new-key) body))))))

(defmacro before (key &rest body)
  (error "`before' must be nested in a `describe' form"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;AFTER;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro after-nested (key suite &rest body)
  (let ((new-key (intern (concat ":after-" (substring (symbol-name key) 1))))
        (all-but-last (-drop-last 1 suite))
        (last (car (-take-last 1 suite))))
    `(setq current-suite ',(append
                            all-but-last
                            (plist-put last new-key
                                       (append body (plist-get last new-key)))))))

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
    (describe "lets"
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
  (before :each "before2")
  (after :each "after")
  (after :each "after2")

  (it "things"
    (ert-bdd-describe-suite current-suite)))
