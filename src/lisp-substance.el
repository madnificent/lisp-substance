(defvar substance-path nil
  "The path of the directory containing `lispin'.")

(setq substance-path "~/Unix/lisp-substance")

(add-hook 'lisp-mode-hook
          (lambda ()
            (define-key lisp-mode-map
              (kbd "C-c C-e") 'substance-eval-defun)
            (define-key lisp-mode-map
              (kbd "C-c C-x") 'substance-eval-last-sexp)))

(defun substance-lispin ()
  (if (null substance-path)
      (error "You must set substance-path")
    (concat substance-path "/bin/lispin")))

(defun substance-eval-region (begin end)
  (shell-command-on-region begin end (substance-lispin)))

(defun substance-eval-defun ()
  (interactive)
  (save-excursion
    (end-of-defun)
    (let ((end (point)))
      (beginning-of-defun)
      (substance-eval-region (point) end))))

(defun substance-eval-last-sexp ()
  (interactive)
  (let ((end (point)))
    (save-excursion
      (backward-sexp)
      (substance-eval-region (point) end))))
