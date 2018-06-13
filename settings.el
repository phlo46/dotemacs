;; SQL
(add-hook 'sql-login-hook 'my-sql-login-hook)

(add-hook 'sql-mode-hook
          (lambda ()
            (auto-complete-mode)))

(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)
            (setq-local show-trailing-whitespace nil)
            (auto-complete-mode t)
            (setq sql-prompt-regexp "^[_[:alpha:]]*[=][#>] ")
            (setq sql-prompt-cont-regexp "^[_[:alpha:]]*[-][#>] ")))

(eval-after-load "sql"
  '(progn
     (sql-set-product 'postgres)
     (load-library "sql-indent")))

(global-set-key (kbd "C-M-]") 'sql-indent-buffer)
(global-set-key (kbd "C-x p g") 'sql-postgres)
(global-set-key (kbd "C-c s") 'sql-set-sqli-buffer)

(defvar sql-last-prompt-pos 1
  "position of last prompt when added recording started")
(make-variable-buffer-local 'sql-last-prompt-pos)
(put 'sql-last-prompt-pos 'permanent-local t)

(defun my-sql-login-hook ()
  "Custom SQL log-in behaviours. See `sql-login-hook'."
  ;; n.b. If you are looking for a response and need to parse the
  ;; response, use `sql-redirect-value' instead of `comint-send-string'.
  (when (eq sql-product 'postgres)
    (let ((proc (get-buffer-process (current-buffer))))
      ;; Output each query before executing it. (n.b. this also avoids
      ;; the psql prompt breaking the alignment of query results.)
      (comint-send-string proc "\\set ECHO queries\n"))))


;; Javascript
(defadvice js-jsx-indent-line (after js-jsx-indent-line-after-hack activate)
  "Workaround sgml-mode and follow airbnb component style."
  (save-excursion
    (beginning-of-line)
    (if (looking-at-p "^ +\/?> *$")
        (delete-char sgml-basic-offset))))


;; ocaml
(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
  (when (and opam-share (file-directory-p opam-share))
    ;; Register Merlin
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
    (autoload 'merlin-mode "merlin" "Merlin mode" t)

    ;; ocp-indent
    (load "ocp-indent")

    ;; tuareg
    (load "tuareg-site-file")

    ;; utop
    (autoload 'utop "utop" "Toplevel for OCaml" t)
    (setq utop-command "opam config exec -- utop -emacs")
    (autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
    (add-hook 'tuareg-mode-hook 'utop-minor-mode)

    ;; Automatically start it in OCaml buffers
    (add-hook 'tuareg-mode-hook 'merlin-mode t)
    (add-hook 'caml-mode-hook 'merlin-mode t)

    ;; Use opam switch to lookup ocamlmerlin binary
    (setq merlin-ac-setup 'easy)
    (setq merlin-command 'opam)))
