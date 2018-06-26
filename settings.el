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
    (with-eval-after-load 'company
      (add-to-list 'company-backends 'merlin-company-backend))

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
    (setq merlin-command 'opam)))
