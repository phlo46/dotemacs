;;; package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;; ===========================
;; ======== * INIT * =========
;; ===========================
(use-package bind-key
  :straight t)

(use-package diminish
  :straight t)

;; Keep emacs custom-settings in a separate file
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Customizations.html
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Load my custom functions
(setq defuns-file (expand-file-name "defuns.el" user-emacs-directory))
(load defuns-file)

;; ===========================
;; ====== * END INIT * =======
;; ===========================



;; ===========================
;; ==== * SYSTEM CONFIG * ====
;; ===========================
(setq js-indent-level 8
      tags-add-tables nil
      ansi-color-for-comint-mode t
      ring-bell-function 'ignore
      ;; place auto-save file to $TMPDIR
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(setq-default indent-tabs-mode nil)

(prefer-coding-system 'utf-8)

(tool-bar-mode -1)
(menu-bar-mode -1)
(electric-pair-mode 1)
(show-paren-mode 1)

;; emacs trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; global keys
(global-set-key (kbd "C-x j") 'eval-print-last-sexp)
(global-set-key "\C-cy"'(lambda ()
                          (interactive)
                          (popup-menu 'yank-menu)))

(global-set-key (kbd "C-c C-p m") 'pp-macroexpand-last-sexp)

;; move forward/backward paragraph
(global-set-key (kbd "M-]") 'forward-paragraph)
(global-set-key (kbd "M-[") 'backward-paragraph)

;; disable keys
(global-unset-key "\C-z")
(global-unset-key "\C-x\C-z")
(global-unset-key "\C-x\C-c")

;; register files
(set-register ?i '(file . "~/.emacs.d/init.el"))
(set-register ?z '(file . "~/.zshrc"))
(set-register ?n '(file . "~/Dropbox/notes/notebook.org"))

;; Config emacs env
(setenv "PYTHONUNBUFFERED" "x")

;; expand abbreviations
(use-package abbrev
  :diminish abbrev-mode)

;; hi-lock
(use-package hi-lock
  :diminish)

;; Dired mode with extra features
(use-package dired-x
  :after dired)

;; winner-mode
(use-package winner
  :defer 5
  :config
  (winner-mode 1))

;; term
(use-package term
  :bind (:map term-raw-map
              ("C-y" . term-paste)))

;; midnight
(use-package midnight
  :config
  (midnight-delay-set 'midnight-delay "6:00pm")
  (setq clean-buffer-list-delay-general 7))

;; org-mode
(use-package org
  :config
  (setq org-log-done 'time)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((sql . t)
     (shell . t)))

  (setq org-confirm-babel-evaluate
        (lambda (lang body)
          (not (member lang (list "sql" "shell" "bash"))))))

;; makefile
(use-package makefile-mode
  :mode (("[Mm]akefile" . makefile-gmake-mode)
         ("\\.mk$" . makefile-gmake-mode)))

;; ==============================
;; ==== * END SYSTEM CONFIG * ===
;; ==============================



;; =============================
;; ==== * DECLARE PACKAGE * ====
;; =============================

;; I, EDIT HELPERS
;; ###############

;; zenburn theme
(use-package zenburn-theme
  :straight t
  :config
  (load-theme 'zenburn t))

;; undo tree
(use-package undo-tree
  :straight t
  :defer 3
  :diminish
  :config
  (global-undo-tree-mode))

;; ibuffer
(use-package ibuffer
  :straight t
  :bind ("C-x C-b" . ibuffer))

;; mode icons
(use-package mode-icons
  :straight t
  :defer 3
  :config
  (mode-icons-mode)
  (setq mode-icons-change-mode-name nil))

;; helpful
(use-package helpful
  :straight t
  :bind (("C-c C-d" . helpful-at-point)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h f" . helpful-callable)
         ("C-h C" . helpful-command)))

;; execute actions based on text patterns
(use-package wand
  :straight t
  :bind (("<M-return>" . wand:execute))
  :config
  (setq wand:*rules*
        (list (wand:create-rule :match "\\$ "
                                :capture :after
                                :action #'shell-command)
              (wand:create-rule :match "https?://"
                                :capture :whole
                                :action #'browse-url-default-browser)
              (wand:create-rule :match "/\\(Users\\|home\\)/.*"
                                :capture :whole
                                :action #'find-file))))

;; highlight thing
(use-package highlight-thing
  :straight t
  :diminish
  :config
  (global-highlight-thing-mode)
  (setq highlight-thing-case-sensitive-p t)
  (setq highlight-thing-exclude-thing-under-point t)
  (setq highlight-thing-limit-to-defun t))

;; buffer move
(use-package buffer-move
  :straight t
  :bind (("<M-s-up>" . buf-move-up)
         ("<M-s-down>" . buf-move-down)
         ("<M-s-left>" . buf-move-left)
         ("<M-s-right>" . buf-move-right)))

;; eyebrowse
(use-package eyebrowse
  :straight t
  :bind (("s-1" . eyebrowse-switch-to-window-config-1)
         ("s-2" . eyebrowse-switch-to-window-config-2)
         ("s-3" . eyebrowse-switch-to-window-config-3)
         ("s-4" . eyebrowse-switch-to-window-config-4)
         ("s-5" . eyebrowse-switch-to-window-config-5)
         ("s-6" . eyebrowse-switch-to-window-config-6)
         ("s-7" . eyebrowse-switch-to-window-config-7)
         ("s-8" . eyebrowse-switch-to-window-config-8)
         ("s-9" . eyebrowse-switch-to-window-config-9)
         ("s-\"" . eyebrowse-last-window-config))
  :config
  (eyebrowse-mode t))

;; yasnippet
(use-package yasnippet-snippets
  :straight t
  :defer 3
  :diminish yasnippet-snippets-mode
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))

;; exec-path-from-file mac os x
(use-package exec-path-from-shell
  :straight t
  :defer 3
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs (list "LANG" "GOPATH")))

;; ag
(use-package ag
  :straight t
  :config
  (setq ag-highlight-search t))

;; rgrep
(use-package deadgrep
  :straight t
  :bind (("<f5>" . deadgrep)
         ("M-s-s" . deadgrep))
  :config
  (setq deadgrep-executable "rg")
  (setq deadgrep-max-buffers 20))

;; wgrep
(use-package wgrep
  :defer t
  :config
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t))

;; dumb-jump
(use-package dumb-jump
  :straight t
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go))
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; ivy-mode
(use-package ivy
  :straight t
  :diminish
  :bind (("C-s" . swiper)
         ("C-r" . swiper)
         ("C-c C-r" . ivy-resume)
         ("M-x" . counsel-M-x)
         ("M-y" . counsel-yank-pop)
         ("C-x C-f" . counsel-find-file)
         ("C-c f" . counsel-describe-function)
         ("C-c v" . counsel-describe-variable)
         ("C-c l" . counsel-find-library)
         ("C-c i" . counsel-info-lookup-symbol)
         ("C-c u" . counsel-unicode-char)
         ("C-c k" . counsel-ag)
         ("C-c d" . counsel-descbinds)
         ("C-x l" . counsel-locate)
         :map read-expression-map
         ("C-r" . counsel-expression-history)
         :map ivy-minibuffer-map
         ("C-c o" . ivy-occur))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers nil)
  (setq enable-recursive-minibuffers t))

(use-package counsel
  :straight t)

;; avy
(use-package avy
  :straight t
  :bind (("M-g a" . avy-goto-char)
         ("M-g f" . avy-goto-char-2-below)
         ("M-g b" . avy-goto-char-2-above)
         ("C-c C-j" . avy-resume)
         ("M-g l" . avy-goto-line)
         ("M-g C-l" . avy-goto-char-in-line))
  :config
  (avy-setup-default))

;; which-key
(use-package which-key
  :straight t
  :diminish
  :bind ("C-c M-=" . which-key-show-major-mode)
  :config
  (which-key-mode))

;; psession
(use-package psession
  :straight t
  :diminish
  :config
  (psession-mode 1)
  (psession-autosave-mode 1))

;; popup window manager
(use-package popwin
  :straight t
  :bind-keymap ("C-z" . popwin:keymap)
  :config
  (popwin-mode 1))

;; projectile-mode
(use-package projectile
  :straight t
  :diminish
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (projectile-mode)
  (setq projectile-completion-system 'ivy))

;; magit
(use-package magit
  :straight t
  :bind (("C-x g" . magit-status)
         ("C-c b" . magit-blame))
  :config
  (setq magit-completing-read-function 'ivy-completing-read))

;; forge
(use-package forge
  :straight t
  :after magit)

;; ace-window
(use-package ace-window
  :straight t
  :bind ("C-." . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; multiple cursors
(use-package multiple-cursors
  :straight t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-c C-s" . mc/mark-all-symbols-like-this-in-defun)))

;; go to last change
(use-package goto-last-change
  :straight t
  :bind ("C-c s-\\" . goto-last-change))

;; expand-region
(use-package expand-region
  :straight t
  :bind (("C-= r" . er/expand-region)
         ("C-= d" . er/mark-defun)
         ("C-= s" . er/mark-symbol)
         ("C-= w" . er/mark-word)
         ("C-= '" . er/mark-inside-quotes)
         ("C-= S" . er/mark-sentence)
         ("C-= p" . er/mark-inside-pairs)
         ("C-= u" . er/mark-url)))

;; add/change/delete pairs based on expand-region
(use-package embrace
  :straight t
  :after expand-region
  :bind ("C-," . embrace-commander))

;; smart-hungry-delete
(use-package smart-hungry-delete
  :straight t
  :defer nil ;; dont defer so we can add our functions to hooks
  :bind (:map prog-mode-map
         ("<s-backspace>" . smart-hungry-delete-backward-char)
         ("C-d" . smart-hungry-delete-forward-char))
  :config
  (smart-hungry-delete-add-default-hooks))

;; paredit
(use-package paredit
  :straight t
  :diminish
  :hook ((emacs-lisp-mode eval-expression-minibuffer-setup ielm-mode
          lisp-mode lisp-interaction-mode sly-mode
          scheme-mode racket-mode
          clojure-mode cider-repl-mode
          tuareg-jbuild-mode) . enable-paredit-mode))

;; company mode
(use-package company
  :straight t
  :defer 3
  :diminish
  :config
  (global-company-mode))

(use-package company-quickhelp
  :straight t
  :after company
  :bind (:map company-active-map
              ("C-c h" . #'company-quickhelp-manual-begin))
  :config
  (company-quickhelp-mode)
  (setq company-quickhelp-delay nil))

;; flycheck
(use-package flycheck
  :straight t
  :defer 2
  :diminish
  :config
  (global-flycheck-mode)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc emacs-lisp))
  (setq flycheck-check-syntax-automatically (list 'save)))

;; ledger
(use-package ledger-mode
  :straight t
  :mode "\\.ledger\\'")

;; gitlab ci
(use-package gitlab-ci-mode
  :mode "\\.gitlab-ci.yml\\'")

(use-package gitlab-ci-mode-flycheck
  :straight t
  :after (flycheck gitlab-ci-mode)
  :init
  (gitlab-ci-mode-flycheck-enable))

;; II, PROGRAMMING MODE
;; ####################

;; emmet mode
(use-package emmet-mode
  :straight t
  :hook ((sgml-mode css-mode web-mode nxml-mode) . emmet-mode))

;; web-mode
(use-package web-mode
  :mode ("\\.phtml\\'" "\\.mustache\\'" "\\.handlebars\\'" "\\.hbs\\'"
         "\\.djhtml\\'" "\\.html?\\'" "\\.vue\\'")
  :config
  (electric-indent-mode 1))

;; haml mode
(use-package haml-mode
  :mode "\\.haml\\'")

;; dockerfile mode
(use-package dockerfile-mode
  :mode "Dockerfile.*\\'")

;; racket
(use-package racket-mode
  :mode "\\.rkt\\'"
  :config
  (setq tab-always-indent 'complete))

;; scheme geiser

(use-package scheme
  :mode ("\\.scm\\'" . scheme-mode))

(use-package geiser
  :straight t
  :after scheme
  :commands (run-geiser run-racket run-guile run-chicken)
  :hook (scheme-mode . geiser-mode)
  :config
  (setq geiser-mode-smart-tab-p t)
  (with-eval-after-load 'geiser-mode
    (define-key geiser-mode-map (kbd "C-.") nil))
  (with-eval-after-load 'geiser-repl
    (define-key geiser-repl-mode-map (kbd "C-.") nil)))

;; common Lisp
(use-package sly
  :straight t
  :defer t
  :diminish
  :bind (()
         :map sly-mode-map
         ("C-c M-p" . sly-pprint-eval-last-expression)
         ;; prevent this key-binding shadowing projectile prefix'
         ("C-c p" . nil))
  :config
  (setq sly-lisp-implementations
        '((sbcl ("sbcl" "--dynamic-space-size" "2000"))
          (ccl ("ros -L ccl-bin -Q run")))
        sly-contribs
        '(sly-fancy sly-mrepl))

  ;;; setup for looking up Hyperspec
  ;;; need to install https://www.hexstreamsoft.com/libraries/clhs/
  (let* ((ros-quicklisp-dir (expand-file-name ".roswell/lisp/quicklisp" "~/"))
         (clhs-local (expand-file-name "clhs-use-local.el" ros-quicklisp-dir)))
    (when (file-exists-p clhs-local)
      ;;; look up Hyperspec local
      (when (load clhs-local t)

        ;;; look up Hyperspec inside emacs
        (defun sly-hyperspec-lookup-eww ()
          (interactive)
          (let ((browse-url-browser-function 'eww-browse-url))
            (call-interactively sly-documentation-lookup-function)))

        (define-key sly-doc-map (kbd "l") 'sly-hyperspec-lookup-eww)))))

(use-package sly-repl-ansi-color
  :straight t
  :after sly
  :config
  (when (not (member 'sly-repl-ansi-color sly-contribs))
    (push 'sly-repl-ansi-color sly-contribs)))

(use-package sly-macrostep
  :straight t
  :after sly)

;; clojure cider
(use-package clj-refactor
  :defer t
  :diminish
  :init
  (add-hook 'clojure-mode-hook (lambda ()
                                 (clj-refactor-mode 1)
                                 (yas-minor-mode 1)
                                 (cljr-add-keybindings-with-prefix "C-c C-m"))))

;; restclient
(use-package restclient
  :mode ("\\.http\\'" . restclient-mode))

(use-package company-restclient
  :straight t
  :after (company restclient)
  :config
  (add-to-list 'company-backends 'company-restclient))

;; yaml-mode
(use-package yaml-mode
  :straight t
  :mode "\\.yml\\'")

;; terraform
(use-package terraform-mode
  :straight t
  :mode "\\.tf\\'")

;; elasticsearch mode
(use-package es-mode
  :mode "\\.es$")

;; tern-mode
(use-package tern
  :defer t
  :init
  (add-hook 'js-mode-hook (lambda () (tern-mode t)))
  :config
  (let* ((node-path (shell-command-to-string "echo $NODE_PATH"))
         (path-list (split-string node-path ":")))
    (dolist (p path-list)
      (when p
        (add-to-list 'load-path p))))

  (autoload 'tern-mode "tern/emacs/tern.el" nil t))

;; js2-mode
(use-package js2-mode
  :mode "\\.js\\'"
  :interpreter "node"
  :config
  (setq-default js2-basic-offset 4)
  (defadvice js-jsx-indent-line (after js-jsx-indent-line-after-hack activate)
    "Workaround sgml-mode and follow airbnb component style."
    (save-excursion
      (beginning-of-line)
      (if (looking-at-p "^ +\/?> *$")
          (delete-char sgml-basic-offset)))))

;; json-mode
(use-package json-mode
  :straight t
  :mode "\\.json\\'")

;; jq-mode
(use-package jq-mode
  :straight t
  :mode "\\.jq$"
  :bind (:map json-mode-map
              ("C-c C-j" . jq-interactively)))

;; markdown mode
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :bind (:map markdown-mode-map
              ("C-c R" . markdown-table-next-row))
  :config
  (setq markdown-command "multimarkdown"))

;; python
(use-package python
  :diminish
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))

(use-package elpy
  :straight t
  :after python
  :diminish
  :config
  (elpy-enable)
  (setq python-shell-interpreter "python3"
        elpy-rpc-python-command "python3"
        elpy-rpc-backend "jedi"))

;; ruby
(use-package ruby-mode
  :mode "\\.rb\\'"
  :interpreter "ruby"
  :config
  (setq ruby-insert-encoding-magic-comment nil))

(use-package ruby-tools
  :straight t
  :after ruby-mode
  :diminish ruby-tools-mode)

(use-package ruby-end
  :straight t
  :diminish ruby-end-mode
  :after ruby-mode)

;; perl
(use-package cperl-mode
  :mode "\\.\\([pP][Llm]\\|al\\)\\'"
  :interpreter "perl")

;; go
(use-package go-mode
  :straight t
  :mode "\\*\\.go")

;; C/C++
(use-package clang-format
  :defer t
  :init
  (defun my-c-hook ()
    (add-hook 'before-save-hook 'clang-format-buffer nil t))

  (add-hook 'c++-mode-hook 'my-c-hook)
  (add-hook 'c-mode-hook 'my-c-hook))

(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

;; ocaml
(use-package tuareg
  :mode ("\\.ml[ip]?\\'" . tuareg-mode)
  :config
  (let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
    (when (and opam-share (file-directory-p opam-share))
      ;; Register Merlin
      (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
      (autoload 'merlin-mode "merlin" "Merlin mode" t)
      (with-eval-after-load 'company
        (add-to-list 'company-backends 'merlin-company-backend))

      ;; Automatically start it in OCaml buffers
      (add-hook 'tuareg-mode-hook 'merlin-mode t)
      (add-hook 'caml-mode-hook 'merlin-mode t)

      ;; Use opam switch to lookup ocamlmerlin binary
      (setq merlin-command 'opam)

      ;; ocp-indent
      (load "ocp-indent")

      ;; utop
      (autoload 'utop "utop" "Toplevel for OCaml" t)
      (setq utop-command "opam config exec -- utop -emacs")
      (autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
      (add-hook 'tuareg-mode-hook 'utop-minor-mode))))

;; =================================
;; ==== * END DECLARE PACKAGE * ====
;; =================================
