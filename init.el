;;; MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(package-initialize)

;; ===========================
;; ======== * INIT * =========
;; ===========================
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(let ((required-packages (list 'use-package 'zenburn-theme)))
  (dolist (p required-packages)
    (unless (package-installed-p p)
      (package-install p))))

(eval-when-compile
  (require 'use-package))

(require 'bind-key)

;; Keep emacs custom-settings in a separate file
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Customizations.html
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Load my custom functions
(setq defuns-file (expand-file-name "defuns.el" user-emacs-directory))
(load defuns-file)

;; Load my settings for packages.
(setq setting-file (expand-file-name "settings.el" user-emacs-directory))
(load setting-file)

;; ===========================
;; ====== * END INIT * =======
;; ===========================



;; ===========================
;; ==== * SYSTEM CONFIG * ====
;; ===========================

(load-theme 'zenburn t)

(eval-after-load "hi-lock"
  '(diminish 'hi-lock-mode))

(require 'dired-x)

(setq js-indent-level 8)
(setq tags-add-tables nil)
(setq ansi-color-for-comint-mode t)
(setq-default indent-tabs-mode nil)
(setq ring-bell-function 'ignore)

(prefer-coding-system 'utf-8)
(tool-bar-mode -1)
(menu-bar-mode -1)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; place auto-save file to $TMPDIR
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; emacs trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; global keys
(global-set-key (kbd "C-M-l") 'package-list-packages)
(global-set-key (kbd "C-x j") 'eval-print-last-sexp)
(global-set-key "\C-cy"'(lambda ()
                          (interactive)
                          (popup-menu 'yank-menu)))

;; org-mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)

;; move forward/backward paragraph
(global-set-key (kbd "M-]") 'forward-paragraph)
(global-set-key (kbd "M-[") 'backward-paragraph)

;; disable keys
(global-unset-key "\C-z")
(global-unset-key "\C-x\C-z")
(global-unset-key "\C-x\C-c")

;; electric-pair
(electric-pair-mode 1)

;; start-up functions
(dolist (x (list #'(lambda () (persp-mode 1))
                 #'(lambda () (yas-global-mode 1))
                 #'global-flycheck-mode
                 'global-company-mode))
  (add-hook 'after-init-hook x))

;; register files
(set-register ?i '(file . "~/.emacs.d/init.el"))
(set-register ?z '(file . "~/.zshrc"))
(set-register ?o '(file . "~/Dropbox/org-mode/notebook.org"))
(set-register ?t '(file . "~/Dropbox/org-mode/todo.org"))
(set-register ?h '(file . "~/emacs-utils/home.http"))

;; term
(add-hook 'term-mode-hook (lambda ()
                            (define-key term-raw-map (kbd "C-y") 'term-paste)))

;; Config emacs env
(setenv "PYTHONUNBUFFERED" "x")

;;Set path for Leiningen
(add-to-list 'exec-path "/usr/local/bin")
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;;highlight parentheses
(show-paren-mode 1)

;; desktop library
;;(desktop-save-mode 1)

;; org-mode
(setq org-log-done 'time)

;; cperl
(defalias 'perl-mode 'cperl-mode)

;; midnight
(require 'midnight)
(midnight-delay-set 'midnight-delay "6:00pm")
(setq clean-buffer-list-delay-general 1)

;; ==============================
;; ==== * END SYSTEM CONFIG * ===
;; ==============================



;; =============================
;; ==== * DECLARE PACKAGE * ====
;; =============================

;; diminish
(use-package diminish
  :ensure t)

;; abbrev
(use-package abbrev
  :diminish)

;; sql
(use-package sql-indent
  :ensure t)

;; undo tree
(use-package undo-tree
  :ensure t
  :diminish
  :config (global-undo-tree-mode))

;; ibuffer
(use-package ibuffer
  :ensure t
  :bind ("C-x C-b" . ibuffer))

;; mode icons
(use-package mode-icons
  :ensure t
  :config
  (mode-icons-mode)
  (setq mode-icons-change-mode-name nil))

;; highlight thing
(use-package highlight-thing
  :ensure t
  :diminish
  :config
  (global-highlight-thing-mode)
  (setq highlight-thing-case-sensitive-p t)
  (setq highlight-thing-exclude-thing-under-point t)
  (setq highlight-thing-limit-to-defun t))

;; persp-mode
(use-package persp-mode
  :ensure t
  :config
  (setq persp-auto-resume-time -1.0 persp-auto-save-opt 0)
  (setq wg-morph-on nil)
  (setq persp-autokill-buffer-on-remove 'kill-weak))

;; eyebrowse
(use-package eyebrowse
  :ensure t
  :init
  (eyebrowse-mode t)
  :config
  (define-key eyebrowse-mode-map (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
  (define-key eyebrowse-mode-map (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
  (define-key eyebrowse-mode-map (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
  (define-key eyebrowse-mode-map (kbd "M-4") 'eyebrowse-switch-to-window-config-4)

  (add-hook 'persp-activated-functions #'load-eyebrowse-for-perspective)
  (add-hook 'persp-before-switch-functions #'update-eyebrowse-for-perspective)
  (add-hook 'eyebrowse-post-window-switch-hook #'save-eyebrowse-for-perspective))

;; yasnippet
(use-package yasnippet-snippets
  :ensure t
  :diminish yasnippet-snippets-mode)

;; exec-path-from-file mac os x
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))

;; racket
(use-package racket-mode
  :ensure t
  :mode "\\.rkt\\'"
  :config
  (setq tab-always-indent 'complete))

;; ag
(use-package ag
  :ensure t
  :config
  (setq ag-highlight-search t))

;; ivy-mode
(use-package ivy
  :ensure t
  :diminish

  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers nil)
  (setq enable-recursive-minibuffers t)

  :bind (("C-s" . swiper)
         ("C-r" . swiper)
         ("C-c C-r" . ivy-resume)
         ("M-x" . counsel-M-x)
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
         ("C-c o" . ivy-occur)))

(use-package counsel
  :ensure t)

;; avy
(use-package avy
  :ensure t
  :config
  (avy-setup-default)

  :bind (("M-g f" . avy-goto-word-1-below)
         ("M-g b" . avy-goto-word-1-above)))

;; projectile-mode
(use-package projectile
  :ensure t
  :diminish
  :config
  (projectile-mode)
  (setq projectile-completion-system 'ivy))

;; magit mode
(use-package magit
  :ensure t
  :config
  (setq magit-completing-read-function 'ivy-completing-read)

  :bind (("C-x g" . magit-status)
         ("C-c b" . magit-blame)))

;; ace-window
(use-package ace-window
  :ensure t
  :bind ("C-." . ace-window))

;;Multiple-cursors
(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-c C-s" . mc/mark-all-symbols-like-this-in-defun)))

;;Emmet mode
(use-package emmet-mode
  :ensure t
  :config
  (dolist (hook '(sgml-mode-hook css-mode-hook web-mode-hook nxml-mode-hook))
    (add-hook hook 'emmet-mode)))

;;Web-mode
(use-package web-mode
  :ensure t
  :mode ("\\.phtml\\'" "\\.mustache\\'" "\\.handlebars\\'" "\\.hbs\\'"
         "\\.djhtml\\'" "\\.html?\\'" "\\.vue\\'")
  :config
  (electric-indent-mode 1))

;; Sly IDE Common Lisp
(use-package sly
  :ensure t
  :diminish
  :config
  (setq inferior-lisp-program "ros -Q run"))

;; clojure cider
(use-package clj-refactor
  :ensure t
  :diminish
  :config
  (add-hook 'clojure-mode-hook (lambda ()
                                 (clj-refactor-mode 1)
                                 (yas-minor-mode 1)
                                 (cljr-add-keybindings-with-prefix "C-c C-m"))))
;; Paredit
(use-package paredit
  :ensure t
  :diminish
  :config
  (dolist (hook '(emacs-lisp-mode-hook
                  eval-expression-minibuffer-setup-hook ielm-mode-hook
                  lisp-mode-hook lisp-interaction-mode-hook
                  scheme-mode-hook clojure-mode-hook cider-repl-mode-hook
                  racket-mode-hook tuareg-jbuild-mode-hook))
    (add-hook hook #'enable-paredit-mode)))

;; company mode
(use-package company
  :ensure t)

;; restclient
(use-package restclient
  :ensure t
  :mode ("\\.http\\'" . restclient-mode)
  :config
  (use-package company-restclient
    :ensure t
    :config
    (with-eval-after-load 'company
      (add-to-list 'company-backends 'company-restclient))))

;; yaml-mode
(use-package yaml-mode
  :ensure t
  :mode "\\.yml\\'")

;; elasticsearch mode
(use-package es-mode
  :mode "\\.es$")

;; tern-mode
(use-package tern
  :ensure t
  :config
  (use-package company-tern
    :ensure t
    :config
    (with-eval-after-load 'company
      (add-to-list 'company-backends 'company-tern)))

  (let* ((node-path (shell-command-to-string "echo $NODE_PATH"))
         (path-list (split-string node-path ":")))
    (dolist (p path-list)
      (when p
        (add-to-list 'load-path p))))
  (autoload 'tern-mode "tern/emacs/tern.el" nil t)
  (add-hook 'js-mode-hook (lambda () (tern-mode t))))

;; js2-mode
(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :interpreter "node"
  :config
  (setq-default js2-basic-offset 4))

;; markdown mode
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config (setq markdown-command "multimarkdown"))

;; python
(use-package elpy
  :ensure t
  :diminish
  :config
  (elpy-enable)
  (setq python-shell-interpreter "python3")
  (setq elpy-rpc-backend "jedi"))

(use-package python
  :diminish
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))

;; go
(use-package go-mode
  :ensure t
  :bind (:map go-mode-map
              ("C-c C-g" . godoc-at-point)
              ("M-." . godef-jump))
  :config
  (use-package go-guru :ensure t)
  (use-package company-go
    :ensure t
    :config
    (with-eval-after-load 'company
      (add-to-list 'company-backends 'company-go)))
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook #'gofmt-before-save))

;; C/C++
(use-package irony
  :ensure t
  :config
  (use-package company-irony
    :ensure t
    :config
    (with-eval-after-load 'company
      (add-to-list 'company-backends 'company-irony)))

  (use-package flycheck-irony
    :ensure t
    :config
    (with-eval-after-load 'flycheck
      (add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))

  (use-package irony-eldoc
    :ensure t
    :config
    (add-hook 'irony-mode-hook #'irony-eldoc))

  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

;; flycheck
(use-package flycheck
  :ensure t
  :diminish
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc emacs-lisp)))

;; =================================
;; ==== * END DECLARE PACKAGE * ====
;; =================================
