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

(setq js-indent-level 8)
(setq tags-add-tables nil)
(setq ansi-color-for-comint-mode t)
(setq-default indent-tabs-mode nil)
(prefer-coding-system 'utf-8)
(tool-bar-mode -1)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq ring-bell-function 'ignore)

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
(set-register ?e '(file . "~/.emacs.d/init.el"))
(set-register ?z '(file . "~/.zshrc"))
(set-register ?o '(file . "~/Dropbox/org-mode/notebook.org"))

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
  :ensure t
  :defer t)

;; midnight
(require 'midnight)
(midnight-delay-set 'midnight-delay "6:00pm")
(setq clean-buffer-list-delay-general 1)

;; cperl
(defalias 'perl-mode 'cperl-mode)

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
  (add-hook 'persp-activated-functions #'load-eyebrowse-for-perspective)
  (add-hook 'persp-before-switch-functions #'update-eyebrowse-for-perspective)
  (add-hook 'eyebrowse-post-window-switch-hook #'save-eyebrowse-for-perspective))

;; yasnippet
(use-package yasnippet-snippets
  :ensure t
  :defer t
  :diminish yasnippet-snippets-mode)

;; exec-path-from-file mac os x
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

;; racket
(use-package racket-mode
  :ensure t
  :defer t
  :config
  (setq tab-always-indent 'complete)
  (add-to-list 'auto-mode-alist '("\\.rkt\\'" . racket-mode)))

;; ag
(use-package ag
  :ensure t
  :defer t
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
  :ensure t
  :defer t)

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
         ("C-c C-<" . mc/mark-all-like-this)))

;;Emmet mode
(use-package emmet-mode
  :ensure t
  :config
  (dolist (hook '(sgml-mode-hook css-mode-hook web-mode-hook nxml-mode-hook))
    (add-hook hook 'emmet-mode)))

;;Web-mode
(use-package web-mode
  :ensure t
  :defer t
  :config
  (dolist (l (list '("\\.phtml\\'" . web-mode)
                   '("\\.mustache\\'" . web-mode)
                   '("\\.handlebars\\'" . web-mode)
                   '("\\.hbs\\'" . web-mode)
                   '("\\.djhtml\\'" . web-mode)
                   '("\\.html?\\'" . web-mode)
                   '("\\.vue\\'" . web-mode)))
    (add-to-list 'auto-mode-alist l))
  (electric-indent-mode 1))

;; Sly IDE Common Lisp
(use-package sly
  :ensure t
  :diminish
  :defer t
  :config
  (setq inferior-lisp-program "ros -Q run"))

;; clojure cider
(use-package clj-refactor
  :ensure t
  :diminish
  :defer t
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

;; Auto-complete mode
(use-package auto-complete
  :ensure t
  :diminish
  :config
  (ac-config-default)
  (global-auto-complete-mode t)
  (dolist (m '(erlang-mode rust-mode racket-mode))
    (add-to-list 'ac-modes m)))

;; company mode
(use-package company
  :ensure t
  :config
  (dolist (b '(company-tern company-restclient))
    (add-to-list 'company-backends b))
  (dolist (h '(cider-repl-mode-hook cider-mode-hook haskell-mode-hook))
    (add-hook h #'company-mode)))

;; restclient
(use-package restclient
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode)))

(use-package company-restclient
  :ensure t
  :defer t)

;; yaml-mode
(use-package yaml-mode
  :ensure t
  :mode "\\.yml\\'")

;; elasticsearch mode
(use-package es-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.es$" . es-mode)))

;; tern-mode
(use-package tern
  :ensure t
  :defer t
  :config
  (add-to-list 'load-path "/usr/local/lib/node_modules/tern/emacs/")
  (autoload 'tern-mode "tern.el" nil t)
  (eval-after-load 'tern
    '(progn
       (require 'tern-auto-complete)
       (tern-ac-setup)))
  (add-hook 'js-mode-hook (lambda () (tern-mode t))))

(use-package company-tern
  :ensure t
  :defer t)

;; js2-mode
(use-package js2-mode
  :ensure t
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
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
  (setq elpy-rpc-backend "jedi")
  (defalias 'workon 'pyvenv-workon)
  (defalias 'workoff 'pyvenv-deactivate))

(use-package python
  :diminish
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))

;; flycheck
(use-package flycheck
  :ensure t
  :diminish
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc racket)))

;; =================================
;; ==== * END DECLARE PACKAGE * ====
;; =================================
