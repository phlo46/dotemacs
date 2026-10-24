;;; package manager
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; ===========================
;; ======== * INIT * =========
;; ===========================
(use-package diminish
  :vc (:rev "0.47"))

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
(setq tags-add-tables nil
      ansi-color-for-comint-mode t
      ring-bell-function 'ignore
      ;; place auto-save file to $TMPDIR
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(setq-default indent-tabs-mode nil)

(electric-pair-mode 1)
(show-paren-mode 1)
(savehist-mode 1)

;; fonts
(cond
 ((eq system-type 'gnu/linux)
  (add-to-list 'default-frame-alist '(font . "JetBrains Mono 10")))
 ((eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(font . "JetBrains Mono 12"))))

;; global keys
(global-set-key (kbd "C-x j") 'eval-print-last-sexp)
(global-set-key (kbd "C-c C-p m") 'pp-macroexpand-last-sexp)
;;; move forward/backward paragraph
(global-set-key (kbd "M-]") 'forward-paragraph)
(global-set-key (kbd "M-[") 'backward-paragraph)
;;; window switch buffers
(global-set-key (kbd "C-x M-j") 'previous-buffer)
(global-set-key (kbd "C-x M-k") 'next-buffer)

;; disable keys
(global-unset-key "\C-z")
(global-unset-key "\C-x\C-z")
(global-unset-key "\C-x\C-c")

;; switch modifier keys on macOS
(when (eq system-type 'darwin)
  (add-hook 'after-init-hook
            (lambda ()
              (setq mac-command-modifier 'meta)
              (setq mac-option-modifier 'super))))

;; register files
(set-register ?i '(file . "~/.emacs.d/init.el"))
(set-register ?n '(file . "~/Dropbox/ledger/notes.ledger"))

;; expand abbreviations
(use-package abbrev
  :diminish abbrev-mode)

;; hi-lock
(use-package hi-lock
  :diminish)

;; Dired mode
(use-package dired
  :config
  (setq dired-listing-switches "-alh"))

(use-package dired-aux
  :config
  (add-to-list 'dired-compress-file-suffixes
               '("\\.mar\\'" "" "unzip -o -d %o %i")))

(use-package dired-x
  :after dired)

;; winner-mode
(use-package winner
  :defer 5
  :config
  (winner-mode 1))

;; windmove-mode
(use-package windmove
  :bind (("s-i" . windmove-up)
         ("s-k" . windmove-down)
         ("s-j" . windmove-left)
         ("s-l" . windmove-right))
  :config
  (windmove-default-keybindings 'super)
  (windmove-swap-states-default-keybindings '(meta super)))

;; shell
(use-package shell
  :hook ((shell-mode . (lambda ()
                         (setq comint-process-echoes t)
                         (setq comint-input-ring-file-name "~/.bash_history")
                         (comint-read-input-ring t)))
         (shell-mode . ansi-color-for-comint-mode-on)))

;; term
(use-package term
  :bind (:map term-raw-map
              ("C-y" . term-paste)))

;; midnight
(use-package midnight
  :config
  (midnight-mode 1)
  (midnight-delay-set 'midnight-delay "6:00pm"))

;; org-mode
(use-package org
  :bind (:map org-mode-map
              ("C-'" . nil))
  :config
  (setq org-log-done 'time)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((sql . t)
     (shell . t)))

  (setq org-confirm-babel-evaluate
        (lambda (lang body)
          (not (member lang (list "sql" "shell" "bash"))))))

;; org-roam
(use-package org-roam
  :vc (:rev "v2.3.1")
  :after org
  :custom
  (org-roam-directory "~/Documents/projects/org-roam")
  (org-roam-capture-templates
   '(("d" "default" plain "%?"
      :target (file+head "nodes/%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n")
      :unnarrowed t)))
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n g" . org-roam-graph)
   ("C-c n c" . org-roam-capture))
  :config
  (when (eq system-type 'darwin)
    (setq org-roam-graph-viewer
          (lambda (file)
            (start-process "org-roam-graph-firefox" nil
                           "open" "-a" "Firefox"
                           (expand-file-name file)))))

  (setq org-cite-global-bibliography
        (list
         (expand-file-name "bibliography/library.bib"
                           org-roam-directory)))

  (org-roam-db-autosync-mode))

;; makefile
(use-package makefile-mode
  :mode (("[Mm]akefile" . makefile-gmake-mode)
         ("\\.mk$" . makefile-gmake-mode)))

;; flymake - universal on-the-fly syntax checker
(use-package flymake
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)))

;; diff-mode
(use-package diff-mode
  :mode "\\.diff\\'")

;; which-key
(use-package which-key
  :diminish
  :bind ("C-c M-=" . which-key-show-major-mode)
  :config
  (which-key-mode))

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
  :vc (:rev "v2.11.0")
  :config
  (load-theme 'zenburn t))

;; visual undo
(use-package vundo
  :vc (:rev "v2.5.0")
  :bind ("C-x u" . vundo))

;; ibuffer
(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

;; helpful
(use-package helpful
  :vc (:rev "03756fa6ad4dcca5e0920622b1ee3f70abfc4e39")
  :bind (("C-c C-d" . helpful-at-point)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h f" . helpful-callable)
         ("C-h C" . helpful-command)))

;; vertico
(use-package vertico
  :vc (:rev "2.15")
  :init
  (vertico-mode))

;; orderless completion style
(use-package orderless
  :vc (:rev "1.8")
  :init
  (setq completion-styles '(orderless basic)
        completion-category-overrides '((file (styles basic partial-completion)))))

;; consult completing-read
(use-package consult
  :vc (:rev "3.10")
  :bind (; C-c bindings in `mode-specific-map'
         ("C-c h" . consult-history)
         ;; C-x bindings (ctl-x-map)
         ("C-x b" . consult-buffer)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ;; M-g bindings (goto-map)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ("M-g l" . consult-line)
         ("M-g L" . consult-line-multi)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)))

;; marginalia in the minibuffer
(use-package marginalia
  :vc (:rev "2.13")
  :init
  (marginalia-mode)
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)))

;; completion overlay region function
(use-package corfu
  :vc (:rev "2.16")
  :custom
  (corfu-auto t)
  :init
  (when-let* ((dir (locate-library "corfu"))
              (ext (expand-file-name "extensions" (file-name-directory dir))))
    (when (file-directory-p ext)
      (add-to-list 'load-path ext)))
  (global-corfu-mode))

;; embark: Emacs Mini-Buffer Actions Rooted in Keymaps
(use-package embark
  :vc (:rev "1.2")
  :bind (("C-'" . embark-act)
         ("C-;" . embark-dwim))
  :config
  (defun phlo/jira-ticket-short-link ()
    "Target a link at point of the Jira ticket form. e.g: VB-888

   Env var EMACS_MY_JIRA_URL needs to be set with a
   domain (without https://) such as: my-url.atlassian.net"
    (save-excursion
      (let* ((beg (progn (skip-chars-backward "[:alnum:]-") (point)))
             (end (progn (skip-chars-forward "[:alnum:]-") (point)))
             (str (buffer-substring-no-properties beg end)))
        (save-match-data
          (when (string-match "[[:upper:]]+-[[:digit:]]+" str)
            `(url
              ,(format "https://%s/browse/%s"
                       (getenv "EMACS_MY_JIRA_URL")
                       (match-string 0 str))
              ,beg . ,end))))))

  (add-to-list 'embark-target-finders 'phlo/jira-ticket-short-link))

(use-package embark-consult
  :vc (:rev "1.2")
  :after (embark consult))

;; highlight thing
(use-package highlight-thing
  :vc (:rev "fd0309e72727e5332be3397e15edb035b426dd0b")
  :diminish
  :config
  (global-highlight-thing-mode)
  (setq highlight-thing-case-sensitive-p t)
  (setq highlight-thing-exclude-thing-under-point t)
  (setq highlight-thing-limit-to-defun t))

;; eyebrowse
(use-package eyebrowse
  :vc (:rev "473381f4f9e847eb50a40ef2306c027432789754")
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

(use-package eyebrowse-restore
  :vc (:rev "abb3877e12b41740305741deec37ca681b896e82")
  :after eyebrowse
  :config
  (eyebrowse-restore-mode))

;; yasnippet
(use-package yasnippet-snippets
  :vc (:rev "1.1")
  :defer 3
  :diminish yasnippet-snippets-mode
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))

;; vterm
(use-package vterm
  :vc (:rev "6d715a93fa0e5182bc137d4db09f376e06938aa5")
  :bind (:map vterm-mode-map
              ("M-p" . vterm-send-up)
              ("M-n" . vterm-send-down)))

;; ghostel
(use-package ghostel
  :vc (:rev "v0.58.0")
  :bind (:map ghostel-semi-char-mode-map
              ("C-s"  . consult-line)
              ("C-k"  . phlo/ghostel-send-C-k-and-kill)
              ("M-p" . (lambda () (interactive) (ghostel-send-key "p" "ctrl")))
              ("M-n" . (lambda () (interactive) (ghostel-send-key "n" "ctrl"))))
  :config
  (defun phlo/ghostel-send-C-k-and-kill ()
    "Send `C-k' to ghostel. Like normal Emacs `C-k'.  Kill to end of line and put content in kill-ring."
    (interactive)
    (kill-ring-save (point) (line-end-position))
    (ghostel-send-key "k" "ctrl")))

;; shell-pop
(use-package shell-pop
  :vc (:rev "0.70")
  :bind ("C-s-t" . shell-pop))

;; exec-path-from-file mac os x
(use-package exec-path-from-shell
  :vc (:rev "2.3")
  :defer 3
  :config
  (dolist (var '("LANG" "GOPATH" "SSH_AGENT_PID" "SSH_AUTH_SOCK"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

;; rgrep
(use-package deadgrep
  :vc (:rev "0.13")
  :bind (("<f5>" . deadgrep)
         ("M-s-s" . deadgrep))
  :config
  (setq deadgrep-executable "rg")
  (setq deadgrep-max-buffers 20))

;; avy
(use-package avy
  :vc (:rev "0.5.0")
  :bind (("M-g a" . avy-goto-char)
         ("M-g f" . avy-goto-char-2-below)
         ("M-g b" . avy-goto-char-2-above)
         ("C-c C-j" . avy-resume)
         ("M-g M-l" . avy-goto-line)
         ("M-g C-l" . avy-goto-char-in-line))
  :config
  (avy-setup-default))

;; ace-link
(use-package ace-link
  :vc (:rev "d9bd4a25a02bdfde4ea56247daf3a9ff15632ea4")
  :after avy
  :config
  (ace-link-setup-default))

;; psession
(use-package psession
  :vc (:rev "ebe81c18f94bb74e0b795bbe6d4fa9284ccbbf5e")
  :diminish
  :config
  (psession-mode 1)
  (psession-autosave-mode 1))

;; magit
(use-package magit
  :vc (:rev "v4.7.1")
  :bind (("C-x g" . magit-status)
         ("C-c b" . magit-blame)))

;; forge
(use-package forge
  :vc (:rev "v0.6.9")
  :after magit)

;; multiple cursors
(use-package multiple-cursors
  :vc (:rev "94b8b07a4bab87f803123723b68227565429dfa1")
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

;; go to last change
(use-package goto-last-change
  :vc (:rev "v1.2.1")
  :bind ("C-c s-\\" . goto-last-change))

;; expand-region
(use-package expand-region
  :vc (:rev "351279272330cae6cecea941b0033a8dd8bcc4e8")
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
  :vc (:rev "0.1.4")
  :after expand-region
  :bind ("C-," . embrace-commander))

;; smart-hungry-delete
(use-package smart-hungry-delete
  :vc (:rev "e06525cc1841805ebe470c876d6b966de90bc275")
  :init
  (smart-hungry-delete-add-default-hooks)
  :bind (:map prog-mode-map
         ("<s-backspace>" . smart-hungry-delete-backward-char)
         ("C-d" . smart-hungry-delete-forward-char)))

;; cleanup whitespace on only touched lines
(use-package ws-butler
  :vc (:url "https://github.com/lewang/ws-butler.git"
       :rev "v1.3")
  :diminish
  :hook (prog-mode . ws-butler-mode))

;; paredit
(use-package paredit
  :vc (:rev "v26")
  :diminish
  :hook ((emacs-lisp-mode eval-expression-minibuffer-setup ielm-mode
          lisp-mode lisp-interaction-mode sly-mode
          scheme-mode racket-mode
          clojure-mode cider-repl-mode
          tuareg-jbuild-mode) . enable-paredit-mode))

;; ledger
(use-package ledger-mode
  :vc (:rev "9539ff67a9ac491be4b382530bdbba9558379700")
  :mode "\\.ledger\\'")

;; gitlab ci
(use-package gitlab-ci-mode
  :mode "\\.gitlab-ci.yml\\'")

;; jenkins
(use-package jenkinsfile-mode
  :mode (("Jenkinsfile\\(?:[._].*\\)?\\'" . jenkinsfile-mode)))

;; browse-at-remote
(use-package browse-at-remote
  :vc (:rev "38e5ffd77493c17c821fd88f938dbf42705a5158")
  :defer t)

;; gptel
(use-package gptel
  :vc (:rev "v0.9.9.6")
  :hook ((gptel-mode . visual-line-mode))
  :bind (("C-c s-r" . gptel-rewrite)
         ("C-c s-g" . gptel-menu))
  :config
  (gptel-make-openai "ChatGPT" :key #'gptel-api-key-from-auth-source :stream t)
  (gptel-make-gemini "Gemini" :key #'gptel-api-key-from-auth-source :stream t)
  (gptel-make-anthropic "Claude" :key #'gptel-api-key-from-auth-source :stream t))

;; agent-shell
(use-package agent-shell
  :vc (:rev "v0.89.1")
  :bind (("C-c a r" . agent-shell-send-region)
         ("C-c a R" . agent-shell-send-region-to)
         ("C-c a f" . agent-shell-send-file)
         ("C-c a d" . agent-shell-send-dwim)
         ("C-c a b" . agent-shell-switch-buffer))
  :config
  (add-to-list 'display-buffer-alist
               '("\\*agent-shell-diff\\*"
                 (display-buffer-reuse-window
                  display-buffer-in-direction)
                 (direction . below)
                 (dedicated . t)
                 (inhibit-same-window . t))))


;; II, PROGRAMMING MODE
;; ####################

;; dockerfile mode
(use-package dockerfile-mode
  :vc (:rev "97733ce074b1252c1270fd5e8a53d178b66668ed")
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
  :vc (:rev "9b3477a5dc6c9a4496d4fbb6804349961e861835")
  :defer t
  :diminish
  :bind (:map sly-mode-map
         ("C-c M-p" . sly-pprint-eval-last-expression))
  :hook (sly-mrepl-mode . (lambda ()
                            (advice-add 'paredit-RET
                                        :after
                                        (lambda ()
                                          (when (string-prefix-p "*sly-mrepl for"
                                                                 (buffer-name (current-buffer)))
                                            (sly-mrepl-return))))))
  :config
  (setq sly-lisp-implementations
        '((sbcl ("sbcl") :coding-system utf-8-unix))
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
        (defun phlo/sly-hyperspec-lookup-eww ()
          (interactive)
          (let ((browse-url-browser-function 'eww-browse-url))
            (call-interactively sly-documentation-lookup-function)))

        (define-key sly-doc-map (kbd "l") 'phlo/sly-hyperspec-lookup-eww)))))

(use-package sly-repl-ansi-color
  :after sly
  :config
  (add-to-list 'sly-contribs 'sly-repl-ansi-color))

(use-package sly-macrostep
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

;; kotlin
(use-package kotlin-ts-mode
  :vc (:rev "292c9a05ff2243c39a48201b277840b1d25e1ea0")
  :mode "\\.kts?\\'")

;; yaml-mode
(use-package yaml-mode
  :vc (:rev "93dba98c050e9abfc623ec66aa499dbbb46b2fe1")
  :mode "\\.yml\\'")

;; terraform
(use-package terraform-mode
  :vc (:rev "1.1.0")
  :mode "\\.tf\\'")

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

;; ruby
(use-package ruby-mode
  :mode "\\.rb\\'"
  :interpreter "ruby"
  :config
  (setq ruby-insert-encoding-magic-comment nil))

;; perl
(use-package cperl-mode
  :mode "\\.\\([pP][Llm]\\|al\\)\\'"
  :interpreter "perl")

;; go
(use-package go-mode
  :vc (:rev "8aaaa9d2574d7862ecbbe1ff369e88fe3796c8be")
  :mode "\\*\\.go")

;; C/C++
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

;; III, EXTERNAL SYSTEMS
;; #####################

;; jira
(use-package jira-issues
  :vc (jira :rev "v2.23.0")
  :commands (jira-issues)
  :config
  (setq jira-base-url
        (if-let* ((host (getenv "EMACS_MY_JIRA_URL")))
            (concat "https://" host)
          (display-warning 'jira "EMACS_MY_JIRA_URL is not set; Jira URLs will not work")
          ""))
  (setq jira-token-is-personal-access-token nil
        jira-api-version 3
        jira-issues-table-fields
        '(:key :issue-type-name :status-name :assignee-name :summary))

  ;; One window for all issue details: reuse whichever window already shows a
  ;; jira-detail-mode buffer instead of splitting.
  (add-to-list 'display-buffer-alist
               '("\\`\\*Jira Issue Detail: "
                 (display-buffer-reuse-window
                  display-buffer-reuse-mode-window
                  display-buffer-in-direction)
                 (mode . jira-detail-mode)
                 (direction . below)
                 (window-height . fit-window-to-buffer)))

  ;; Dedicated window for edit/comment buffers: killing the buffer on submit
  ;; then deletes it, instead of leaving a second window on the detail buffer.
  (add-to-list 'display-buffer-alist
               '("\\`\\*Jira \\(?:Description\\|Comment\\)"
                 (display-buffer-reuse-window
                  display-buffer-in-direction)
                 (direction . below)
                 (dedicated . t)))

  ;; Summary in the buffer name, so completion matches keywords, not just the key
  (advice-add 'jira-detail--get-issue-buffer :around
              (lambda (orig key)
                (let ((prefix (concat "*Jira Issue Detail: [" key "]")))
                  (or (seq-find (lambda (b) (string-prefix-p prefix (buffer-name b)))
                                (buffer-list))
                      (funcall orig key)))))

  (advice-add 'jira-detail--issue :after
              (lambda (key issue)
                (when-let* ((summary (alist-get 'summary (alist-get 'fields issue))))
                  (with-current-buffer (jira-detail--get-issue-buffer key)
                    (rename-buffer
                     (format "*Jira Issue Detail: [%s] %s*" key summary) t)))))

  ;; jira.el reads -text- as strikethrough and _text_ as italic with no
  ;; word-boundary guard, so kebab-case and snake_case get mangled on submit.
  ;; Drop "-" and "_" as delimiters; accept losing those two marks.
  (with-eval-after-load 'jira-doc
    (let ((drop '("-" "_")))
      (setq jira-doc--marks-delimiters
            (seq-remove (lambda (d) (member (car d) drop)) jira-doc--marks-delimiters))
      (setq jira-doc--markup-marks
            (seq-remove (lambda (m) (member (cadr m) drop)) jira-doc--markup-marks)))))

;; =================================
;; ==== * END DECLARE PACKAGE * ====
;; =================================
