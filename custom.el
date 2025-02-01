(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(browse-at-remote-prefer-symbolic nil)
 '(clean-buffer-list-delay-general 3)
 '(cljr-hotload-dependencies t)
 '(comint-input-ring-size 10000)
 '(copilot-indent-offset-warning-disable t)
 '(copilot-max-char-warning-disable t)
 '(es-always-pretty-print t)
 '(exec-path-from-shell-check-startup-files nil)
 '(gptel-api-key 'gptel-api-key-from-auth-source)
 '(gptel-default-mode 'org-mode)
 '(httpd-port 8100)
 '(js-indent-level 4)
 '(js2-include-node-externs t)
 '(ledger-accounts-file "~/Dropbox/ledger/notes.ledger")
 '(ledger-post-auto-align nil)
 '(ledger-report-use-native-highlighting nil)
 '(ledger-reports
   '(("bal" "ledger [[ledger-mode-flags]] -f ~/Dropbox/ledger/notes.ledger bal Expenses --current --format \"\\\12        %-17((depth_spacer)+(partial_account))\\\12        %10(percent(market(display_total), market(parent.total)))\\\12        %16(market(display_total))\\n%/\"\12")
     ("reg" "%(binary) -f %(ledger-file) reg --monthly --period-sort \"(amount)\" ^Expenses")
     ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
     ("account" "%(binary) -f %(ledger-file) reg %(account)")))
 '(mac-command-modifier 'super)
 '(mac-option-modifier 'meta)
 '(magit-list-refs-sortby "-committerdate")
 '(major-mode-remap-alist
   '((python-mode . python-ts-mode)
     (yaml-mode . yaml-ts-mode)
     (js-json-mode . json-ts-mode)
     (dockerfile-mode . dockerfile-ts-mode)
     (go-mode . go-ts-mode)))
 '(org-agenda-files '("~/Dropbox/org-mode/todo.org"))
 '(shell-pop-shell-type '("shell" "*shell*" (lambda nil (shell))))
 '(sly-net-coding-system 'utf-8-unix)
 '(straight-recipes-gnu-elpa-use-mirror t)
 '(tramp-default-method "ssh")
 '(undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(corfu-current ((t (:background "black" :foreground "white"))))
 '(corfu-default ((t (:background "gray31"))))
 '(embark-verbose-indicator-documentation ((t (:inherit completions-common-part))))
 '(marginalia-documentation ((t (:inherit completions-common-part)))))
