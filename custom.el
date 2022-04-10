(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cljr-hotload-dependencies t)
 '(company-show-quick-access t)
 '(es-always-pretty-print t)
 '(exec-path-from-shell-check-startup-files nil)
 '(flycheck-python-flake8-executable "/usr/local/bin/python3")
 '(httpd-port 8100)
 '(js2-include-node-externs t)
 '(ledger-accounts-file "/Users/longg/Dropbox/ledger/notes.ledger")
 '(ledger-report-use-native-highlighting nil)
 '(ledger-reports
   '(("" "")
     ("bal" "ledger [[ledger-mode-flags]] -f /Users/longg/Dropbox/ledger/notes.ledger bal Expenses --current --format \"\\
        %-17((depth_spacer)+(partial_account))\\
        %10(percent(market(display_total), market(parent.total)))\\
        %16(market(display_total))\\n%/\"
")
     ("reg" "%(binary) -f %(ledger-file) reg --monthly --period-sort \"(amount)\" ^Expenses")
     ("payee" "%(binary) -f %(ledger-file) reg @%(payee)")
     ("account" "%(binary) -f %(ledger-file) reg %(account)")))
 '(mac-command-modifier 'super)
 '(mac-option-modifier 'meta)
 '(org-agenda-files '("~/Dropbox/org-mode/todo.org"))
 '(shell-pop-shell-type
   '("ansi-term" "*ansi-term*"
     (lambda nil
       (ansi-term shell-pop-term-shell))))
 '(sly-net-coding-system 'utf-8-unix)
 '(straight-recipes-gnu-elpa-use-mirror t)
 '(tramp-default-method "ssh"))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(embark-verbose-indicator-documentation ((t (:inherit completions-common-part))))
 '(marginalia-documentation ((t (:inherit completions-common-part)))))
