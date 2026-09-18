(setq package-enable-at-startup nil)

;; Disable title bar
;; On KDE, use ~/.config/kwinrulesrc to hide the title bar; KDE/Wayland
;; may not honor this frame parameter.
(unless (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(undecorated . t)))
