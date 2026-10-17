(setq package-enable-at-startup nil)

(menu-bar-mode -1)
(when (eq system-type 'darwin)
  ;; ns-win re-enables `menu-bar-mode' during `window-system-initialization'
  ;; (which runs after `early-init.el' and before `before-init-hook') unless
  ;; the macOS "Menus" default is explicitly NO.
  (add-hook 'before-init-hook (lambda () (menu-bar-mode -1))))

(tool-bar-mode -1)

;; Guard against builds compiled without toolkit scroll bars
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Disable title bar
;; On KDE, use ~/.config/kwinrulesrc to hide the title bar; KDE/Wayland
;; may not honor this frame parameter.
(unless (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(undecorated . t)))
