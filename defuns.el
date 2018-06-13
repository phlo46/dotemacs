;; ==== MY CUSTOM FUNCTIONS =====
;; ==============================

(defun toggle-frame-split ()
  "If the frame is split vertically, split it horizontally or vice versa.
   Assumes that the frame is only split into two."
  (interactive)
  (unless (= (length (window-list)) 2) (error "Can only toggle a frame split in two"))
  (let ((split-vertically-p (window-combined-p)))
    (delete-window)                     ; closes current window
    (if split-vertically-p
        (split-window-horizontally)
      (split-window-vertically)) ; gives us a split with the other window twice
    (switch-to-buffer nil))) ; restore the original window in this part of the frame

(global-set-key (kbd "C-x |") 'toggle-frame-split)

;; sudo edit
(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.
   With a prefix ARG prompt for a file to visit.
   Will also prompt for a file to visit if current
   buffer is not visiting a file."

  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))


;; shell emacs
(defun terminal (buffer-name)
  "Start a terminal and rename buffer."
  (interactive "sShell name: ")
  (ansi-term "zsh")
  (rename-buffer buffer-name t))

(global-set-key (kbd "C-'") 'terminal)

;; ssh
(defun vicare-nightly ()
  (interactive)
  (dired "/ubuntu@nightly.vicare.vn:/home/ubuntu/meddy"))

(defun xander-nightly ()
  (interactive)
  (dired "/ubuntu@nightly.xanderp.vicare.vn:/home/ubuntu/xanderp_server"))

(defun vicare-staging ()
  (interactive)
  (dired "/ubuntu@staging.vicare.vn:/home/ubuntu/meddy"))

;; sql db
(setq sql-connection-alist
      '((xander_local (sql-product 'postgres)
                      (sql-port 5400)
                      (sql-server "localhost")
                      (sql-user "super_user")
                      (sql-password "super_hard_to_guess")
                      (sql-database "xanderp"))
        (xander_nightly (sql-product 'postgres)
                        (sql-port 5410)
                        (sql-user "super_user")
                        (sql-server "localhost")
                        (sql-database "xanderp"))
        (vicare_nightly (sql-product 'postgres)
                        (sql-port 5415)
                        (sql-user "projectf_dev")
                        (sql-server "localhost")
                        (sql-database "meddy"))
        (vicare_local (sql-product 'postgres)
                      (sql-port 5432)
                      (sql-server "localhost")
                      (sql-user "projectf_dev")
                      (sql-database "meddy"))
        (local (sql-product 'postgres)
               (sql-port 5432)
               (sql-server "localhost")
               (sql-user "admin")
               (sql-database "book"))))

(defun xander-local-db ()
  (interactive)
  (my-sql-connect 'postgres 'xander_local))

(defun xander-nightly-db ()
  (interactive)
  (my-sql-connect 'postgres 'xander_nightly))

(defun vicare-nightly-db ()
  (interactive)
  (my-sql-connect 'postgres 'vicare_nightly))

(defun vicare-local-db ()
  (interactive)
  (my-sql-connect 'postgres 'vicare_local))

(defun local-db ()
  (interactive)
  (my-sql-connect 'postgres 'local))

(defun my-sql-connect (product connection)
  (setq sql-product product)
  (sql-connect connection))



;; helper functions for work.
(defun fire-up-meddy ()
  (interactive)
  (let ((meddy-term-buffers (list "sv" "web" "shell")))
    (dolist (buff-name meddy-term-buffers)
      (let ((b (or (get-buffer buff-name)
                   (get-buffer (format "*%s*" buff-name)))))
        (cond
         ((null b)
          (setq b (ansi-term "/usr/local/bin/zsh" buff-name)))
         ((not (eq 'term-mode (buffer-local-value 'major-mode b)))
          (kill-buffer b)
          (setq b (ansi-term "/usr/local/bin/zsh" buff-name)))
         (t nil))
        (process-send-string b "cd ~/work/meddy\n")
        (sleep-for 1)
        (process-send-string b "vs\n")
        (sleep-for 2.5)
        (process-send-string b "vagrant\n")))))



;; Eyebrowse - allow perspective-local workspaces
(defun get-persp-workspace (&optional persp frame)
  "Get the correct workspace parameters for perspective.
   + PERSP is the perspective, and defaults to the current perspective.
   + FRAME is the frame where the parameters are expected to be used, and defaults
     to the current frame."
    (let ((param-names (if (display-graphic-p frame)
                         '(gui-eyebrowse-window-configs
                           gui-eyebrowse-current-slot
                           gui-eyebrowse-last-slot)
                       '(term-eyebrowse-window-configs
                         term-eyebrowse-current-slot
                         term-eyebrowse-last-slot))))
    (--map (persp-parameter it persp) param-names)))

(defun set-persp-workspace (workspace-params &optional persp frame)
  "Set workspace parameters for perspective.
   + WORKSPACE-PARAMS should be a list containing 3 elements in this order:
     - window-configs, as returned by (eyebrowse--get 'window-configs)
     - current-slot, as returned by (eyebrowse--get 'current-slot)
     - last-slot, as returned by (eyebrowse--get 'last-slot)
   + PERSP is the perspective, and defaults to the current perspective.
   + FRAME is the frame where the parameters came from, and defaults to the current frame.

   Each perspective has two sets of workspace parameters:
     * one set for graphical frames,
     * and one set for terminal frames."
  (let ((param-names (if (display-graphic-p frame)
                         '(gui-eyebrowse-window-configs
                           gui-eyebrowse-current-slot
                           gui-eyebrowse-last-slot)
                       '(term-eyebrowse-window-configs
                         term-eyebrowse-current-slot
                         term-eyebrowse-last-slot))))
    (--zip-with (set-persp-parameter it other persp)
                param-names workspace-params)))

(defun load-eyebrowse-for-perspective (type &optional frame)
  "Load an eyebrowse workspace according to a perspective's parameters.
 FRAME's perspective is the perspective that is considered, defaulting to
 the current frame's perspective.
 If the perspective doesn't have a workspace, create one."
  (when (eq type 'frame)
    (let* ((workspace-params (get-persp-workspace (get-frame-persp frame) frame))
           (window-configs (nth 0 workspace-params))
           (current-slot (nth 1 workspace-params))
           (last-slot (nth 2 workspace-params)))
      (if window-configs
          (progn
            (eyebrowse--set 'window-configs window-configs frame)
            (eyebrowse--set 'current-slot current-slot frame)
            (eyebrowse--set 'last-slot last-slot frame)
            (eyebrowse--load-window-config current-slot))
        (eyebrowse--set 'window-configs nil frame)
        (eyebrowse-init frame)
        (save-eyebrowse-for-perspective frame)))))


(defun update-eyebrowse-for-perspective (&rest _args)
  "Update and save current frame's eyebrowse workspace to its perspective."
  (let* ((current-slot (eyebrowse--get 'current-slot))
         (current-tag (nth 2 (assoc current-slot (eyebrowse--get 'window-configs)))))
    (eyebrowse--update-window-config-element
     (eyebrowse--current-window-config current-slot current-tag)))
  (save-eyebrowse-for-perspective))

(defun save-eyebrowse-for-perspective (&optional frame)
  "Save FRAME's eyebrowse workspace to FRAME's perspective.
FRAME defaults to the current frame."
  (set-persp-workspace (list (eyebrowse--get 'window-configs frame)
                             (eyebrowse--get 'current-slot frame)
                             (eyebrowse--get 'last-slot frame))
                       (get-frame-persp frame)
                       frame))



;; (defun load-eyebrowse-for-perspective (&optional frame)
;;   "Load an eyebrowse workspace according to a perspective's parameters.
;; FRAME's perspective is the perspective that is considered, defaulting to
;; the current frame's perspective.
;; If the perspective doesn't have a workspace, create one."
;;   (let* ((persp (get-frame-persp frame))
;;          (window-configs (persp-parameter 'eyebrowse-window-configs persp))
;;          (current-slot (persp-parameter 'eyebrowse-current-slot persp))
;;          (last-slot (persp-parameter 'eyebrowse-last-slot persp)))
;;     (if window-configs
;;         (progn
;;           (eyebrowse--set 'window-configs window-configs frame)
;;           (eyebrowse--set 'current-slot current-slot frame)
;;           (eyebrowse--set 'last-slot last-slot frame)
;;           (eyebrowse--load-window-config current-slot))
;;       (eyebrowse--set 'window-configs nil frame)
;;       (eyebrowse-init frame)
;;       (save-eyebrowse-for-perspective frame))))

;; (defun update-eyebrowse-for-perspective (&rest _args)
;;   "Update and save current frame's eyebrowse workspace to its perspective."
;;   (eyebrowse--update-window-config-element
;;    (eyebrowse--current-window-config (eyebrowse--get 'current-slot)
;;                                      (eyebrowse--get 'current-tag)))
;;   (save-eyebrowse-for-perspective))

;; (defun save-eyebrowse-for-perspective (&optional frame)
;;   "Save FRAME's eyebrowse workspace to FRAME's perspective. FRAME defaults to the current frame."
;;   (let ((persp (get-frame-persp frame)))
;;     (set-persp-parameter
;;      'eyebrowse-window-configs (eyebrowse--get 'window-configs frame) persp)
;;     (set-persp-parameter
;;      'eyebrowse-current-slot (eyebrowse--get 'current-slot frame) persp)
;;     (set-persp-parameter
;;      'eyebrowse-last-slot (eyebrowse--get 'last-slot frame) persp)))
