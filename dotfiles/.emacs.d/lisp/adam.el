(defun adam/qoutize-string (str)
  "surround a string STR in \"\" qoutes."
  (concat "\"" str "\""))

(defun adam/set-font (font-name font-size)
  "set frame font FONT-NAME and size FONT-SIZE."
  (let ((font-height (* font-size 10)))
    (set-face-attribute
     'default nil
     :font font-name :height font-height)
    (set-face-attribute
     'variable-pitch nil
     :font font-name :height font-height)
    (set-face-attribute
     'fixed-pitch nil
     :font font-name :height font-height))
  (let
      ((font-frame (concat font-name "-" (number-to-string font-size))))
    (add-to-list
     'default-frame-alist
     `(font . ,font-frame))))

(defun adam/set-frame-default-params ()
  "set all frame params."
  (adam/set-font "Iosevka Nerd Font Mono" 12))

;; Emacs daemon-mode doesn't load frame params correctly.
(if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (with-selected-frame frame
                    (adam/set-frame-default-params))))
  (adam/set-frame-default-params))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(defun adam/goto-init-file ()
  "open init file."
  (interactive)
  (find-file user-init-file))

(defun adam/goto-homepage ()
  "find main emacs page."
  (interactive)
  (find-file "~/adam/master.org"))

(defun adam/reload-init-file ()
  "reload emacs config."
  (interactive)
  (load-file user-init-file))

(defun adam/switch-buffer ()
  "switch to buffer command."
  (interactive)
  (call-interactively #'counsel-switch-buffer))

(defun adam/ibuffer ()
  "interactive buffer menu."
  (interactive)
  (call-interactively #'ibuffer))

(defun adam/find-file ()
  "find file."
  (interactive)
  (call-interactively #'find-file-existing))

(defun adam/find-file-new ()
  "file file new."
  (interactive)
  (call-interactively #'counsel-find-file))

(defun adam/imenu ()
  "interactive menu."
  (interactive)
  (call-interactively #'counsel-imenu))

(defun adam/M-x ()
  "meta x."
  (interactive)
  (call-interactively #'counsel-M-x))

(defun adam/lookup-func ()
  "lookup symbol under cursor."
  (interactive)
  (cond ((eq major-mode 'emacs-lisp-mode)
         (call-interactively #'describe-symbol))
        ((eq lsp-mode t)
         (call-interactively #'lsp-describe-thing-at-point))
        (t (call-interactively #'man))))

(defun adam/fuzzy-find ()
  "fuzzy find based on the contents of the current buffer."
  (interactive)
  (cond ((eq major-mode 'dired-mode)
         (call-interactively #'adam/find-file))
        ((eq major-mode 'eshell-mode)
         (call-interactively #'adam/find-file))
        ((eq major-mode 'ibuffer-mode)
         (call-interactively #'adam/switch-buffer))
        (t (call-interactively #'adam/imenu))))


(defvar adam/xsettings-list
  '("setxkbmap gb"
    "xset r rate 200 80"
    "xset s off -dpms"
    "xgfxtablet &"))

(defun adam/xsettings ()
  "call puter xsettings script."
  (interactive)
  (dolist (el adam/xsettings-list)
    (ignore-errors
      (start-process-shell-command el nil el)))
  (message "xsettings applied"))

;; (defun adam/set-wallpaper (pape &optional window-transparency)
;;   "Set the desktop wallpaper to a filepath PAPE."
;;   (interactive)
;;   (when window-transparency
;;     (set-frame-parameter nil 'alpha-background window-transparency))
;;   (if (shell-command (concat "feh --bg-fill " pape))
;;       (message (concat "wallpaper set: " pape))
;;       (message (concat "wallpaper failed to be set to: " pape))))


(defun adam/puter-linkup ()
  "relink all the puter based files."
  (interactive)
  (if (shell-command "~/puter/scriptz/puter-linkup")
      (message "linked-up!")
      (message "linked-up failed!")))

;; why do i have two versions of this function???

(defun puter/linkup ()
  (interactive)
  (if (shell-command "puter-linkup")
      (message "linked up!")
    (message "linkup failed!")))

(defvar adam/auth-file "~/adam/auth.json")

(defun adam/lookup-auth (auth-sym)
  "Fetch a given auth string from the auth-file with a given symbol: auth-sym."
  (cdr (assoc auth-sym (json-read-file adam/auth-file))))

(defun adam/display-startup-time ()
  "display emacs starting time."
  (message "emacs loaded in: %s, gc collects: %d."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook #'adam/display-startup-time)

(defun myeshell/clear ()
  "clears the current eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))


(provide 'adam)
