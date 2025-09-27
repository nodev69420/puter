(require 'adam)

(use-package desktop-environment)

(defun puter/logout ()
  (interactive)
  (bookmark-save)
  (recentf-save-list)
  (save-some-buffers)
  (start-process-shell-command "logout" nil "lxsession-logout"))

(defvar puter/polybar-proc nil)

(defun puter/polybar-kill ()
  (interactive)
  (when puter/polybar-proc
    (ignore-errors
      (kill-process puter/polybar-proc)))
  (setq puter/polybar-proc nil))

(defun puter/polybar-start ()
  (interactive)
  (puter/polybar-kill)
  (setq puter/polybar-proc (start-process-shell-command "polybar" nil "polybar panel")))

(use-package exwm
  :config
  (setq exwm-workspace-number 10)

  (setq focus-follows-mouse t)
  (setq mouse-autoselect-window t)
  (setq exwm-workspace-warp-cursor t)

  (setq exwm-input-prefix-keys
        '(
          ?\C-x
          ?\C-u
          ?\C-h
          ?\M-x
          ?\M-`
          ?\M-&
          ?\M-:
          ?\C-\M-j
          ?\C-\ ))

  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  (add-hook 'exwm-update-class-hook (lambda () (exwm-workspace-rename-buffer exwm-class-name)))
  (add-hook 'exwm-update-title-hook
            (lambda ()
              (pcase exwm-class-name
                ("Firefox" (exwm-workspace-rename-buffer (format "Firefox: %s" exwm-title))))))
  (add-hook 'exwm-init-hook
            (lambda ()
              (adam/xsettings)
              (adam/set-wallpaper "~/puter/pape/pape2.jpg" 90)
              (puter/polybar-start)
              (message "EXWM finished")))

  (setq exwm-input-global-keys
        `(([?\s-r] . exwm-reset)

          ([?\s-w] . evil-window-delete)
          ([?\s-d] . evil-window-delete)
          ([?\s-.] . exwm-workspace-switch)
          ([?\s-m] . delete-other-windows-internal)

          ([?\s-h] . windmove-left)
          ([?\s-j] . windmove-down)
          ([?\s-l] . windmove-right)
          ([?\s-k] . windmove-up)

          ([?\s-H] . windmove-swap-states-left)
          ([?\s-J] . windmove-swap-states-down)
          ([?\s-L] . windmove-swap-states-right)
          ([?\s-K] . windmove-swap-states-up)

          ;; (,(kbd "C-s-h") . windmove-swap-states-left)
          ;; (,(kbd "C-s-h") . windmove-swap-states-down)
          ;; (,(kbd "C-s-h") . windmove-swap-states-right)
          ;; (,(kbd "C-s-h") . windmove-swap-states-up)

          ([s-left] . windmove-left)
          ([s-right] . windmove-right)
          ([s-up] . windmove-up)
          ([s-down] . windmove-down)

          ([?\s-&] . (lambda (cmd)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command cmd nil cmd)))


          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) . (lambda () (interactive) (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))
          ([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))))

  (require 'exwm-randr)
  (setq exwm-randr-workspace-monitor-plist '(0 "HDMI-1" 9 "HDMI-2"))
  (add-hook 'exwm-randr-screen-change-hook
            (lambda ()
              (start-process-shell-command "xrandr" nil "xrandr --output HDMI-1 --primary --output HDMI-2 --right-of HDMI-1")))
  (exwm-randr-mode 1)

  ;; (require 'exwm-systemtray)
  ;; (setq exwm-systemtray-height 32)
  ;; (exwm-systemtray-mode 1)
  )

(exwm-wm-mode 1)
