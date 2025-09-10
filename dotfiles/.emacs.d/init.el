(setq custom-file "~/.emacs.d/custom.el")
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode -1)
(menu-bar-mode -1)
(setq visible-bell nil)
(column-number-mode 1)
(setq-default cursor-in-non-selected-windows nil)

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; transparency
(set-frame-parameter nil 'alpha-background 90)

(defalias 'yes-or-no-p 'y-or-n-p)
(setq vc-follow-symlinks t)

(global-display-line-numbers-mode 1)
(set-default 'truncate-lines t)

(global-prettify-symbols-mode 1)

(setq blink-cursor-interval 0.15)
(setq blink-cursor-blinks -1)
(setq mouse-autoselect-window t)
(setq focus-follows-mouse t)
(global-hl-line-mode)
(global-visual-line-mode)

(display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)
(setq browse-url-browser-function #'browse-url-firefox)

(setq debug-on-error t)
(setq edebug-all-forms t)
(setq message-log-max 16384)
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq make-backup-files t)
(setq version-control nil)
(setq backup-by-copying t)
(setq vc-make-backup-files t)
(setq delete-old-versions t)
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))

(push "/home/adam/.emacs.d/extra/" load-path)

(electric-pair-mode 1)
(setq compile-command "")

(setq read-process-output-max (* 1024 1024))
(setq gc-cons-threshold (* 100 1024 1024))

;; * ADAM FUNCTIONS

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
  ;; (adam/set-font "JetBrainsMono Nerd Font Mono" 13)
  (adam/set-font "Iosevka Nerd Font Mono" 13))

;; Emacs daemon-mode doesn't load frame params correctly.
(if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (with-selected-frame frame
                    (adam/set-frame-default-params))))
  (adam/set-frame-default-params))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(defun adam/enable-fast-keys ()
  "enable fast typing."
  (interactive)
  (shell-command-to-string "xset r rate 200 80"))

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

;; * PACMAN

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq straight-use-package-by-default t)

;; * EMAC

(use-package emacs
  :hook (emacs-lisp-mode . adam/elisp-setup)
  :config
  (defun adam/elisp-setup ()
    "custom elisp setup."
    (setq-local imenu-generic-expression
                '(("function" "^\\s-*(defun\\s-+\\([^[:space:]]+\\)" 1)
                  ("variable" "^\\s-*(defvar\\s-+\\([^[:space:]]+\\)" 1)
                  ("macro" "^\\s-*(defmacro\\s-+\\([^[:space:]]+\\)" 1)
                  ("require" "^\\s-*(require\\s-+\\([^[:space:]]+\\)" 1)
                  ("package" "^\\s-*(use-package\\s-+\\([^[:space:]]+\\)" 1)))))



(use-package counsel
  :init
  (counsel-mode 1)
  :bind (:map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (counsel-mode t))

(use-package swiper)
(use-package flx)
(use-package ivy
  :bind
  (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :custom
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)
  :config
  (use-package org-ivy-search)
  (ivy-mode 1)
  (setq ivy-height 20)
  (setq ivy-re-builders-alist '((t . ivy--regex)
                                (t . ivy--regex-fuzzy))))


(use-package ivy-rich
  :after
  ivy
  :init (ivy-rich-mode 1))

;; (use-package hydra)

(use-package all-the-icons)
(use-package all-the-icons-completion)
(use-package all-the-icons-dired)
(use-package all-the-icons-ibuffer)
(use-package all-the-icons-ivy)
(use-package all-the-icons-nerd-fonts)

(use-package which-key
  :init
  (which-key-mode 1)
  :config
  (setq which-key-side-window-location 'bottom
        which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-max-display-columns nil
        which-key-min-display-lines 6))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package company
  :after
  lsp-mode
  :hook (prog-mode . company-mode)
  :bind
  (:map company-active-map
        ("C-l" . company-complete-selection)
        ("<return>" . nil)
        ("RET" . nil))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

;; * THEMES

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic nil)
  (setq doom-winter-is-coming-no-italics t)
  (setq doom-winter-is-coming-brighter-comments t)
  (setq doom-ir-black-brighter-comments t))

(use-package modus-themes)

(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 28)
  (setq doom-modeline-enable-word-count t))

;; * EVIL

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-want-minibuffer nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-normal-state-map
              (kbd "C-u")
              #'(lambda ()
                  (interactive)
                  (call-interactively #'recenter)
                  (evil-scroll-up 16)))
  (define-key evil-normal-state-map
              (kbd "C-d")
              #'(lambda ()
                  (interactive)
                  (call-interactively #'recenter)
                  (evil-scroll-down 16)))

  ;; EMMS Volume
  (define-key evil-normal-state-map
              (kbd "C--")
              #'(lambda () (interactive) (emms-volume-lower)))
  (define-key evil-normal-state-map
              (kbd "C-=")
              #'(lambda () (interactive) (emms-volume-raise)))

  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  (setq evil-lookup-func #'adam/lookup-func))

(use-package evil-lispy
  :after
  evil
  :config
  (add-hook 'evil-mode-hook #'evil-lispy-mode))

(use-package evil-collection
  :after
  evil
  :config
  (evil-collection-init))

(use-package evil-nerd-commenter
  :bind
  ("C-;" . evilnc-comment-or-uncomment-lines))

;; (use-package projectile
;;   :config
;;   (projectile-mode)
;;   :custom ((projectile-completion-system 'ivy))
;;   :bind-keymap
;;   ("C-c p" . projectile-command-map)
;;   :init
;;   (when (file-directory-p "~/work")
;;     (setq projectile-project-search-path '("~/work")))
;;   (setq projectile-switch-project-action #'projectile-dired))

;; * LSP

(use-package flycheck)

(use-package lsp-mode
  :init
  (setq lsp-log-io nil)
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-enable-which-key-integration t)
  :config
  (setq lsp-clients-clangd-args '("--header-insertion=never")))

(use-package lsp-ui
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-show-with-cursor nil)
  (setq lsp-ui-doc-show-with-mouse nil))

(use-package lsp-ivy)

(use-package yasnippet
  :config
  (yas-global-mode 1))

;; (use-package dap-mode
;;   :hook (dap-stopped . (lambda (arg) (call-interactively #'dap-hydra)))
;;   :config
;;   (setq lsp-enable-dap-auto-configure nil)
;;   (require 'dap-gdb)
;;   (dap-register-debug-template
;;    "gdb-pok"
;;    (list :type "gdb"
;;          :request "launch"
;;          :name "gdb-pok"
;;          :program "${workspaceFolder}/out/pok"
;;          :args "./test/test.pok")))

;; * DIRED

(use-package dired
  :straight nil
  :commands (dired dired-jump)
  :bind
  (("C-x C-j" . dired-jump))
  :custom
  ((dired-listing-switches "-lah --group-directories-first"))
  :config
  (setq dired-kill-when-opening-new-dired-buffer t)
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-find-file
    "R" 'dired-do-rename
    "C" 'dired-do-copy
    "D" 'dired-do-delete)
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package dired-open
  :config
  (setq dired-open-extensions
        '(("gif" . "ristretto")
          ("jpg" . "ristretto")
          ("png" . "ristretto")
          ("mkv" . "mpv")
          ("mp4" . "mpv")
          ("webm" . "mpv")
          ("xcf" . "gimp")
          ("kra" . "krita")
          ("pdf" . "xreader")
          ("epub" . "xreader")
          ("blend" . "blender"))))

(use-package dired-hide-dotfiles
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

;; * SHELLS

(use-package eshell
  :bind
  (:map eshell-mode-map
        ("C-l" . myeshell/clear))
  :config
  (mapc (lambda (alias) (defalias (car alias) (cdr alias)))
        '((ll . (lambda () (eshell/ls '-lah)))
          (dir . dired))))

(use-package eshell-syntax-highlighting
  :after
  esh-mode
  :config
  (eshell-syntax-highlighting-global-mode 1))

(use-package vterm)

;; * MISC

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package magit)
(use-package git-gutter
  :config
  (global-git-gutter-mode 1))

(use-package forge)

(use-package emms
  :init
  (emms-all)
  (setq emms-player-list '(emms-player-mpv))
  (setq emms-info-functions '(emms-info-native))
  :config
  (setq-default emms-source-file-default-directory "~/Music/")
  (setq-default emms-volume-change-function 'emms-volume-mpv-change))

(use-package gptel
  :config
  (setq gptel-model 'deepseek-chat)
  (setq gptel-backend
        (gptel-make-openai "DeepSeek"
          :host "api.deepseek.com"
          :endpoint "/chat/completions"
          :stream t
          :key (adam/lookup-auth 'deepseek)
          :models '(deepseek-chat deepseek-coder))))


;; * DATA

(use-package i3wm-config-mode)
(use-package css-mode)
(use-package yaml-mode)
(use-package js2-mode
  :config
  (add-hook 'js-mode-hook 'js2-minor-mode)
  (add-hook 'js2-mode-hook 'ac-js2-mode)
  (add-hook 'js-mode-hook 'lsp-mode))

(use-package org
  :config
  (setq org-edit-src-content-indentation 0)
  (setq org-link-descriptive t))

;; * LISPS

(use-package sly
  :config
  (setq inferior-lisp-program "/bin/sbcl --dynamic-space-size 4Gb"))

(use-package sly-asdf)
(use-package sly-quicklisp)

(use-package clojure-mode)
(use-package cider)

;; * REAL LANGUAGES

(use-package cc-mode
  :hook ((c-mode . lsp-mode)
         (c++-mode . lsp-mode))
  :config
  (c-add-style
   "adam"
   '((c-auto-align-backslashes . nil)
     (c-continued-statement-offset . 4)
     (c-basic-offset . 4)
     (c-offsets-alist
      (arglist-intro . +)
      (substatement-open . +)
      (inline-open . +)
      (block-open . +)
      (brace-list-open . +)
      (case-label . +)
      )))
  (setq c-default-style "adam"))

(use-package zig-mode
  :config
  (add-hook 'zig-mode-hook 'lsp-mode)
  (add-hook 'zig-mode-hook
            #'(lambda ()(interactive)(zig-format-on-save-mode -1))))

(use-package rust-mode
  :config
  (add-hook 'rust-mode-hook 'lsp-mode))

(use-package go-mode
  :config
  (add-hook 'go-mode-hook 'lsp-mode))

;; * KIDDY SCRIPTING LANGUAGES

(use-package lua-mode
  :config
  (add-hook 'lua-mode-hook 'lsp-mode)
  (define-key lua-mode-map (kbd "<normal-state> K") nil))

(use-package gdscript-mode
  :config
  (add-hook 'gdscript-mode-hook 'lsp-mode)
  (setq gdscript-godot-executable "/bin/godot/godot")
  (setq gdscript-use-tab-indents t)
  (setq gdscript-gdformat-save-and-format nil))

;; * SHADERS

(use-package glsl-mode)
(use-package wgsl-mode)

;; * KEYMAPS

(use-package general
  :config
  (general-evil-setup)
  (general-create-definer adam/leader-keys
    :states
    '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")
  (adam/leader-keys
    "SPC" '(adam/M-x :wk "M-x")

    "s" '(:ignore t :wk "search")
    "ss" '(swiper :wk "search swiper")

    "d" '(:ignore t :wk "debug")
    "dm" '(dap-hydra :wk "debug menu")

    "a" '(:ignore t :wk "ai")
    "aa" '(gptel :wk "ai start")

    "f" '(:ignore t :wk "find")
    "fc" '(adam/goto-init-file :wk "find config")
    "fh" '(adam/goto-homepage :wk "find homepage")
    "fp" '(list-processes :wk "find processes")
    "ff" '(adam/fuzzy-find :wk "find fuzzy-contextual")
    "fa" '(lsp-ivy-workspace-symbol :wk "find lsp symbol")
    "f." '(adam/find-file :wk "find file")
    "f," '(projectile-find-file :wk "find project file")
    "fz" '(projectile-switch-project :wk "find project")
    "fn" '(adam/find-file-new :wk "file file new")
    "fe" '(list-matching-lines :wk "find regex")

    "gg" '(magit :wk "magit")

    "b" '(:ignore t :wk "buffer")
    "bb" '(adam/switch-buffer :wk "buffer switch")
    "bm" '(adam/ibuffer :wk "buffer menu")
    "bx" '(kill-buffer :wk "buffer kill")

    "m" '(:ignore t :wk "music")
    "mm" '(emms :wk "music")
    "mp" '(emms-pause :wk "music pause")
    "mf" '(emms-play-directory :wk "music play")

    "e" '(:ignore t :wk "eval")
    "ee" '(eval-expression :wk "eval expression")
    "eb" '(eval-buffer :wk "eval buffer")
    "ex" '(eval-last-sexp :wk "eval last sexpr")

    "h" '(:ignore t :wk "help")
    "hf" '(counsel-describe-function :wk "help function")
    "hv" '(counsel-describe-variable :wk "help variable")

    "l" '(:ignore t :wk "lsp")
    "lr" '(lsp-rename :wk "lsp rename")
    "lf" '(lsp-find-references :wk "lsp find references")
    "ld" '(flycheck-list-errors :wk "lsp errors")
    "la" '(lsp-execute-code-action :wk "lsp code action")
    "ls" '(lsp-treemacs-symbols :wk "lsp symbols tab")

    "p" '(:ignore t :wk "perspective")
    "pp" '(persp-switch :wk "perspective switch")
    "px" '(persp-kill :wk "perspective kill")

    "w" '(:ignore t :wk "window")
    "w1" '(delete-other-windows-internal :wk "window solo")
    "wn" '(evil-window-split :wk "window split horizontal")
    "wv" '(evil-window-vsplit :wk "window split vertical")
    "ww" '(evil-window-next :wk "window next")
    "wc" '(evil-window-delete :wk "window close")
    "wx" '(kill-buffer-and-window :wk "window kill and close")
    "wh" '(evil-window-left :wk "window left")
    "wj" '(evil-window-down :wk "window down")
    "wk" '(evil-window-up :wk "window up")
    "wl" '(evil-window-right :wk "window right")

    "c" '(:ignore t :wk "command")
    "ce" '(eshell :wk "command eshell")
    "cc" '((lambda ()
               (interactive)
               (call-interactively #'compile))
           :wk "command current")
    "cp" '((lambda ()
               (interactive)
               (when (projectile-project-p)
                   (call-interactively #'projectile-compile-project)))
               :wk "command project")
    "cn" '((lambda ()
               (interactive)
               (setq compile-command "")
               (call-interactively #'compile))
           :wk "command new")

    "r" '(:ignore t :wk "reload")
    "rc" '(adam/reload-init-file :wk "reload config")

    "q" '(:ignore t :wk "quick tool")
    "qq" '(quick-calc :wk "quick tool calculator")))


(define-minor-mode adam-mode
  "Adam global mode for Adam based sheringans!"
  1
  :global t
  :group 'adam
  :lighter " adam-mode"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "M-x") 'adam/M-x)
            (define-key map (kbd "C-x SPC") 'adam/M-x)
            (define-key map (kbd "C-x j") 'adam/fuzzy-find)
            (define-key map (kbd "C-x .") 'adam/find-file)
            (define-key map (kbd "C-x ,") 'counsel-linux-app)
            (define-key map (kbd "C-x b") 'adam/switch-buffer)
            (define-key map (kbd "C-x C-b") 'adam/ibuffer)
            
            (define-key map (kbd "C-x k") 'kill-buffer)
            (define-key map (kbd "C-x K") 'kill-buffer-and-window)

            (define-key map (kbd "C-x w 1") 'delete-other-windows)
            (define-key map (kbd "C-x w n") 'evil-window-split)
            (define-key map (kbd "C-x w v") 'evil-window-vsplit)
            (define-key map (kbd "C-x w w") 'evil-window-next)
            (define-key map (kbd "C-x w k") 'evil-window-delete)
            (define-key map (kbd "C-x w h") 'windmove-left)
            (define-key map (kbd "C-x w j") 'windmove-down)
            (define-key map (kbd "C-x w k") 'windmove-up)
            (define-key map (kbd "C-x w l") 'windmove-right)

            (define-key map (kbd "C-x c f")
                        #'(lambda () (interactive) (kill-new (buffer-file-name))))
            (define-key map (kbd "C-x c d")
                        #'(lambda ()
                            (interactive)
                            (kill-new
                             (file-name-directory (buffer-file-name)))))
            map))

;; * EXWM

(use-package exwm
  :config
  (setq exwm-workspace-number 5)

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

  (setq exwm-input-global-keys
        `(([?\s-r] . exwm-reset)

          ([s-left] . windmove-left)
          ([s-right] . windmove-right)
          ([s-up] . windmove-up)
          ([s-down] . windmove-down)

          ([?\s-&] . (lambda (cmd)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command cmd nil cmd)))

          ([?\s-w] . exwm-workspace-switch)

          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) . (lambda () (interactive) (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9)))))

(exwm-wm-mode)

;; * LAST

(load-theme 'modus-vivendi-tinted t)
;; (load-theme 'doom-winter-is-coming-dark-blue t)
(load-file custom-file)
(setq inhibit-startup-screen t)
(adam/goto-homepage)

(provide 'init)
