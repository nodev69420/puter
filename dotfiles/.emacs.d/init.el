;;; package -- Summary
;;; Commentary:
;;; Code:

(setq custom-file "~/.emacs.d/custom.el")
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode -1)
(menu-bar-mode -1)
(setq visible-bell nil)
(setq inhibit-startup-screen t)
(column-number-mode 1)
(setq-default cursor-in-non-selected-windows nil)

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; Transparency
;; (set-frame-parameter nil 'alpha-background 95)

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

(electric-pair-mode 1)
(setq compile-command "")

(setq read-process-output-max (* 1024 1024))
(setq gc-cons-threshold (* 100 1024 1024))

(defun adam/qoutize-string (str)
  "Surround a string STR in \"\" qoutes."
  (concat "\"" str "\""))

(defun adam/set-font (font-name font-size)
  "Set frame font FONT-NAME and size FONT-SIZE."
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
  "Set all frame params."
  (adam/set-font "JetBrainsMono Nerd Font Mono" 13))

;; Emacs daemon-mode doesn't load frame params correctly.
(if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (with-selected-frame frame
                    (adam/set-frame-default-params))))
  (adam/set-frame-default-params))


(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(defun adam/enable-fast-keys ()
  "Enable fast typing."
  (interactive)
  (shell-command-to-string "xset r rate 200 80"))

(defun adam/goto-init-file ()
  "Open init file."
  (interactive)
  (find-file user-init-file))

(defun adam/goto-homepage ()
  "Find plan."
  (interactive)
  (find-file "~/adam/homepage.org"))

(defun adam/reload-init-file ()
  "Reload Emacs config."
  (interactive)
  (load-file user-init-file))

(defun adam/switch-buffer ()
  "Switch to buffer command."
  (interactive)
  (call-interactively #'counsel-switch-buffer))

(defun adam/ibuffer ()
  "Interactive buffer menu."
  (interactive)
  (call-interactively #'ibuffer))

(defun adam/find-file ()
  "Find file."
  (interactive)
  (call-interactively #'find-file-existing))

(defun adam/find-file-new()
  "File file new."
  (interactive)
  (call-interactively #'counsel-find-file))

(defun adam/imenu ()
  "Interactive menu."
  (interactive)
  (call-interactively #'counsel-imenu))

(defun adam/M-x ()
  "Meta X."
  (interactive)
  (call-interactively #'counsel-M-x))

(defun adam/lookup-func ()
  "Lookup symbol under cursor."
  (interactive)
  (cond ((eq major-mode 'elisp-mode)
         (call-interactively #'describe-symbol))
        ((eq lsp-mode t)
         (call-interactively #'lsp-describe-thing-at-point))
        (t (call-interactively #'man))))

(defun adam/fuzzy-find ()
  "Fuzzy find based on the contents of the current buffer."
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
  "Display Emacs starting time."
  (message "Emacs loaded in: %s, gc collects: %d."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook #'adam/display-startup-time)

(defun myeshell/clear ()
  "Clears the current EShell Buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-install 'use-package))
(require 'use-package)

(setq use-package-always-ensure t)


(use-package emacs
  :hook (emacs-lisp-mode . adam/elisp-setup)
  :config
  (defun adam/elisp-setup ()
    "Custom ELisp Setup."
    (setq-local imenu-generic-expression
                '(("Functions" "^\\s-*(defun\\s-+\\([^[:space:]]+\\)" 1)
                  ("Variables" "^\\s-*(defvar\\s-+\\([^[:space:]]+\\)" 1)
                  ("Macros" "^\\s-*(defmacro\\s-+\\([^[:space:]]+\\)" 1)
                  ("Packages" "^\\s-*(use-package\\s-+\\([^[:space:]]+\\)" 1)))))

;; (use-package perspective
;;   :init
;;   (persp-mode 1)
;;   :config
;;   (setq persp-mode-prefix-key (kbd "C-x x")))

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
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy))))


(use-package ivy-rich
  :after
  ivy
  :init (ivy-rich-mode 1))

(use-package all-the-icons)
(use-package all-the-icons-completion)
(use-package all-the-icons-dired)
(use-package all-the-icons-ibuffer)
(use-package all-the-icons-ivy)
(use-package all-the-icons-nerd-fonts)

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic nil)
  (setq doom-winter-is-coming-no-italics t)
  (setq doom-winter-is-coming-brighter-comments t)
  (setq doom-ir-black-brighter-comments t))

(use-package modus-themes)
(use-package morrowind-theme)

(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 28)
  (setq doom-modeline-enable-word-count t))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

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

(use-package projectile
  :config
  (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/work")
    (setq projectile-project-search-path '("~/work")))
  (setq projectile-switch-project-action #'projectile-dired))

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


(use-package org
  :config
  (setq org-edit-src-content-indentation 0)
  (setq org-link-descriptive t))

(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

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

;; (use-package dap-mode
;;   :config
;;   (add-hook 'dap-stopped-hook (lambda (arg) (call-interactively #'dap-hydra)))
;;   (require 'dap-cpptools)
;;   (with-eval-after-load 'dap-cpptools
;;     (dap-register-debug-template "Rust::Cpptools Run Configuration"
;;                                  (list :type "cppdbg"
;;                                        :request "launch"
;;                                        :name "Rust::Run"
;;                                        :MIMode "gdb"
;;                                        :miDebuggerPath "rust-gdb"
;;                                        :environment []
;;                                        :program "${workspaceFolder}/target/debug/mystery-dungeon"
;;                                        :cwd "${workspaceFolder}"
;;                                        :console "external"
;;                                        :dap-compilation "cargo build"
;;                                        :dap-compilation-dir "${workspaceFolder}")))
;;   (setq dap-default-terminal-kind "integrated"))

(use-package i3wm-config-mode)

(use-package css-mode)

(use-package sly
  :config
  (setq inferior-lisp-program "/bin/sbcl"))

(use-package sly-asdf)
(use-package sly-quicklisp)

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

(use-package gdscript-mode
  :config
  (add-hook 'gdscript-mode-hook 'lsp-mode)
  (setq gdscript-godot-executable "/bin/godot/godot")
  (setq gdscript-use-tab-indents nil)
  (setq gdscript-gdformat-save-and-format nil))

(use-package glsl-mode)
(use-package wgsl-mode)

(use-package hydra)

(use-package evil-nerd-commenter
  :bind
  ("C-;" . evilnc-comment-or-uncomment-lines))

(use-package dired
  :ensure
  nil
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
        '(("gif" . "feh")
          ("jpg" . "feh")
          ("png" . "feh")
          ("mkv" . "mpv")
          ("mp4" . "mpv")
          ("webm" . "mpv")
          ("xcf" . "gimp")
          ("pdf" . "firefox")
          ("kra" . "krita")
          ("blend" . "blender"))))

(use-package dired-hide-dotfiles
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

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

;; (use-package forge)

(use-package python-mode
  :config
  (add-hook 'python-mode-hook 'lsp-mode))

(use-package lua-mode
  ;; :bind
  ;; (:map evil-normal-state-map
  ;;       ("K" . evil-lookup))
  :config
  (add-hook 'lua-mode-hook 'lsp-mode)
  (define-key lua-mode-map (kbd "<normal-state> K") nil))

(use-package fennel-mode
  :config
  (add-hook 'fennel-mode-hook 'lsp-mode))

(use-package js2-mode
  :config
  (add-hook 'js-mode-hook 'js2-minor-mode)
  (add-hook 'js2-mode-hook 'ac-js2-mode)
  (add-hook 'js-mode-hook 'lsp-mode))

(use-package cc-mode
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

  (add-hook 'c-mode-hook 'lsp-mode)
  (add-hook 'c++-mode-hook 'lsp-mode)
  (setq c-default-style "adam"))

;; (use-package emms
;;   :init
;;   (emms-all)
;;   (setq emms-player-list '(emms-player-mpv))
;;   (setq emms-info-functions '(emms-info-native))
;;   :config
;;   (setq-default emms-source-file-default-directory "~/Music/")
;;   (setq-default emms-volume-change-function 'emms-volume-pulse-change))

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

    "s" '(:ignore t :wk "Search")
    "ss" '(swiper :wk "Search Swiper")

    "d" '(:ignore t :wk "Debug")
    "dm" '(dap-hydra :wk "Debug Menu")

    "a" '(:ignore t :wk "AI")
    "aa" '(gptel :wk "AI Start")

    "f" '(:ignore t :wk "Find")
    "fc" '(adam/goto-init-file :wk "Find Config")
    "fh" '(adam/goto-homepage :wk "Find Homepage")
    "fp" '(list-processes :wk "Find Processes")
    "ff" '(adam/fuzzy-find :wk "Find Fuzzy-Contextual")
    "fa" '(lsp-ivy-workspace-symbol :wk "Find LSP Symbol")
    "f." '(adam/find-file :wk "Find File")
    "f," '(projectile-find-file :wk "Find Project File")
    "fz" '(projectile-switch-project :wk "Find Project")
    "fn" '(adam/find-file-new :wk "File File New")

    "gg" '(magit :wk "Magit")

    "b" '(:ignore t :wk "Buffer")
    "bb" '(adam/switch-buffer :wk "Buffer Switch")
    "bm" '(adam/ibuffer :wk "Buffer Menu")
    "bx" '(kill-buffer :wk "Buffer Kill")

    ;; "m" '(:ignore t :wk "Music")
    ;; "mm" '(emms :wk "Music")
    ;; "mp" '(emms-pause :wk "Music Pause")
    ;; "mf" '(emms-add-playlist-file :wk "Music Play")

    "e" '(:ignore t :wk "Eval")
    "ee" '(eval-expression :wk "Eval Expression")
    "eb" '(eval-buffer :wk "Eval Buffer")
    "ex" '(eval-last-sexp :wk "Eval Last Sexpr")

    "h" '(:ignore t :wk "Help")
    "hf" '(counsel-describe-function :wk "Help Function")
    "hv" '(counsel-describe-variable :wk "Help Variable")

    "l" '(:ignore t :wk "LSP")
    "lr" '(lsp-rename :wk "LSP Rename")
    "lf" '(lsp-find-references :wk "LSP Find References")
    "ld" '(flycheck-list-errors :wk "LSP Errors")
    "la" '(lsp-execute-code-action :wk "LSP Code Action")

    "p" '(:ignore t :wk "Perspective")
    "pp" '(persp-switch :wk "Perspective Switch")
    "px" '(persp-kill :wk "Perspective Kill")

    "w" '(:ignore t :wk "Window")
    "w1" '(delete-other-windows :wk "Window Solo")
    "wn" '(evil-window-split :wk "Window Split Horizontal")
    "wv" '(evil-window-vsplit :wk "Window Split Vertical")
    "ww" '(evil-window-next :wk "Window Next")
    "wc" '(evil-window-delete :wk "Window Close")
    "wx" '(kill-buffer-and-window :wk "Window Kill and Close")
    "wh" '(evil-window-left :wk "Window Left")
    "wj" '(evil-window-down :wk "Window Down")
    "wk" '(evil-window-up :wk "Window Up")
    "wl" '(evil-window-right :wk "Window Right")

    "c" '(:ignore t :wk "Command")
    "ce" '(eshell :wk "Command Eshell")
    "cc" '((lambda ()
               (interactive)
               (call-interactively #'compile))
           :wk "Command Current")
    "cp" '((lambda ()
               (interactive)
               (when (projectile-project-p)
                   (call-interactively #'projectile-compile-project)))
               :wk "Command Project")
    "cn" '((lambda ()
               (interactive)
               (setq compile-command "")
               (call-interactively #'compile))
           :wk "Command New")

    "r" '(:ignore t :wk "Reload")
    "rc" '(adam/reload-init-file :wk "Reload Config")

    "q" '(:ignore t :wk "Quick Tool")
    "qq" '(quick-calc :wk "Quick Tool Calculator")))


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

;; (use-package exwm
;;   :config
;;   (add-hook 'exwm-update-class-hook
;;             #'(lambda () (exwm-workspace-rename-buffer exwm-class-name)))

;;   (require 'exwm-randr)
;;   (exwm-randr-enable)

;;   ;; (exwm-systemtray 1)

;;   (setq exwm-input-prefix-keys
;;         '(?\C-x
;;           ?\C-u
;;           ?\C-h
;;           ?\M-x
;;           ?\M-&
;;           ?\M-:))

;;   (setq exwm-input-global-keys
;;         `(([?\s-r] . exwm-reset)
;;           ([?\s-h] . windmove-left)
;;           ([?\s-j] . windmove-down)
;;           ([?\s-k] . windmove-up)
;;           ([?\s-l] . windmove-right)
;;           ([?\s-&] . (lambda (command)
;;                        (interactive (list (read-shell-command "$ ")))
;;                        (start-process-shell-command command nil command)))
;;           ([?\s-w] . exwm-workspace-switch)))

;;   (exwm-enable))
;; (load-theme 'doom-winter-is-coming-dark-blue t)

(load-theme 'modus-vivendi-tinted t)
(load-file custom-file)
(adam/goto-homepage)

(provide 'init)
;;; init.el ends here
